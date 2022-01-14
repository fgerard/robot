(ns robot.core.essentials
  (:require
    [clojure.core.async :refer [go-loop put! <! chan] :as async]
    [clojure.pprint :as pp]
    [clojure.tools.logging :as log]
    ;[clojure.data :refer [diff]]
    [clojure.walk :as walk]
    [integrant.core :as ig]
    ;[manifold.stream :as s]
    [taoensso.sente :as sente]
    [robot.core.util :as U]
    [keyval.konservedb :as db])
  (:import
    (java.io File PrintWriter OutputStreamWriter FileOutputStream)))

(defmethod ig/init-key :robot.core.essentials/operations
  [_ opers]
  (reduce (fn opers-reducer [result [k [factory ui]]]
            (assoc result k [factory ui]))
          {}
          opers))

(defmethod ig/init-key :robot.core.essentials/flow-factory
  [_ {:keys [delay] :or {delay 1000}}]
  (fn flow-factory [arrows]
    (fn flow [context you]
      (let [current-val (context you)
            next-state (ffirst
                         (filter
                           (fn [[next re]]
                             (let [re (and re (U/contextualize context re))]
                               (when (or (not re) (re-find (re-pattern re) (name (str current-val))))
                                 next)))
                           arrows))]
        (if next-state
          (do
            (log/debug "next-state: " next-state)
            (Thread/sleep delay)
            (assoc context :robot/current next-state :robot/previous you))
          (do
            (log/info "No next-state stopping!")
            (assoc context :robot/status :stopped)))))))

(defmethod ig/init-key :robot.core.essentials/state-factory
  [_ _]
  (fn state-factory [operation flow]
    (fn state [context you]
      (let [t0 (System/currentTimeMillis)
            opr-result (-> context
                           (operation you)
                           (assoc :robot/last-delta (- (System/currentTimeMillis) t0)))
            ;_ (log/debug "opr-result: " opr-result "  you:" you)
            ]
        (flow opr-result you)))))

(defmulti app-cmd
          (fn [_ _ _ _ [cmd _]] ;db state-factory flow-factory operations
            cmd)
          :default :default)

(defmethod app-cmd :default [_ _ _ _ [cmd _]] ;db state-factory flow-factory operations
  (let [msg (str "Command: " cmd " unknown")]
    (log/error msg)
    [:error {:err-msg msg}]))

(defn rotate-file-ver [f-name]
  (dorun
    (for [idx (reverse (range 0 9))]
      (let [older (File. (str f-name "." (inc idx)))
            newer (File.  ^String (if (> idx 0) (str f-name "." idx) f-name))]
        (.delete older)
        (.renameTo newer older)))))

(defn stringify-map [m]
  (reduce-kv
    (fn [result k v]
      (assoc result k (str v)))
    {}
    m))

  #_(defn stringify-map-of-maps [m]
      (reduce-kv
       (fn [result k map-v]
         (assoc k (stringify-map map-v)))
       {}
       m))
  

(defn remove-robot-keys [m]
  (into {} (remove
             (fn [[k v]]
               (= "robot" (namespace k)))
             m)))

(defn stringify-conf [app-conf]
  (let [path (atom [])]
    (letfn [(optional-stringify-map
              [path elem]
              (if (and (map? elem)
                       (or (and (= (first path) :app-params))
                           (and (= (first path) :instances) (= 2 (count path)))
                           (and (= (first path) :states) (= :conf (last path)))))
                (-> elem
                    stringify-map
                    remove-robot-keys)
                elem))
            (path-ctrl-fn
              [path elem]
              (let [path (if (instance? clojure.lang.MapEntry elem) (conj path (key elem)) path)]
                (walk/walk (partial path-ctrl-fn path) (partial optional-stringify-map path) elem)))]
      (walk/walk (partial path-ctrl-fn []) (partial optional-stringify-map []) app-conf))))

(defmethod app-cmd :store [db state-factory flow-factory operations [cmd {:keys [app-id app-conf]}]]
  (try
    (log/info :store (pr-str [cmd app-id]))
    (let [f-name (format "app-log/%s.edn" app-id)
          app-conf (stringify-conf app-conf)]
      (rotate-file-ver f-name)
      (with-open [ostream (PrintWriter.
                            (OutputStreamWriter.
                              (FileOutputStream. f-name) "UTF-8"))]
        (pp/pprint app-conf ostream))
      (db/put db [:apps app-id] app-conf)
      [:success])
    (catch Exception e
      (.printStackTrace e)
      [:error {:err-msg (format "%s -> %s" (str (class e)) (.getMessage e))}])))

(defmethod app-cmd :load [db state-factory flow-factory operations [cmd {:keys [app-id]}]]
  (try
    (log/info :load (pr-str [cmd app-id]))
    (let [app-conf (db/get db [:apps app-id])]
      [:success {:app-id app-id :app-conf app-conf}])
    (catch Exception e
      (.printStackTrace e)
      [:error {:err-msg (format "%s -> %s" (str (class e)) (.getMessage e))}])))

(defmethod app-cmd :remove [db state-factory flow-factory operations [cmd {:keys [app-id inst-id]}]]
  (if inst-id
    (try
      (log/info :remove-inst (pr-str [cmd app-id inst-id]))
      (let [app-conf (-> (db/get db [:apps app-id])
                         (update :instances dissoc inst-id))]
        (if (= [:success]
               (app-cmd db state-factory flow-factory operations [:store {:app-id app-id :app-conf app-conf}]))
          [:success {:app-id app-id :app-conf app-conf}]))
      (catch Exception e
        (.printStackTrace e)
        [:error {:err-msg (format "%s -> %s" (str (class e)) (.getMessage e))}]))
    (try
      (log/info :remove (pr-str [cmd app-id]))
      (db/delete db [:apps app-id])
      [:success {:app-id app-id}]
      (catch Exception e
        (.printStackTrace e)
        [:error {:err-msg (format "%s -> %s" (str (class e)) (.getMessage e))}]))))

(defmethod app-cmd :start [db state-factory flow-factory operations [cmd {:keys [app-id inst-id]}]]
  (try
    (log/info :start (pr-str [cmd app-id inst-id]))
    (db/put db [:running-info app-id inst-id] {:status :running})
    [:success {:app-id app-id :inst-id inst-id}]
    (catch Exception e
      (.printStackTrace e)
      [:error {:err-msg (format "%s -> %s" (str (class e)) (.getMessage e))}])))

(defmethod app-cmd :stop [db state-factory flow-factory operations [cmd {:keys [app-id inst-id]}]]
  (try
    (log/info :stop (pr-str [cmd app-id inst-id]))
    (db/put db [:running-info app-id inst-id] {:status :stopped})
    [:success {:app-id app-id :inst-id inst-id}]
    (catch Exception e
      (.printStackTrace e)
      [:error {:err-msg (format "%s -> %s" (str (class e)) (.getMessage e))}])))


(defn instance-controller-factory [state-factory flow-factory operations app-id
                                   app-params init-state states
                                   result instance instance-params]
  (let [instance-params (merge app-params instance-params {:robot/app      app-id
                                                           :robot/instance instance
                                                           :robot/current  init-state
                                                           :robot/status   :stopped
                                                           :robot/mood     {}})]
    (assoc result instance {:init-ctx instance-params})))

(defn instantiate-app [state-factory flow-factory operations app-id
                       {:keys [app-params instances init-state states]}]
  (let [instances (reduce-kv
                    (partial instance-controller-factory state-factory flow-factory operations
                             app-id app-params init-state states)
                    (sorted-map)
                    instances)
        states (reduce-kv
                 (fn [result state-id {:keys [operation conf center flow]}]
                   (let [[operation-factory ui] (operations operation)]
                     (assoc result state-id (state-factory
                                              (operation-factory conf)
                                              (flow-factory flow)))))
                 (sorted-map)
                 states)]
    {:instances instances :states states}))

(defmethod app-cmd :instantiate [db state-factory flow-factory operations [cmd {:keys [app-id app-conf]}]]
  (try
    (if app-conf
      (let [app (instantiate-app state-factory flow-factory operations app-id app-conf)]
        [:success {:instantiated-app app}])
      [:error {:err-msg (format "Missing configuration for app: %s" app-id)}])
    (catch Exception e
      (.printStackTrace e)
      [:error {:err-msg (format "%s -> %s" (str (class e)) (.getMessage e))}])))

(defn trunc-str [s max-length]
  (let [len (count s)]
    (if (> len max-length)
      (subs s 0 max-length)
      s)))

(comment

  (defmethod app-cmd :stats [db state-factory flow-factory operations [cmd {:keys [app-id inst-id opr-id result delta]}]]
    (let [now (System/currentTimeMillis)]
      (try
        (let [result (if (string? result)
                       (trunc-str result 512)
                       result)
              stats (db/get db [:stats app-id inst-id] {})
              opr-stats (get stats opr-id)
              opr-stats (cons [now delta result] (take 1000 opr-stats))
              stats (assoc stats opr-id opr-stats)]
          (db/put db [:stats app-id inst-id] stats)
          (println "STATS: " [app-id inst-id (count (str stats))])
          (pp/pprint stats)
          [:success {:stats "stored"}])
        (catch Exception e
          (.printStackTrace e)
          (:error {:err-msg (format "%s -> %s" (str (class e)) (.getMessage e))})))))

  )

(defmethod ig/init-key :robot.core.essentials/app-controller
  [_ {:keys [operations state-factory flow-factory db]}]
  (let [app-cmd-controller (partial app-cmd db state-factory flow-factory operations)]
    (fn app-controller [cmd info]
      (app-cmd-controller [cmd info]))))


(defn change-executor-status [publisher status {:keys [states ctx] :as inst-agent}]
  (let [current-status (get-in inst-agent [:ctx :robot/status])
        app-id (:robot/app ctx)
        inst-id (:robot/instance ctx)
        new-ctx (assoc ctx :robot/status status)]
    (put! publisher [:robot/state app-id inst-id new-ctx])
    (assoc-in inst-agent [:ctx] new-ctx)))

(defn mutate-state [publisher {:keys [states ctx] :as inst-agent}]
  (let [mood (:robot/mood ctx {})
        current (:robot/current ctx)
        app-id (:robot/app ctx)
        inst-id (:robot/instance ctx)
        opr (states current)
        ;new-ctx (opr ctx current)
        status (:robot/status ctx)
        ]
    ;(put! publisher [:robot/state app-id inst-id (second (diff ctx new-ctx))])
    (if (= status :running)
      (let [;_ (log/debug ["ANTES: " current (pr-str ctx)])
            new-ctx (opr ctx current)
            delta (:robot/last-delta new-ctx 0)
            deltas (-> (:robot/deltas new-ctx {}) (assoc current delta))
            new-ctx (assoc new-ctx :robot/deltas deltas)
            ;_ (log/debug "DESPUES: " (pr-str new-ctx))
            status (:robot/status new-ctx)
            new-inst-agent (assoc-in inst-agent [:ctx] new-ctx)
            ]
        (log/debug (str "robot-stats:" (pr-str {:app-id app-id :inst-id inst-id :opr-id current :delta delta :result (get ctx current)})))
        (put! publisher [:robot/state app-id inst-id new-ctx])
        (send *agent* (partial mutate-state publisher))
        new-inst-agent)
      inst-agent)))

(defmulti robot-cmd (fn [_ _ _ [cmd & _]]
                      cmd))

(defmethod robot-cmd :instantiate [robot app-controller operations [cmd {:keys [app-id]}]]
  (log/info :instantiate (pr-str [cmd app-id]))
  (let [[status {:keys [err-msg instantiated-app]}] (app-controller :instantiate {:app-id app-id :app-conf (get-in robot [:apps app-id])})]
    (if (= :success status)
      (-> robot
          (assoc-in [:ready-apps app-id] instantiated-app)
          (assoc :cmd-status :success)
          (dissoc :cmd-err))
      (-> robot
          (assoc :cmd-status status)
          (assoc :cmd-err err-msg)))))

(defmethod robot-cmd :load [robot app-controller operations [cmd {:keys [app-id]}]]
  (log/info :load (pr-str [cmd app-id]))
  (let [[status {:keys [app-id err-msg app-conf]}] (app-controller :load {:app-id app-id})]
    (if (= :success status)
      (let [new-robot (-> robot
                          (assoc-in [:apps app-id] app-conf)
                          (assoc :cmd-status :success)
                          (dissoc :cmd-err)
                          ;(robot-cmd app-controller operations [:instantiate {:app-id app-id}])
                          )]
        (if (not= :success (:cmd-status new-robot))
          (-> new-robot
              (update new-robot :apps dissoc app-id))
          new-robot))
      (-> robot
          (assoc :cmd-status status)
          (assoc :cmd-err err-msg)))))

(defmethod robot-cmd :store [robot app-controller operations [cmd {:keys [app-id app-conf]}]]
  (log/info :store (pr-str [cmd app-id]))
  (let [[status {:keys [err-msg]}] (app-controller :store {:app-id app-id :app-conf app-conf})]
    (if (= :success status)
      (-> robot
          (assoc-in [:apps app-id] app-conf)
          (assoc :cmd-status :success)
          (dissoc :cmd-err))
      (-> robot
          (assoc :cmd-status status)
          (assoc :cmd-err err-msg)))))

(defn stop-all [instances publisher]
  (dorun
    (map (fn [[inst-name {:keys [inst-agent]}]]
           (if inst-agent
             (send inst-agent (partial change-executor-status publisher :stopped)))) instances)))

(defmethod robot-cmd :remove [robot app-controller operations [cmd {:keys [app-id inst-id] :as params}]]
  (log/info :remove (pr-str params))
  (if inst-id
    (let [robot (robot-cmd robot app-controller operations [:stop params])
          [status {:keys [app-id app-conf]}] (app-controller :remove params)
          new-robot (-> robot
                        (assoc-in [:apps app-id] app-conf)
                        (assoc :cmd-status :success)
                        (dissoc :cmd-err))]
      new-robot)
    (if-let [app (get-in robot [:ready-apps app-id])]
      (let [instances (:instances app)
            publisher (:publish robot)]
        (stop-all instances publisher)
        (app-controller :remove params)
        (-> robot
            (update :ready-apps dissoc app-id)
            (update :apps dissoc app-id)
            (assoc :cmd-status :success)
            (dissoc :cmd-err)))
      (-> robot
          (assoc :cmd-status :error :cmd-err (format "Application: %s not found" app-id))))))

(defn robot-instance-error-handler [app-id inst-id ex]
  (log/error "robot error@" [app-id inst-id])
  (log/error ex))

(defn create-ctx-agte [app-id inst-id init-ctx]
  (let [agte (agent init-ctx
                    :error-handler (partial robot-instance-error-handler app-id inst-id)
                    :error-mode :continue)]
    agte))

(defmethod robot-cmd :start [robot app-controller operations [cmd {:keys [app-id inst-id]}]]
  (log/info :start (pr-str [cmd app-id]))
  (if-let [init-ctx (get-in robot [:ready-apps app-id :instances inst-id :init-ctx])]
    (let [publisher (:publish robot)
          states (get-in robot [:ready-apps app-id :states])
          inst-agent (get-in robot [:ready-apps app-id :instances inst-id :inst-agent] (create-ctx-agte
                                                                                         app-id inst-id
                                                                                         {:ctx    (into {} init-ctx)
                                                                                          :states states}))
          current-status (get-in @inst-agent [:ctx :robot/status])]
      (when (not= :running current-status)
        (send inst-agent (comp
                           (partial mutate-state publisher)
                           (partial change-executor-status publisher :running)))
        (app-controller :start {:app-id app-id :inst-id inst-id}))
      (-> robot
          (assoc :cmd-status :success)
          (dissoc :cmd-err)
          (assoc-in [:ready-apps app-id :instances inst-id :inst-agent] inst-agent)))
    (-> robot
        (assoc :cmd-status :error :cmd-err (format "Application: %s/%s not found" app-id inst-id)))))

(defmethod robot-cmd :stop [robot app-controller operations [cmd {:keys [app-id inst-id]}]]
  (log/info :stop (pr-str [cmd app-id]))
  (if-let [inst-agent (get-in robot [:ready-apps app-id :instances inst-id :inst-agent])]
    (let [publisher (:publish robot)]
      (send inst-agent (partial change-executor-status publisher :stopped))
      (app-controller :stop {:app-id app-id :inst-id inst-id})
      (-> robot
          (assoc :cmd-status :success)
          (dissoc :cmd-err)))
    (-> robot
        (assoc :cmd-status :error :cmd-err (format "Application: %s/%s not found" app-id inst-id)))))

(defn normalize-value [value]
  (cond (string? value) value
        (keyword? value) value
        (map? value) value
        (number? value) value
        :else (str value)))

(defn start-listening [robot {:keys [ch-recv send-fn connected-uids]}]
  (let [publisher (get @robot :publish)]
    (go-loop []
      (let [[action app-id inst-id state :as data] (<! publisher)
            state (reduce-kv (fn [m k v] (assoc m k (normalize-value v))) {} state)]
        ;(log/debug "got update from robot: " (subs 0 (min 1000 (count data2-print)) data2-print))
        (if-let [uids (seq (:any @connected-uids))]
          (future
            (doseq [uid uids]
              (log/debug "MANDANDO INFORMACION A:" (pr-str {:uid uid :app-id app-id :inst-id inst-id :state state}))
              (send-fn uid [:robot/update [action app-id inst-id state]]))))
        (recur)))))

(defn connected-manager [_ _ old new]
  (when (not= old new)
    (log/info "Connected changed: " new)))

(defmulti ws-event-handler (fn [_ {:keys [id]}] id) :default :default)

(defmethod ws-event-handler :default [robot event]
  (log/debug "Evento indefinido: " (:id event)))

(defmethod ws-event-handler :chsk/ws-ping [robot {:keys [id event uid client-id send-fn]}]
  (log/debug "Cliente dice ping! ")
  (send-fn uid [:robot/ws-pong]))


(defmethod ig/init-key :robot.core.essentials/robot-controller
  [_ {:keys [operations app-controller manager]}]
  (let [robot (atom {:publish (chan)})
        event-handler (partial ws-event-handler robot)]
    (sente/start-server-chsk-router! (:ch-recv manager) event-handler)
    (start-listening robot manager)
    (fn robot-controler
      ([] @robot)
      ([[cmd payload :as params]]
       (swap! robot robot-cmd app-controller operations params)))))

(defmethod ig/init-key :robot.core.essentials/robot-info
  [_ {:keys [robot-controller db]}]
  (letfn [(get-stored-apps
            []
            (into [] (db/get-apps db)))
          (get-loaded-apps
            [robot-controller]
            (let [robot-data (robot-controller)]
              (into [] (sort (keys (:apps robot-data))))))
          (get-ready-apps
            [robot-controller]
            (let [robot-data (robot-controller)]
              (reduce
                (fn robot-info-reducer [result [app-id running-info]]
                  (let [instances-info (reduce
                                         (fn robot-inst-reducer [instances [inst-id {:keys [inst-agent] :as cosa}]]
                                           (let [inst-status (and inst-agent (:ctx @inst-agent))]
                                             (assoc instances inst-id inst-status)))
                                         (sorted-map)
                                         (:instances running-info))]
                    (assoc result app-id instances-info)))
                (sorted-map)
                (:ready-apps robot-data))))
          (get-editable-apps
            [robot-controller]
            (let [robot-data (robot-controller)]
              (:apps robot-data)))

          (robot-info-fn
            []
            (let [stored-apps (get-stored-apps)
                  loaded-apps (get-loaded-apps robot-controller)
                  ready-apps (get-ready-apps robot-controller)
                  editable-apps (get-editable-apps robot-controller)]
              (log/debug :stored-apps (pr-str stored-apps))
              {:stored   stored-apps
               :loaded   loaded-apps
               :ready    ready-apps
               :editable editable-apps
               }))]
    (let [stored-apps (get-stored-apps)
          _ (log/info (str "loading apps: " (pr-str stored-apps)))
          _ (doseq [app-id stored-apps]
              (robot-controller [:load {:app-id app-id}]))
          loaded-apps (get-loaded-apps robot-controller)
          _ (log/info (str "instantiate apps: " (pr-str loaded-apps)))
          _ (doseq [app-id loaded-apps]
              (robot-controller [:instantiate {:app-id app-id}]))
          ready-apps (get-ready-apps robot-controller)
          _ (log/info (str "Starting instances that where running.."))
          _ (log/info (keys ready-apps))
          _ (doseq [[app instances] ready-apps]
              (doseq [[inst _] instances]
                (log/info "verifying if " app " / " inst " was running")
                (let [running-info (db/get db [:running-info app inst])]
                  (if (and running-info (= :running (:status running-info)))
                    (do
                      (log/info "Starting " app " / " inst)
                      (robot-controller [:start {:app-id app :inst-id inst}]))
                    (log/info "Not started!")))))]
      (log/info :robot-ready)
      robot-info-fn)))


(defmethod ig/init-key :robot.core.essentials/app-info
  [_ {:keys [robot-controller db]}]
  (letfn [(get-editable-apps
            [robot-controller]
            (let [robot-data (robot-controller)]
              (:apps robot-data)))

          (robot-info-fn
            []
            (let [editable-apps (get-editable-apps robot-controller)]
              editable-apps
              ))]
    robot-info-fn))
