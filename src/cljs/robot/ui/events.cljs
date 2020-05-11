(ns robot.ui.events
  (:require-macros [robot.ui.macros :refer [sfn]]
                   [cljs.core.async.macros :refer [go go-loop]])
  (:require [re-frame.core :as re-frame]
            [robot.ui.db :as db]
            [cljs.pprint :refer [pprint]]
            [cljs.core.async :refer [<! >! timeout]]
            [cljs-http.client :as http]
            [cljs-time.core :as time]
            [cljs-time.format :as t-fmt]
            [cljs-time.coerce :as tc]
            [cljs-hash.goog :as gh]
            [taoensso.sente :as sente :refer (cb-success?)]
            [goog.string :as gstring]
            [goog.string.format]))

(def not-snapshotable [[:designer :ctrl :new-app]
                       [:designer :ctrl :new-inst]])

(defn snapshot-event [db [k :as e]]
  (let [{:keys [curr-idx snapshots] :as history} (:history db {:curr-idx 0 :snapshots (list (dissoc db :history))})]
    (condp = k
      :undo (let [curr-idx (min (- (count snapshots) 3) (inc curr-idx))]
              (.log js/console (str "IDX: " curr-idx))
              (-> db
                  (merge (or (nth snapshots curr-idx nil) db))
                  (assoc-in [:history :curr-idx] curr-idx)))
      :redo (let [curr-idx (max 0 (dec curr-idx))]
              (-> db
                  (merge (or (nth snapshots curr-idx nil) db))
                  (assoc-in [:history :curr-idx] curr-idx)))
      (do
        (-> db
            (assoc
              :history
              {:curr-idx  0
               :snapshots (take 100 (conj (nthrest snapshots curr-idx)
                                          (reduce
                                            (fn [result-db path]
                                              (update-in result-db (butlast path) dissoc (last path)))
                                            (dissoc db :history)
                                            not-snapshotable)
                                          ))}))))))

(defn now []
  (js/goog.date.DateTime.) ;(str (js/Date.)) ;(time/time-now)
  )

(defn create-log [level status msg]
  (let [ts (now)]
    [:log [(t-fmt/unparse (t-fmt/formatter "yyyy-MM-dd HH:mm:ss.SSS") ts) level status msg (pr-str ts)]]))

(re-frame/reg-event-db
  :initialize-db
  (fn [_ [_ url-base]]
    (let [db db/default-db]
      (assoc
        db
        :url-base url-base
        :history {:curr-idx 0 :snapshots (list (dissoc db :history))}))))

(re-frame/reg-event-db
  :reset!
  (fn [db [_ path val :as e]]
    (println "reset! " (pr-str e))
    (let [db (-> db
                 (assoc-in path val)
                 (snapshot-event e))
          _ (println (pr-str db))]
      db)))

(re-frame/reg-event-db
  :flip-set-elem!
  (fn [db [_ path elem]]
    (update-in db path (fn [d-set]
                         (let [d-set (or d-set #{})]
                           (if (d-set elem)
                             (disj d-set elem)
                             (conj d-set elem)))))))

(defmulti ws-event-handler :id :default :default)

(defmethod ws-event-handler :default [evt]
  (pprint evt))

(defmethod ws-event-handler :chsk/handshake [evt]
  (.log js/console "chsk/handshake"))

(defmethod ws-event-handler :chsk/recv [{:keys [id event send-fn]}]
  (let [[_ [verb data]] event]
    ;(.log js/console (str id ":" event))
    (cond
      (= verb :chsk/ws-ping)
      (send-fn [:robot-client/ws-pong])

      (= verb :robot/ws-pong)
      (.log js/console "server says pong!")

      (= verb :robot/update)
      (re-frame/dispatch data))))

(re-frame/reg-event-db
  :robot/state
  (fn [db [_ app inst state]]
    (-> db
        (assoc-in [:applications :ready app inst] state))))

;[:chsk/recv [:robot/update [:robot/state "prueba3" "tres" {:robot/current :prueba}]]]


(re-frame/reg-event-db
  :login
  (fn [{:keys [url-base] :as db} [_ {:keys [uid pass] :as e}]]
    (go
      (.log js/console (str "LOGIN: " (pr-str e)))
      (let [{:keys [status body]} (<! (http/post
                                        (str url-base "login")
                                        {:body    {:uid uid :pass pass}
                                         :headers {"content-type" "application/edn"
                                                   "Accept"       "application/edn"}}))]
        (.log js/console "load-applications status:" status)
        ;(pprint body)
        (if (#{204 200} status)
          (do
            (if (= 200 status)
              (re-frame/dispatch-sync (create-log :info status (str uid " logged in as admin")))
              (re-frame/dispatch-sync (create-log :info status (str uid " logged in"))))
            (re-frame/dispatch-sync [:load-applications])
            (re-frame/dispatch-sync [:load-operations])
            (re-frame/dispatch-sync [:start-server-comm {:uid uid :admin (= status 200)}]))
          (re-frame/dispatch (create-log (if (= status 0) :error :warn) (if (= 0 status) 500 status) "login-failed")))))
    (-> db
        (update :control dissoc :uid))))

(re-frame/reg-event-db
  :logout
  (fn [db _]
    ;auth2.signOut().then(function () {
    ;                                  console.log('User signed out.');
    ;                                  });
    ;gapi.auth2.getAuthInstance.signOut()
    (let []
      (js/GOOGlogout)
      (re-frame/dispatch (create-log :info "bye" (str (get-in db [:contro :uid]) " logged out")))
      (update db :control dissoc :uid :admin))))

(re-frame/reg-event-db
  :load-applications
  (fn [{:keys [url-base] :as db} _]
    (.log js/console "Loading applications 1")
    (go
      (.log js/console "Loading applications 2")
      (let [{:keys [status body]} (<! (http/get
                                        (str url-base "applications")
                                        {;query-params {}
                                         :headers {"Accept" "application/edn"}}))]
        (.log js/console "load-applications status:" status)
        ;(pprint body)
        (if (#{200} status)
          (do
            (re-frame/dispatch [[:applications] body]))
          (re-frame/dispatch [:status body]))))
    db))

(re-frame/reg-event-db
  :load-operations
  (fn [{:keys [url-base] :as db} _]
    (.log js/console "OPERS: Loading operations 1")
    (go
      (let [
            {:keys [status body]} (<! (http/get
                                        (str url-base "operations")
                                        {;query-params {}
                                         :headers {"Accept" "application/edn"}}))]
        (if (#{200} status)
          (do
            (re-frame/dispatch [[:operations] body]))
          (re-frame/dispatch (create-log :error status body)))))
    db))

(re-frame/reg-event-db
  [:applications]
  (fn [{:keys [applications] :as db} [path new-applications :as e]]
    (-> db
        (update-in path merge new-applications)
        (snapshot-event e))))

(re-frame/reg-event-db
  [:operations]
  (fn [{:keys [operations] :as db} [path new-operations]]
    (assoc-in db path new-operations)))

(re-frame/reg-event-db
  :save-app
  (fn [{:keys [url-base] :as db} [_ app-id]]
    (println "Para guardar!!: ")
    (println (get-in db [:applications :editable app-id]))
    (go
      (let [{:keys [status body]} (<! (http/post
                                        (str url-base "store/" app-id)
                                        {;query-params {}
                                         :headers {"content-type" "application/edn"
                                                   "Accept"       "application/edn"}
                                         :body    (get-in db [:applications :editable app-id])}))]
        (if (#{200 204} status)
          (do
            (re-frame/dispatch [:instantiate app-id])
            ;(re-frame/dispatch [:pprint])
            )
          (re-frame/dispatch (create-log :error status body)))
        ))
    (js/removeListener)
    db))

(re-frame/reg-event-db
  :instantiate
  (fn [{:keys [url-base] :as db} [_ app-id]]
    (go
      (.log js/console "Loading application:" app-id)
      (let [{:keys [status body]} (<! (http/post
                                        (str url-base "instantiate/" app-id)
                                        {;query-params {}
                                         :headers {"Accept" "application/edn"}}))]
        (.log js/console "load-application status:" status)
        ;(pprint body)
        (if (#{204} status)
          (re-frame/dispatch [[:applications :editable] app-id body])
          (re-frame/dispatch (create-log :error status body)))))
    db))


(re-frame/reg-event-db
  :remove-app
  (fn [{:keys [url-base] :as db} [_ app-id]]
    (println "Para eliminar!!: " app-id)
    (go
      (let [{:keys [status body]} (<! (http/post
                                       (str url-base "remove/" app-id)
                                       {:headers {"content-type" "application/edn"
                                                  "Accept"       "application/edn"}}))]
        (if (#{200 204} status)
          (do
            (re-frame/dispatch-sync (create-log :info status(str "Removed app " app-id)))
            (re-frame/dispatch-sync [:load-applications])
            (re-frame/dispatch [:reset! [:designer :ctrl :app] nil])
            )
          (re-frame/dispatch (create-log :error status body)))
        ))
    (js/removeListener)
    db))

(re-frame/reg-event-db
  :remove-inst
  (fn [{:keys [url-base] :as db} [_ app-id inst-id]]
    (println "Para eliminar!!: " app-id inst-id)
    (go
      (let [{:keys [status body]} (<! (http/post
                                       (str url-base "remove/" app-id "/" inst-id)
                                       {:headers {"content-type" "application/edn"
                                                  "Accept"       "application/edn"}}))]
        (if (#{200 204} status)
          (do
            ;(re-frame/dispatch [:instantiate app-id])
            ;(re-frame/dispatch [:pprint])
            (re-frame/dispatch-sync (create-log :info status(str "Removed app instance " (str app-id "/" inst-id))))
            (re-frame/dispatch-sync [:load-applications])
            )
          (re-frame/dispatch (create-log :error status body)))
        ))
    db))

(re-frame/reg-event-db
  :start-server-comm
  (fn [db [_ {:keys [uid admin]} :as e]]
    (let [{:keys [chsk ch-recv send-fn state] :as comms}
          (sente/make-channel-socket-client! "robot-client" ; url-base  Note the same path as before
                                             {:type :auto   ; e/o #{:auto :ajax :ws}
                                              })]
      (sente/start-client-chsk-router! ch-recv ws-event-handler)
      (println ">>>>>> " uid admin)
      (-> db
          (assoc :communication comms)
          (assoc-in [:control :uid] uid)
          (assoc-in [:control :admin ] admin)
          (snapshot-event e)))))


(re-frame/reg-event-db
  :start
  (fn [{:keys [url-base] :as db} [_ app inst]]
    (go
      (let [{:keys [status body]} (<! (http/post
                                        (str url-base "start/" app "/" inst)
                                        {:headers {"content-type" "application/edn"
                                                   "Accept"       "application/edn"}}))]
        (.log js/console "load-applications status:" status)
        ;(pprint body)
        (if (#{204} status)
          (re-frame/dispatch (create-log :info 200 (str app "/" inst " started!")))
          (re-frame/dispatch (create-log (if (= status 0) :error :warn) (if (= 0 status) 500 status) "start-failed")))
        ;(re-frame/dispatch [:load-applications])
        ))
    (-> db
        (update-in [:applications :ready app inst] assoc :robot/status :transitioning)
        (update-in [:applications :editable app :instances inst] assoc :robot/status :transitioning))))

(re-frame/reg-event-db
  :stop
  (fn [{:keys [url-base] :as db} [_ app inst]]
    (go
      (let [{:keys [status body]} (<! (http/post
                                        (str url-base "stop/" app "/" inst)
                                        {:headers {"content-type" "application/edn"
                                                   "Accept"       "application/edn"}}))]
        (.log js/console "load-applications status:" status)
        ;(pprint body)
        (if (#{204} status)
          (re-frame/dispatch (create-log :info 200 (str app "/" inst " stopped!")))
          (re-frame/dispatch (create-log (if (= status 0) :error :warn) (if (= 0 status) 500 status) "stop-failed")))
        ;(re-frame/dispatch [:load-applications])
        ))
    (-> db
        (update-in [:applications :ready app inst] assoc :robot/status :transitioning)
        (update-in [:applications :editable app :instances inst] assoc :robot/status :transitioning))))

(re-frame/reg-event-db
  :pprint
  (fn [db _]
    (pprint db)
    db))

(re-frame/reg-event-db
  :log
  (fn [db [_ err-evt]]
    ;(pprint db)
    (.log js/console (str "err-evt: " err-evt))
    (.log js/console (pr-str (:log (update db :log conj err-evt))))
    (update db :log (fn [log]
                      (take 1000 (conj log err-evt))))))

(re-frame/reg-event-db
  :add-param
  (fn [db [_ path {:keys [k v]} :as e]]
    (let [path (concat [:applications :editable] path [(keyword k)])]
      (js/registerListener)
      (-> db
          (assoc-in path v)
          (snapshot-event e)
          ))))

(re-frame/reg-event-db
  :rm-param
  (fn [db [_ path k :as e]]
    (let [path (concat [:applications :editable] path)]
      (js/registerListener)
      (-> db
          (update-in path dissoc (keyword k))
          (snapshot-event e)
          ))))

(re-frame/reg-event-db
  :add-user
  (fn [db [_ {:keys [email pass hpass admin]} :as e]]
    (let [hpass (if (and (not (seq pass)) hpass) hpass (gh/sha1-hex pass))]
      (.log js/console (str "SHA1: " (pr-str [email hpass pass])))
      (-> db
          (update-in [:users] assoc email {:hpass hpass :admin admin})
          (snapshot-event e)))))

(re-frame/reg-event-db
  :rm-user
  (fn [db [_ user :as e]]
    (-> db
        (update-in [:users] dissoc user)
        (snapshot-event e))))

(re-frame/reg-event-db
  :load-users
  (fn [{:keys [url-base] :as db} [_]]
    (go
      (let [{:keys [status body]} (<! (http/get
                                        (str url-base "users")
                                        {:headers {"Accept" "application/edn"}}))]
        (if (#{200} status)
          (re-frame/dispatch [:reset! [:users] body])
          (re-frame/dispatch (create-log :info 200 (str "Users loaded!"))))))
    db))

(re-frame/reg-event-db
  :save-users
  (fn [{:keys [url-base] :as db} [_]]
    (go
      (let [{:keys [status body]} (<! (http/put
                                        (str url-base "users")
                                        {:headers {"content-type" "application/edn"
                                                   "Accept"       "application/edn"}
                                         :body    (get-in db [:users])}))]
        (if (#{204} status)
          (re-frame/dispatch (create-log :info 200 (str "Users saved!"))))))
    db))

(def set-conj (fnil conj #{}))

(re-frame/reg-event-db
  :undo
  (sfn [db e]
       db))

(re-frame/reg-event-db
  :redo
  (sfn [db e]
       db))


(re-frame/reg-event-db
  :import-app-dlg
  (fn [db [_]]
    (-> db
        (assoc-in
          [:designer :ctrl :import] true)

        )))


(re-frame/reg-event-db
  :import
  (fn [db [_ {:keys [file app-name]}]]
    (js/registerListener)
    (go
      (let [{:keys [status body]} (<! (http/post
                                        (str (:url-base db) "store/" app-name)
                                        {
                                         :headers {"content-type" "application/edn"
                                                   "Accept"       "application/edn"}
                                         :body    file}))]
        (if (#{204} status)
          (do
            (re-frame/dispatch [:instantiate app-name])
            (re-frame/dispatch (create-log :info 200 (str "App saved!")))
            (re-frame/dispatch [:reset! [:designer :ctrl :import] nil]))
          (re-frame/dispatch (create-log :error status body))
          )))
    db))

(re-frame/reg-event-db
  :change-opr-type
  (fn [db [_ app-id opr-id opr-type]]
    (update-in db [:applications :editable app-id :states opr-id] assoc :operation opr-type)))

(re-frame/reg-event-db
 :move-robot
 (fn [db [_ id x0 y0 x1 y1 delta]] ;id,x0,y0,x1,y1,delta
   (js/SVGAnimate id x0 y0 x1 y1 delta)
   (println "YA")
   db))

;(re-frame/reg-event-db
;  :change-opr-id
;  (fn [db [_ app-id opr-id opr-new-id]]
;    (let [oprs (get-in db [:applications :editable app-id :states])
;          oprs-renamed (clojure.set/rename-keys oprs {opr-id opr-new-id})
;          oprs-id (keys oprs)
;          replaced (doall
;                     (map (fn [key]
;                            (let [links (get-in db [:applications :editable app-id :states key :flow])]
;                              (doall
;                                (map (fn [[k _]]
;                                       (if (= k opr-id)
;                                         (println "Hay que reemplazar " k " con " opr-new-id)))
;                                     links))))
;                          oprs-id))]
;      (println "replaced " replaced)
;      (doall
;        (map (fn [key]
;               (let [links (get-in db [:applications :editable app-id :states key :flow])]
;                 (doall
;                   (map (fn [[k _]]
;                          (if (= k opr-id)
;                            (update-in db (get-in db [:applications :editable app-id :states key :flow]))
;                            ))
;                        links))))
;             oprs-id))
;
;      db
;      ;(update-in db [:applications :editable app-id :states] dissoc opr-id)
;      ;(update-in db [:applications :editable app-id] assoc :states oprs-renamed)
;      )))
