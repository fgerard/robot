(ns robot2.events.api
  "Toda la comunicacion con el servidor (HTTP + sente) vive aqui, y SOLO aqui,
   como un par de efectos declarativos (:http y :ws-send/:ws-connect)
   registrados una sola vez con reg-fx.

   En la version 1, cada handler de evento metia un (go (<! (http/...))) a
   mano dentro de un reg-event-db -- en teoria una funcion pura db->db que en
   realidad disparaba peticiones de red como side-effect. Aqui los handlers
   de evento (events/core.cljs en adelante, y cada namespace de dominio) usan
   reg-event-fx y regresan datos: {:db ... :http {...}}. Quien ejecuta el
   I/O real es el efecto, no el handler."
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [re-frame.core :as re-frame]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [taoensso.sente :as sente]
            [robot2.interop :as interop]
            [robot2.events.core :refer [create-log]]))

;; --- efecto :http ------------------------------------------------------------
;; data: {:method :get/:post/:put :url ... :headers {...} :body ...
;;        :on-success [event-vec] :on-failure [event-vec]}
;; on-success/on-failure son vectores de evento a los que se les agrega
;; [status body] al final antes de dispatch -> siguen siendo datos puros,
;; inspeccionables en re-frame-10x.
(re-frame/reg-fx
  :http
  (fn [{:keys [method url headers body on-success on-failure]}]
    (go
      (let [resp (<! (case method
                       :get (http/get url {:headers headers})
                       :post (http/post url {:headers (or headers {}) :body body})
                       :put (http/put url {:headers (or headers {}) :body body})))
            {:keys [status body]} resp
            ok? (#{200 204} status)
            target (if ok? on-success on-failure)]
        (when target
          (re-frame/dispatch (into target [status body])))))))

;; --- efecto :ws-connect / :ws-send -------------------------------------------
(defmulti ws-event-handler :id :default :default)

(defmethod ws-event-handler :default [evt]
  (.warn js/console "ws-event-handler sin manejar:" (pr-str evt)))

(defmethod ws-event-handler :chsk/handshake [_]
  (.log js/console "chsk/handshake"))

(defmethod ws-event-handler :chsk/recv [{:keys [event send-fn]}]
  (let [[_ [verb data]] event]
    (cond
      (= verb :chsk/ws-ping) (send-fn [:robot-client/ws-pong])
      (= verb :robot/update) (re-frame/dispatch data)
      :else nil)))

(re-frame/reg-fx
  :ws-connect
  (fn [_]
    (let [{:keys [ch-recv] :as comms} (sente/make-channel-socket-client! "robot-client" {:type :auto})]
      (sente/start-client-chsk-router! ch-recv ws-event-handler)
      (re-frame/dispatch [:api/ws-connected comms]))))

(re-frame/reg-event-db
  :api/ws-connected
  (fn [db [_ comms]]
    (assoc db :communication comms)))

;; --- sesion -------------------------------------------------------------------
(re-frame/reg-event-fx
  :api/login
  (fn [{:keys [db]} [_ {:keys [uid pass]}]]
    {:db (update db :control dissoc :uid)
     :http {:method :post
            :url (str (:url-base db) "login")
            :headers {"content-type" "application/edn" "Accept" "application/edn"}
            :body {:uid uid :pass pass}
            :on-success [:api/login-ok uid]
            :on-failure [:api/login-failed]}}))

(re-frame/reg-event-fx
  :api/login-ok
  (fn [{:keys [db]} [_ uid status _body]]
    {:db (-> db
             (assoc-in [:control :uid] uid)
             (assoc-in [:control :admin] (= status 200)))
     :dispatch-n [(create-log :info status (str uid (if (= status 200) " logged in as admin" " logged in")))
                  [:api/load-applications]
                  [:api/load-operations]
                  [:api/ws-connect]]}))

(re-frame/reg-event-fx
  :api/login-failed
  (fn [_ [_ status _body]]
    {:dispatch (create-log (if (= status 0) :error :warn) (if (= status 0) 500 status) "login-failed")}))

(re-frame/reg-event-fx
  :api/logout
  (fn [{:keys [db]} _]
    {:google-logout! nil
     :db (update db :control dissoc :uid :admin)
     :dispatch (create-log :info "bye" (str (get-in db [:control :uid]) " logged out"))}))

(re-frame/reg-fx
  :google-logout!
  (fn [_] (interop/google-logout!)))

;; --- aplicaciones --------------------------------------------------------------
(re-frame/reg-event-fx
  :api/load-applications
  (fn [{:keys [db]} _]
    {:http {:method :get
            :url (str (:url-base db) "applications")
            :headers {"Accept" "application/edn"}
            :on-success [:applications/replace]
            :on-failure [:api/log-error]}}))

(re-frame/reg-event-db
  :applications/replace
  (fn [db [_ _status body]]
    (update-in db [:applications] merge body)))

(re-frame/reg-event-fx
  :api/load-operations
  (fn [{:keys [db]} _]
    {:http {:method :get
            :url (str (:url-base db) "operations")
            :headers {"Accept" "application/edn"}
            :on-success [:operations/replace]
            :on-failure [:api/log-error]}}))

(re-frame/reg-event-db
  :operations/replace
  (fn [db [_ _status body]]
    (assoc db :operations body)))

(re-frame/reg-event-fx
  :api/save-app
  (fn [{:keys [db]} [_ app-id]]
    {:http {:method :post
            :url (str (:url-base db) "store/" app-id)
            :headers {"content-type" "application/edn" "Accept" "application/edn"}
            :body (get-in db [:applications :editable app-id])
            :on-success [:api/instantiate app-id]
            :on-failure [:api/log-error]}}))

(re-frame/reg-event-fx
  :api/instantiate
  (fn [{:keys [db]} [_ app-id]]
    {:http {:method :post
            :url (str (:url-base db) "instantiate/" app-id)
            :headers {"Accept" "application/edn"}
            :on-success [:applications/editable-replace app-id]
            :on-failure [:api/log-error]}}))

(re-frame/reg-event-db
  :applications/editable-replace
  (fn [db [_ app-id _status body]]
    (assoc-in db [:applications :editable app-id] body)))

(re-frame/reg-event-fx
  :api/load-stored-app
  (fn [{:keys [db]} [_ app-id]]
    {:http {:method :post
            :url (str (:url-base db) "load/" app-id)
            :headers {"Accept" "application/edn"}
            :on-success [:applications/editable-replace app-id]
            :on-failure [:api/log-error]}}))

(re-frame/reg-event-fx
  :api/remove-app
  (fn [{:keys [db]} [_ app-id]]
    {:http {:method :post
            :url (str (:url-base db) "remove/" app-id)
            :headers {"content-type" "application/edn" "Accept" "application/edn"}
            :on-success [:api/remove-app-ok app-id]
            :on-failure [:api/log-error]}}))

(re-frame/reg-event-fx
  :api/remove-app-ok
  (fn [_ [_ app-id status _body]]
    {:dispatch-n [(create-log :info status (str "Removed app " app-id))
                  [:api/load-applications]
                  [:reset! [:designer/ctrl :app] nil]]}))

(re-frame/reg-event-fx
  :api/remove-inst
  (fn [{:keys [db]} [_ app-id inst-id]]
    {:http {:method :post
            :url (str (:url-base db) "remove/" app-id "/" inst-id)
            :headers {"content-type" "application/edn" "Accept" "application/edn"}
            :on-success [:api/remove-inst-ok app-id inst-id]
            :on-failure [:api/log-error]}}))

(re-frame/reg-event-fx
  :api/remove-inst-ok
  (fn [_ [_ app-id inst-id status _body]]
    {:dispatch-n [(create-log :info status (str "Removed app instance " app-id "/" inst-id))
                  [:api/load-applications]]}))

(defn- set-transitioning [db app inst]
  (-> db
      (update-in [:applications :ready app inst] assoc :robot/status :transitioning)
      (update-in [:applications :editable app :instances inst] assoc :robot/status :transitioning)))

(re-frame/reg-event-fx
  :api/start
  (fn [{:keys [db]} [_ app inst]]
    {:db (set-transitioning db app inst)
     :http {:method :post
            :url (str (:url-base db) "start/" app "/" inst)
            :headers {"content-type" "application/edn" "Accept" "application/edn"}
            :on-success [:api/start-stop-ok app inst "started"]
            :on-failure [:api/start-stop-failed "start-failed"]}}))

(re-frame/reg-event-fx
  :api/stop
  (fn [{:keys [db]} [_ app inst]]
    {:db (set-transitioning db app inst)
     :http {:method :post
            :url (str (:url-base db) "stop/" app "/" inst)
            :headers {"content-type" "application/edn" "Accept" "application/edn"}
            :on-success [:api/start-stop-ok app inst "stopped"]
            :on-failure [:api/start-stop-failed "stop-failed"]}}))

(re-frame/reg-event-fx
  :api/start-stop-ok
  (fn [_ [_ app inst verb _status _body]]
    {:dispatch (create-log :info 200 (str app "/" inst " " verb "!"))}))

(re-frame/reg-event-fx
  :api/start-stop-failed
  (fn [_ [_ reason status _body]]
    {:dispatch (create-log (if (= status 0) :error :warn) (if (= status 0) 500 status) reason)}))

(re-frame/reg-event-fx
  :api/import-app
  (fn [{:keys [db]} [_ {:keys [file app-name]}]]
    {:http {:method :post
            :url (str (:url-base db) "store/" app-name)
            :headers {"content-type" "application/edn" "Accept" "application/edn"}
            :body file
            :on-success [:api/import-app-ok app-name]
            :on-failure [:api/log-error]}}))

(re-frame/reg-event-fx
  :api/import-app-ok
  (fn [_ [_ app-name status _body]]
    {:dispatch-n [[:api/instantiate app-name]
                  (create-log :info status "App saved!")
                  [:reset! [:designer/ctrl :import] nil]]}))

;; --- usuarios -------------------------------------------------------------------
(re-frame/reg-event-fx
  :api/load-users
  (fn [{:keys [db]} _]
    {:http {:method :get
            :url (str (:url-base db) "users")
            :headers {"Accept" "application/edn"}
            :on-success [:users/replace]
            :on-failure [:api/log-error]}}))

(re-frame/reg-event-db
  :users/replace
  (fn [db [_ _status body]]
    (assoc db :users body)))

(re-frame/reg-event-fx
  :api/save-users
  (fn [{:keys [db]} _]
    {:http {:method :put
            :url (str (:url-base db) "users")
            :headers {"content-type" "application/edn" "Accept" "application/edn"}
            :body (:users db)
            :on-success [:api/log-info "Users saved!"]
            :on-failure [:api/log-error]}}))

(re-frame/reg-event-fx
  :api/log-info
  (fn [_ [_ msg status _body]]
    {:dispatch (create-log :info status msg)}))

(re-frame/reg-event-fx
  :api/log-error
  (fn [_ [_ status body]]
    {:dispatch (create-log :error status body)}))
