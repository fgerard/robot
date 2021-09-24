(ns robot.web.rest-handle
  (:require [clojure.data.json :as json]
            [clojure.tools.logging :as log]
            [clojure.pprint :as pp]
            [clj-jwt.core :as jwt]
            [clj-jwt.key :refer [private-key public-key pem->public-key]]
    ;[ring.util.response :refer [response]]
            [integrant.core :as ig]
            [ring.util.response :refer [response]]
            [keyval.konservedb :as db]
            [ring.middleware.multipart-params :as multipart]
            [ring.middleware.nested-params :as nested]
            [ring.middleware.cors :as cors]
            [ring.middleware.params :as params]
            [ring.middleware.multipart-params.byte-array :refer [byte-array-store]]
            [ring.middleware.keyword-params :as keyword-params]
            [ring.middleware.format :refer [wrap-restful-format]]
            [ring.middleware.file :refer [wrap-file]]
            [ring.middleware.session :refer [wrap-session]]

            [util.io :refer [read-body]]
            [util.http :refer [smart-params
                               http-code-of
                               defhandler
                               middleware-query-params
                               make-handler
                               valid-post-request?]]
            [util.crypt-util :as crypt])
  ;(:import (org.bouncycastle.jce.provider BouncyCastleProvider))
  )


(defn get-google-keys []
  (let [certs (json/read-str (slurp "https://www.googleapis.com/oauth2/v1/certs"))]
    (doall
      (map
        (fn [[uuid cert-str]]
          (pem->public-key (java.io.StringReader. cert-str) nil))
        certs))))

;(java.security.Security/addProvider (new BouncyCastleProvider))

(defmethod ig/init-key :robot.web.rest-handle/handlers
  [_ routes]
  (log/info :robot.web.rest-handle/handlers)
  (->
    (make-handler
      routes)

    (cors/wrap-cors #".*localhost.*")
    ;(cors/wrap-cors #".*dsw.*")
    ;(cors/wrap-cors #".*10\.3\.2*")
    ;(cors/wrap-cors #".*interware.*")
    (wrap-restful-format :formats [:edn :json-kw :yaml-kw :yaml-in-html])
    ;(nested/wrap-nested-params)
    (multipart/wrap-multipart-params {:store (byte-array-store)})
    (keyword-params/wrap-keyword-params)
    (params/wrap-params)
    (wrap-session)
    (wrap-file "resources/public")
    ))

(defn toJWT [token]
  (try
    (jwt/str->jwt token)
    (catch Exception e
      nil)))

(defmethod ig/init-key :robot.web.rest-handle/login
  [_ {:keys [db]}]
  (defhandler login [request tx]
              ;(println (str "\n\nLOGIN:" (pr-str (get-in request [:params]))))
              (let [allowed-users (db/get db [:allowed-users])
                    session (:session request)
                    ;_ (println "SESSION: " session)
                    {:keys [uid pass]} (:params request)
                    token-info (toJWT pass)]
                (if token-info
                  (if (some identity (map (fn [goog-key]
                                            (let [ok (jwt/verify token-info goog-key)]
                                              (log/debug (str "verify goog token: " ok))
                                              ok)) (get-google-keys)))
                    (let [email (get-in token-info [:claims :email])
                          _ (log/info "email: " (pr-str email))]
                      (if (re-matches #"^felipe.gerard.c@gmail.com$|^.*\@quantumlabs.ai" email) ;^.*\@interware.com.mx$|
                        (do
                          (log/debug "ES ADMIN!")
                          {:status 200 :session (assoc session :uid uid :admin true)})
                        (if-let [{:keys [admin]} (get allowed-users email)]
                          {:status (if admin 200 204) :session (assoc session :uid uid :admin admin)}
                          {:status 405})))
                    {:status 405})
                  (let [{:keys [hpass admin]} (get allowed-users uid)
                        _ (log/debug "SHA1-HEX: " [pass] [(crypt/sha1-as-hex pass)] [hpass])
                        should-login? (and hpass (not (nil? pass)) (seq pass) (= (crypt/sha1-as-hex pass) hpass))]
                    (if should-login?
                      {:status (if admin 200 204) :session (assoc session :uid uid :admin admin)}
                      {:status 405}))))))

(defmethod ig/init-key :robot.web.rest-handle/load-users
  [_ {:keys [db]}]
  (defhandler load-users [request tx]
              (log/debug "\nLOAD-USERS:" (:params request))
              (let [session (:session request)]             ;(-> request :session :admin)
                (if-let [admin (:admin session)]
                  {:status 200 :body (db/get db [:allowed-users])}
                  {:status 401}))))

(defmethod ig/init-key :robot.web.rest-handle/save-users
  [_ {:keys [db]}]
  (defhandler save-users [request tx]
              (log/debug "\nSAVE-USERS:")
              ;(pp/pprint request)
              (let [session (:session request)
                    users (:params request)]
                (if-let [admin (:admin session)]
                  (do
                    (db/put db [:allowed-users] users)
                    {:status 204})
                  {:status 401}))))


(defmethod ig/init-key :robot.web.rest-handle/get-operation
  [_ {:keys [operations]}]
  (defhandler get-operation [request tx]
              ;(pp/pprint request)
              ;(pp/pprint operations)
              (if (or true)                                 ;(valid-post-request? request)
                (if-let [operation (get-in request [:params :operation])]
                  (if-let [[f ui] (operations (keyword operation))]
                    {:status 200 :body ui}
                    {:status 404 :body (format "Operation [%s] undefined" operation)})
                  {:status 200 :body (into [] (sort (map (comp first) operations)))})
                {:status 400 :body "Request has invalid parameters and or headers"})))

(defmethod ig/init-key :robot.web.rest-handle/get-operations
  [_ {:keys [operations]}]
  (defhandler get-operations [request tx]
              ;(pp/pprint request)
              ;(pp/pprint operations)
              (if (or true)                                 ;(valid-post-request? request)
                (let [opers (into {} (map (fn [[k [f ui]]]
                                            [k ui]) operations))]
                  {:status 200 :body opers})
                {:status 400 :body "Request has invalid parameters and or headers"})))

(defmethod ig/init-key :robot.web.rest-handle/get-applications
  [_ {:keys [robot-info]}]
  (defhandler get-applications [request tx]
              (if (or true)                                 ;(valid-post-request? request)
                {:status 200 :body (robot-info)}
                {:status 400 :body "Request has invalid parameters and or headers"})))


(defmethod ig/init-key :robot.web.rest-handle/get-application
  [_ {:keys [app-info]}]
  (defhandler get-application [request tx]
              (if (or true)                                 ;(valid-post-request? request)
                (if-let [app (get-in request [:params :application])]
                  {:status  200
                   :headers {:Content-Disposition (str "attachment; filename=" app ".edn")}
                   :body    (with-out-str (clojure.pprint/pprint ((app-info) app)))}
                  {:status 400 :body "Request has invalid parameters and or headers"}))))

(defmulti do-cmd (fn [_ admin? {:keys [cmd]}] cmd) :default :default)

(defmethod do-cmd :default [_ admin? {:keys [cmd]}]
  {:status 404 :body (format "Command: [%s] unknown!" (name cmd))})

(defmethod do-cmd "load" [robot-controller admin? {:keys [cmd app-id]}]
  (let [{:keys [cmd-status cmd-err]
         :as   robot} (robot-controller [:load {:app-id app-id}])]
    (if (= :success cmd-status)
      {:status 200 :body (get-in robot [:apps app-id])}
      {:status 404 :body (format "App [%s] not found [%s]" (name app-id) cmd-err)})))

(defmethod do-cmd "store" [robot-controller admin? {:keys [cmd app-id app-params instances init-state states] :as req}]
  (let [{:keys [cmd-status cmd-err]
         :as   robot} (if (and admin? app-params instances init-state states)
                        (robot-controller [:store {:app-id   app-id
                                                   :app-conf {:app-params app-params
                                                              :instances  instances
                                                              :init-state init-state
                                                              :states     states}}])
                        {:cmd-status :fatal :cmd-err (if admin?
                                                       "[app-params instances init-state states] missing from request use header [content-type: application/edn]"
                                                       "Must be admin!")})]
    (if (= :success cmd-status)
      {:status 204}
      {:status (if admin? 400 401) :body (format "Wrong format [%s]" cmd-err)})))

(defmethod do-cmd "remove" [robot-controller admin? {:keys [cmd app-id inst-id] :as params}]
  (log/info (pr-str params))
  (let [{:keys [cmd-status cmd-err]
         :as   robot} (if admin?
                        (robot-controller [:remove params])
                        {:cmd-status :fatal :cmd-err "Must be admin!"})]
    (if (= :success cmd-status)
      {:status 200 :body (get-in robot [:apps app-id])}
      {:status (if admin? 404 401) :body (format "App [%s] not found [%s]" (name app-id) cmd-err)})))

(defmethod do-cmd "instantiate" [robot-controller admin? {:keys [cmd app-id app-params instances init-state states]}]
  (let [{:keys [cmd-status cmd-err]
         :as   robot} (robot-controller [:instantiate {:app-id app-id}])]
    (if (= :success cmd-status)
      {:status 204}
      {:status (if admin? 404 401) :body (format "App [%s] error: %s" (name app-id) cmd-err)})))

(defmethod do-cmd "start" [robot-controller admin? {:keys [cmd app-id inst-id app-params instances init-state states]}]
  (let [{:keys [cmd-status cmd-err]
         :as   robot} (robot-controller [:start {:app-id app-id :inst-id inst-id}])]
    (if (= :success cmd-status)
      {:status 204}
      {:status (if admin? 404 401) :body (format "App [%s] error: %s" (name app-id) cmd-err)})))

(defmethod do-cmd "stop" [robot-controller admin? {:keys [cmd app-id inst-id app-params instances init-state states]}]
  (let [{:keys [cmd-status cmd-err]
         :as   robot} (robot-controller [:stop {:app-id app-id :inst-id inst-id}])]
    (if (= :success cmd-status)
      {:status 204}
      {:status 404 :body (format "App [%s] error: %s" (name app-id) cmd-err)})))

(defmethod ig/init-key :robot.web.rest-handle/robot
  [_ {:keys [robot-controller]}]
  (let [do-cmd-controller (partial do-cmd robot-controller)]
    (defhandler robot-controller [request tx]
                ;(pp/pprint request)
                (do-cmd-controller (-> request :session :admin) (:params request)))))

(defmethod ig/init-key :robot.web.rest-handle/post-event
  [_ {:keys [evt2topic]}]
  (defhandler post-event [request tx]
    (log/debug "\npost-event:")
    (let [{:keys [topic name score img]} (:params request)]
      (evt2topic topic {:topic topic :name name :score score :img img})
      {:status 204})))

(defmethod ig/init-key :robot.web.rest-handle/default
  [_ _]
  (let []
    (defhandler robot-controller [request tx]
                {:status 404})))
