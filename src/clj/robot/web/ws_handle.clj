(ns robot.web.ws-handle
  (:require [clojure.data.json :as json]
            [clojure.tools.logging :as log]
            [clojure.pprint :as pp]
            ;[ring.util.response :refer [response]]
            [integrant.core :as ig]
            [aleph.http :as http]
            [ring.util.response :refer [response]]

            [ring.middleware.multipart-params :as multipart]
            [ring.middleware.nested-params :as nested]
            [ring.middleware.cors :as cors]
            [ring.middleware.params :as params]
            [ring.middleware.multipart-params.byte-array :refer [byte-array-store]]
            [ring.middleware.keyword-params :as keyword-params]
            [ring.middleware.format :refer [wrap-restful-format]]

            [taoensso.sente :as sente]
            [taoensso.sente.server-adapters.aleph :refer (get-sch-adapter)]

            [util.io :refer [read-body]]
            [util.http :refer [smart-params
                               http-code-of
                               defhandler
                               middleware-query-params
                               make-handler
                               valid-post-request?]]
            ))

(defmethod ig/init-key :robot.web.ws-handle/manager
  [_ {}]
  (log/debug "preparando manager")
  (let [{:keys [ch-recv send-fn connected-uids
                ajax-post-fn ajax-get-or-ws-handshake-fn]
         :as ws-mngr}
        (sente/make-channel-socket-server! (get-sch-adapter) {})]
    ws-mngr))

(defmethod ig/init-key :robot.web.ws-handle/handshake
  [_ {:keys [manager]}]
  (let [{:keys [ajax-get-or-ws-handshake-fn]} manager]
    (fn [req]
      @(ajax-get-or-ws-handshake-fn req))))

(defmethod ig/init-key :robot.web.ws-handle/post
  [_ {:keys [manager]}]
  (:ajax-post-fn manager))
