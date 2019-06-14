(ns robot.tests.wstest
  (:require
   [clojure.core.async :refer [go go-loop put! >! <! chan] :as async]
   [clojure.pprint :as pp]
   [clojure.tools.logging :as log]
   [integrant.core :as ig]
   [manifold.stream :as s]
   [aleph.http :as http]
   [util.http-client :as http-client]))

(def flg (atom true))

(defn follow [app inst]
  (let [conn @(http/websocket-client (format "ws://localhost:8051/follow/%s/%s" app inst))]
    (loop []
      (let [msg @(s/take! conn)]
        (println msg)
        (if @flg
          (recur)
          (s/close! conn)))))
  (println "YA REGRESO"))

(comment

 (future
  (follow "prueba2" "uno"))

 (reset! flg false)
 (reset! flg true)
 )
