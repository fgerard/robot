(ns robot.web.rest-server
  (:require [clojure.tools.logging :as log]
            [clojure.java.io :refer [file]]
            [aleph.http :as aleph-http]
            [integrant.core :as ig])
  (:import (io.netty.handler.ssl SslContextBuilder)
           (io.netty.handler.ssl.util InsecureTrustManagerFactory)))

(defmethod ig/init-key :robot.web/rest-server
  [_ {:keys [host http-host http-port https-host https-port server-key server-crt server-key-pass handler]
      :or   {http-host "localhost" http-port 8050 https-host "localhost" https-port 4050}}]
  (log/info :robot.web/https-rest-server)
  (try
    (let [http-host (or host http-host)
          https-host (or host https-host)
          _ (log/info "Starting HTTP Server, host:port -> " http-host ":" http-port)
          _ (log/info "Starting HTTPS Server, host:port -> " https-host ":" https-port)
          http-server-inet (and http-port (java.net.InetSocketAddress. http-host http-port))
          https-server-inet (and https-port (java.net.InetSocketAddress. https-host https-port))
          ssl-context-builder (and https-port
                                   server-key
                                   server-crt
                                   (if server-key-pass
                                     (SslContextBuilder/forServer (file server-crt)
                                                                  (file server-key)
                                                                  server-key-pass)
                                     (SslContextBuilder/forServer (file server-crt)
                                                                  (file server-key))))
          ;_ (.trustManager ssl-context-builder InsecureTrustManagerFactory/INSTANCE)
          https-serv (and ssl-context-builder (aleph-http/start-server handler {:socket-address https-server-inet :ssl-context (.build ssl-context-builder)}))
          http-serv (and http-port (aleph-http/start-server handler {:socket-address http-server-inet}))]
      (if-not https-serv
        (log/error "Can't start HTTP Server, host:port ->" http-host ":" http-port))
      (if-not http-serv
        (log/error "Can't start HTTPS Server, host:port ->" https-host ":" https-port))
      (cond-> []
              https-serv (conj https-serv)
              http-serv (conj http-serv)))
    (catch Exception e
      (.printStackTrace e)
      )))

(defmethod ig/halt-key! :robot.web/rest-server [_ servers]
  (doseq [server servers]
    (log/info (str "Closing http(s) server: " server))
    (.close server)))
