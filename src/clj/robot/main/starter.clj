(ns robot.main.starter
  (:gen-class)
  (:require
   [clojure.tools.logging :as log]
   [clojure.java.io :as io]
   [clojure.pprint :as pp]
   [robot.core.state :as state]
   [robot.util.telegram :as telegram]
   [robot.core.operations :as opr])
  (:import (java.net Authenticator PasswordAuthentication)
           (org.apache.logging.log4j LogManager)
           (org.apache.logging.log4j.core LoggerContext)))

(defn include-extra-code []
  (let [extra-code-file (io/file "resources/extra_code.clj")]
    (when (.exists extra-code-file)
      (try
        (load-file (.getCanonicalPath extra-code-file))
        (reset! opr/extra-code true)
        (log/info "Extra @" (.getCanonicalPath extra-code-file) " loaded. ")
        (catch Exception e
          (log/error e)
          (log/error "Extra @" (.getCanonicalPath extra-code-file) " NOT loaded! "))))))

(let [[_ name version] (-> "./project.clj" slurp read-string vec)]
     (defn name&version
           "Returns name and version reading project.clj"
           []
           [name version]))

(defn -main [& args]
      ; startup
      (let [[name version] (name&version)
            config-dir (get (System/getenv) "ROBOT_CONFIG" "./config")
            log4j (java.io.File. (str  config-dir "/log4j2.xml"))]
           (println "Configuring log4j2 form:" (.getCanonicalPath log4j))
           (-> (cast LoggerContext (LogManager/getContext false)) (.setConfigLocation (.toURI log4j)))
           (println (str (slurp (str config-dir "/banner.txt"))
                         "\n" name " " version "   Clojure: " (clojure-version) "\n"))
           (include-extra-code)
           (.addShutdownHook
             (Runtime/getRuntime)
             (Thread. (fn []
                          (println "Shutting down" name version))))
           (state/start))

      ; handles authenticator (NTLM)
      (let [usr (System/getProperty "http.proxyUser")
            pwd (System/getProperty "http.proxyPassword")]
           (when (and usr pwd)
                 (log/info {:setting-default-authenticator {:user usr :pwd "******"}})
                 (Authenticator/setDefault
                   (proxy [Authenticator] []
                          (getPasswordAuthentication []
                                                     (PasswordAuthentication. usr (.toCharArray pwd)))))))

      (when-let [servers (seq (:robot.web/rest-server state/system))]
                (log/debug :servers2 (pr-str servers))
                (doseq [server servers]
                       (.wait-for-close server))))

; _____     _       _
;| __  |___| |_ ___| |_
;|    -| . | . | . |  _|
;|__|__|___|___|___|_|
