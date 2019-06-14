(ns robot.core.state
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :as pp]
            [integrant.core :as ig]
            [util.file-util :refer [find-files]]))

(println "loading " (.getName *ns*) "...")

(declare system)

(defmethod ig/init-key :robot.core.state/system [_ _]
  (println :robot.core.state/system)
  (fn []
    system))

(defn start []
  (println "Defining system ...")
  (let [system-data (let [files-internal (find-files "./config" #"^robot-.*-config.edn$")
                          files-external (find-files "./config-ext/robot" #"^robot-.*-config.edn$")]
                      (log/info "Internal config files " (mapv #(.getName %) files-internal))
                      (log/info "External config files " (mapv #(.getName %) files-external))

                      (reduce (fn [result conf-file]
                                (log/info "merging config file: " (.getName conf-file))
                                (merge result (ig/read-string (slurp conf-file))))
                              {}
                              (concat files-internal files-external)))]
    (ig/load-namespaces system-data)
    (def system
      (ig/init system-data))))
