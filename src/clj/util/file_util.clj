(ns util.file-util
  (:require [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [util.date-util :as date-util])
  (:import (java.io File)))

(println "Loading " *ns* " ...")

(defn find-files [dir-path reg-exp]
  (let [directory (new File dir-path)
        files     (filter #(re-find reg-exp (.getName %)) (file-seq directory))]
    files))

(defn exists? [directory filter-pattern]
  (let [files (find-files directory filter-pattern)]
    (and files (> (count files) 0) files)))

(defn rename [origin destination]
  (let [origin-file      (if (instance? File origin) origin (new File origin))
        destination-file (if (instance? File destination) destination (new File destination))
        succeded         (.renameTo origin-file destination-file)]
    (if succeded destination-file)))


(def uuid-pattern "[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}")

(defn cleanup-markers
  ([file]
   (cleanup-markers file 120000))
  ([file no-modification-delta]
   (if (.exists file)
     (let [modified (.lastModified file)]
       (if (> (System/currentTimeMillis) (+ modified no-modification-delta))
         (if-let [markers (seq (find-files
                                (-> file .getParentFile .getCanonicalPath)
                                (re-pattern (str "^" (.getName file) "\\." uuid-pattern "$"))))]
           (doall
             (mapv (fn [marker]
                     (log/info "Removing old marker: " (.getName marker))
                     (.delete marker)
                     marker)
                   markers))))))))

(defn mark-file
  ([file]
   (mark-file file (str (java.util.UUID/randomUUID))))
  ([file uuid]
  (let [cleaned-markers (cleanup-markers file)
        _ (log/debug "cleanup-markers" (map #(.getName %)cleaned-markers))
        marked-file (java.io.File. (.getParentFile file) (str (.getName file) "." uuid))
        _ (log/debug "marked-file: " (.getName marked-file))
        _ (log/info (-> file .getParentFile .getCanonicalPath))
        files (exists? (-> file .getParentFile .getCanonicalPath) (re-pattern (str "^" (.getName file) "\\." uuid-pattern "$")))]
    (when  (not files)
      (re-pattern (str "^" (.getName file) "\\." uuid-pattern "$"))
      (spit marked-file (.toString (date-util/ahora)))
      (let [files (exists? (-> file .getParentFile .getCanonicalPath) (re-pattern (str "^" (.getName file) "\\." uuid-pattern "$")))]
        (log/info "mark-files(2) for:" (.getName file) " -> " (map #(.getName %) files))
        (if (> 1 (count files))
          (.remove marked-file)
          marked-file))))))

(defn unmark-file [file uuid]
  (let [marked-file (java.io.File. (.getParentFile file) (str (.getName file) "." uuid))]
    (if (.exists marked-file)
      (.delete marked-file))))

(defn build-lock-file [file]
  (java.io.File. (.getParentFile file) (str (.getName file) ".lock")))

(defn lock-file [file]
  (let [lock-file (build-lock-file file)]
    (when (.createNewFile lock-file)
      (.deleteOnExit lock-file)
      lock-file)))

(defn unlock-file [file]
  (let [lock-file (build-lock-file file)]
    (.delete lock-file)))
