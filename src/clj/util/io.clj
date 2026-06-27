(ns util.io
  (:import (java.io ByteArrayOutputStream)))

(def DEFAULT-BUFFER-SIZE 4096)

(defn read-stream [in]
  (let [out    (new ByteArrayOutputStream)
        buffer (make-array Byte/TYPE DEFAULT-BUFFER-SIZE)]
    (loop [len (.read in buffer)]
      (if-not (= len -1)
        (do
          (.write out buffer 0 len)
          (recur (.read in buffer)))))
    (.toByteArray out)))

(defn read-body [body]
  (cond
    (nil? body)
    (.getBytes "" "UTF-8")

    (instance? java.io.InputStream body)
    (read-stream body)

    (string? body)
    (.getBytes body "UTF-8")

    :OTHERWIZE
    (.getBytes "" "UTF-8")))
