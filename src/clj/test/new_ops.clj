(ns test.new-ops
  (:require [clj-time.core :as t]
            [clj-time.format :as tf]
            [clj-time.coerce :as tc]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.data :refer  [diff]])
  (:import (java.text SimpleDateFormat)))


(def root (io/file "estado"))
(.getCanonicalPath root)
(.exists root)
(def F-act-file (io/file root "estado-local1-P.edn"))
(def F-ant-file (io/file (io/file root "inicial") "estado-local1-P.edn"))
(.exists F-act-file)
(.exists F-ant-file)

(def act (read-string (slurp F-act-file)))
(def ant (read-string (slurp F-ant-file)))
(pp/pprint act)
(pp/pprint ant)
(pp/pprint (nth (diff act ant) 0))

(:PU02AM act)
(:PU02AM ant)

(defn get-diffs [act ant]
  (let [new-keys (-> (diff act ant)
                     (first)
                     (keys))
        result (reduce (fn [r k]
                         (conj r [k (k ant) :-> (k act)]))
                       []
                       new-keys)]
    result))

(pp/pprint (get-diffs act ant))

(saca :PU02ST)






(defn wait-till [re]
  (let [tz (t/time-zone-for-id "America/Mexico_City")
        fmt (tf/formatter "HH:mm:ss")
        fmt1 (tf/with-zone fmt tz)
        now (* (int (/ (System/currentTimeMillis) 1000)) 1000)
        delay (reduce
               (fn [result delta]
                 (let [t (+ now (* delta 1000))
                       now (tc/from-long t)
                       d (tf/unparse fmt1 now)]
                   (if (re-matches re d)
                     (reduced delta)
                     1)))
               nil
               (range (* 3600 24)))]
    (println :delay delay)
    (Thread/sleep (* 1000 delay))
    (println (java.util.Date.))))

;(wait-till #"..:..:.7")

(comment
  
(let [t0 (java.util.Date.)
      s (seconds-till #"..:..:55")]
  (println [t0 s])
  ;(Thread/sleep (* s 1000))
  ;(java.util.Date.)
  s
  )

(let [sdf (SimpleDateFormat. "CDT HH:mm:ss")]
  (.format sdf (java.util.Date.)))

(t/today-at 12 00 00)
(tf/show-formatters)

(def fmt (tf/formatter "HH:mm:ss"))

(tf/unparse fmt (tl/local-now))
(tl/local-now)
(def fmt (tf/formatters :time))

(tl/format-local-time (tl/local-now) :hour-minute-second)
(tl/format-local-time (t/now) :hour-minute-second)

(tf/unparse-local fmt (t/now))

(clojure.pprint/pprint tl/*local-formatters*)

(tc/to-date-time (tc/from-long (System/currentTimeMillis)) )

(def tz (t/time-zone-for-id "America/Mexico_City"))

(def fmt (tf/formatter "HH:mm:ss"))

(def fmt1 (tf/with-zone fmt tz))


(tf/unparse fmt (tc/from-long (System/currentTimeMillis)))
(tf/unparse fmt1 (tc/from-long (System/currentTimeMillis)))


(java.time.LocalTime/now)

)