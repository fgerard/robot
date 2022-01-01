(ns extra-code 
  (:require [clojure.pprint :as pp]
            [clojure.data :refer [diff]]
            [test.new-ops :as NO]))

#_(defn date2local [t]
  (if (inst? t)
    (let [tz (java.util.TimeZone/getDefault)
          tz-name (.getDisplayName tz true java.util.TimeZone/SHORT)
          fd (java.text.SimpleDateFormat. (str "yyyy-MM-dd HH:mmz"))]
      (.format fd t))
    t))

#_(defn get-diffs [ant act]
  (let [new-keys (-> (diff act ant)
                     (first)
                     (keys))
        result (reduce (fn [r k]
                         (conj r [k (date2local (k ant)) :-> (date2local (k act))]))
                       []
                       new-keys)]
    result))

#_(comment
  (def local1-str (slurp "estado/obispo/data-local1.edn"))

  (def local1 (read-string local1-str))

  (pr-str (map name (keys local1)))

  (reduce 
   (fn [r [k v]]
     (println [k v])
     (let [v (get r k [])]
       (update r k conj v)))
   {}
   (map #(reverse (re-find #"([A-Z]+[0-9]{2}).*" %)) (map name (keys local1))))
  )




#_(defn cosa []
  (NO/wait-till #"..:..:.7")
  (java.util.Date.))

#_(defn fact2 [n]
  (reduce * 1N (range 1 (inc n))))

(println "Loading extra_code.clj")


