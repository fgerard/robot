(ns test.searcher)

(require '[clojure.java.io :as io])



(defn perdidos [f-name]
  (with-open [in (io/reader f-name)]
    (let [faltantes (line-seq in)
          re #"(.*MILLONES.)(.*)"]
      (into
       #{}
       (doall
         (map (fn [line]
                (last (re-matches re line))) faltantes))))))

(defn do-match [perdidos1 perdidos2 f-name]
  (with-open [in (io/reader f-name)]
    (let [lines (line-seq in)
          re1 #"vault - Storing file \["
          re2 #"\.vault - File \["
          re3 #"^.*\-([0-9]+)\-\-.*$"]
      (reduce
       (fn [[init end :as result] [idx line]]
         (if (= 0 (mod idx 100000)) (println idx))
         (cond
           (re-find re1 line)
           (let [[_ id :as x] (re-matches re3 line)]
             ;(if-not (init id)
              ; (println "ID:" id " Not found in init."))
             [(disj init id) end])

           (re-find re2 line)
           (let [[_ id] (re-matches re3 line)]
             ;(if-not (end id)
              ; (println "ID:" id " Not found in end."))
             [init (disj end id)])

           :OTHERWIZE
           result))
       [perdidos1 perdidos2]
       (map-indexed (fn [idx line]
                      [idx line]) lines)))))


;(perdidos "faltaronDeEmitir.txt")
;(def P (perdidos "faltaronDeEmitir.txt"))
;(do-match P P "strauz-vault1.log")
