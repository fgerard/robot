(ns extra-code 
  (:require [clojure.pprint :as pp]
            [test.new-ops :as NO]))

(NO/wait-till #"..:..:.7")

(defn cosa []
  (NO/wait-till #"..:..:.7")
  (java.util.Date.))

(defn fact2 [n]
  (reduce * 1N (range 1 (inc n))))

(println "SIII")
(println "SIII")
(println "SIII")


(comment

  (require '[clojure.java.io :as io])

  (def f (io/file "resources/extra_code.clj"))

  (.getCanonicalPath f)

  (def code-str (slurp f))
  code-str
  (def x (load-string code-str))

  (require '[extra-code :as EC])

  (EC/cosa)

  (def S "(require '[test.new-ops :as NO])) (defn fact [n] (NO/wait-till #\"..:..:.7\") (reduce + 1N (range 1 (inc n))))")

  (def S-frm (read-string S))

  (eval S-frm)


  (fact 23)

  (fact 10)






  )