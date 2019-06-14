(ns robot.util.j3270
  (:require [clojure.java.io :as io]
            [clojure-csv.core :as csv])
  (:import (com.jagacy Key)
           (com.jagacy Location)
           (com.jagacy Session3270)
           (com.jagacy.util JagacyException)))

(defmulti exec-script (fn [_ {:keys [cmd]}] cmd))

(defmethod exec-script "writePosition" [session {:keys [cmd params val vars]}]
  (println "Executing: " cmd)
  (println "With : " params " : " val)
  (println "Into : " vars " : " vars)
  (let [start-time (System/currentTimeMillis)
        positionx (Integer/parseInt (first (clojure.string/split (clojure.string/trim params) #",")))
        positiony (Integer/parseInt (second (clojure.string/split (clojure.string/trim params) #",")))]
    (.writePosition session positionx positiony val)
    nil))

(defmethod exec-script "waitForChange" [session {:keys [cmd params val vars]}]
  (println "Executing: " cmd)
  (println "With : " params " : " val)
  (println "Into : " vars)
  (let [start-time (System/currentTimeMillis)]
    (.waitForChange session (Integer/parseInt params))
    nil))

(defmethod exec-script "waitForUnlock" [session {:keys [cmd params val vars]}]
  (println "Executing: " cmd)
  (println "With : " params " : " val)
  (println "Into : " vars)
  (let [start-time (System/currentTimeMillis)]
    (.waitForUnlock session (Integer/parseInt params))
    nil))

(defmethod exec-script "writeKey" [session {:keys [cmd params val vars]}]
  (println "Executing: " cmd)
  (println "With : " params " : " (= (clojure.string/lower-case params) "key.enter"))
  (println "Into : " vars)
  (let [start-time (System/currentTimeMillis)]
    (println "condición")
    (cond
      (= (clojure.string/lower-case params) "key.enter")
      (.writeKey session (Key/ENTER))

      (= (clojure.string/lower-case params) "key.clear")
      (.writeKey session (Key/CLEAR)))
    nil))

(defmethod exec-script "readPosition" [session {:keys [cmd params val vars]}]
  (println "Executing: " cmd)
  (println "With : " params " : " val)
  (println "Into : " vars)
  (let [start-time (System/currentTimeMillis)
        positionx (Integer/parseInt (first (clojure.string/split (clojure.string/trim params) #",")))
        positiony (Integer/parseInt (second (clojure.string/split (clojure.string/trim params) #",")))
        val (Integer/parseInt val)
        read (.readPosition session positionx positiony val)]
    {(keyword vars) read}))

(defmethod exec-script "waiForPosition" [session {:keys [cmd params val vars]}]
  (println "Executing: " cmd)
  (println "With : " params " : " val)
  (println "Into : " vars)
  (let [start-time (System/currentTimeMillis)
        positionx (Integer/parseInt (first (clojure.string/split (clojure.string/trim params) #",")))
        positiony (Integer/parseInt (second (clojure.string/split (clojure.string/trim params) #",")))
        time (Integer/parseInt (nth (clojure.string/split (clojure.string/trim params) #",") 3))]
    (.waitForPosition session positionx positiony val time)
    nil))


(defn lazy-read-csv
  [csv-file]
  (let [in-file (io/reader csv-file)
        csv-seq (csv/parse-csv in-file)
        lazy (fn lazy [wrapped]
               (lazy-seq
                 (if-let [s (seq wrapped)]
                   (cons (first s) (lazy (rest s)))
                   (.close in-file))))]
    (lazy csv-seq)))

(defn script-reducer [[session result] [cmd params val vars]]
  "Función que reduce la estructura de datos generada por **create-nav** para ejecutar cada instrucción mediante **exec-cmd**
   driver: {Firefox|Chrome} Selenium Driver.
   result: Vector dónde se almacenará los resultados de cada operación (tiempo de ejecución).
   step: indice de paso de ejecución.
   command: comando a ejecutar"
  (println "Enviando: " {:cmd cmd :params params :val val :vars vars})
  (Thread/sleep 4000)
  (let [r (exec-script session {:cmd cmd :params params :val val :vars vars})]
    [session (merge result r)]))

(defn create-csv-lst [csv]
  (lazy-read-csv csv))

(defn execute-script [csv-lst address port ssl? protocol]
  "Función que crea la estructura de datos requerida por el reader a partir de un xml parseado mediante (clojure.xml/parse).
  nav-xml: xml parseado mediante (clojure.xml/parse).   "
  (let [session (Session3270. "iwscrape" address port protocol ssl?)
        _ (.open session)
        result (reduce script-reducer
                       [session {}]
                       (rest csv-lst))
        _ (.close session)]
    (second result)
    ))
