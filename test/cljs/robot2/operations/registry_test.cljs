(ns robot2.operations.registry-test
  "Prueba de regresion: el catalogo real (resources/public/edn/operations-schema.edn)
   debe poder leerse con cljs.reader/read-string -- el mismo reader que usa
   robot2.operations.registry al recibirlo del servidor -- y luego compilarse
   a RegExp reales con compile-schema."
  (:require [cljs.test :refer [deftest is testing]]
            [cljs.reader :as reader]
            [robot2.operations.registry :as registry]
            ["fs" :as fs]))

(deftest real-schema-file-parses
  (testing "el archivo real parsea como edn plano (sin literales de regex) y trae las 25 operaciones"
    (let [content (.readFileSync fs "resources/public/edn/operations-schema.edn" "utf8")
          data (reader/read-string content)]
      (is (map? data))
      (is (= 25 (count data)))
      (is (contains? data :sleep))
      (is (string? (get-in data [:sleep :flds 0 :re])))))
  (testing "compile-schema convierte los :re de string a RegExp real"
    (let [content (.readFileSync fs "resources/public/edn/operations-schema.edn" "utf8")
          compiled (registry/compile-schema (reader/read-string content))]
      (is (instance? js/RegExp (get-in compiled [:sleep :flds 0 :re])))
      (is (re-matches (get-in compiled [:sleep :flds 0 :re]) "1000")))))
