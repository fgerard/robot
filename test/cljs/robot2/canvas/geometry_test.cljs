(ns robot2.canvas.geometry-test
  (:require [cljs.test :refer [deftest is testing]]
            [robot2.canvas.geometry :as geom]))

(deftest compute-entry&exit-test
  (testing "cajas alineadas horizontalmente, B a la derecha de A"
    (let [[[x1 _] [x2 _]] (geom/compute-entry&exit
                            {:x 0 :y 0 :h 60 :w 130}
                            {:x 300 :y 0 :h 60 :w 130})]
      (is (< x1 x2) "el punto de salida de A debe quedar a la izquierda del punto de entrada de B")))
  (testing "cajas alineadas verticalmente, mismo x"
    (let [[[x1 y1] [x2 y2]] (geom/compute-entry&exit
                              {:x 0 :y 0 :h 60 :w 130}
                              {:x 0 :y 300 :h 60 :w 130})]
      (is (= x1 x2 65) "con el mismo x, el punto de salida/entrada no se desplaza horizontalmente")
      (is (< y1 y2)))))

(deftest compute-exit2point-test
  (testing "el punto de salida apunta hacia el punto suelto"
    (let [[x y] (geom/compute-exit2point {:x 0 :y 0 :h 50 :w 130} [500 25])]
      (is (> x 0))
      (is (<= x 65)))))

(deftest connect2points-test
  (testing "el ultimo punto del trazo coincide con el destino"
    (let [pts (geom/connect2points [0 0] [100 50])]
      (is (= (last pts) [100 50])))))

(deftest connecting-path-test
  (testing "genera un path SVG bien formado de 4 puntos"
    (is (re-matches #"M-?\d+,-?\d+L-?\d+,-?\d+L-?\d+,-?\d+L-?\d+,-?\d+"
                     (geom/connecting-path [0 0] [100 50])))))

(deftest position&rotate-test
  (testing "incluye translate al punto destino"
    (is (re-find #"translate\(100 50\)" (geom/position&rotate 0 0 100 50)))))
