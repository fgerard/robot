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
      (is (< y1 y2))
      (is (= y1 60) "la salida de abajo de A debe quedar exactamente en su borde inferior real (y + h), no mas alla")
      (is (= y2 300) "la entrada de arriba de B debe quedar exactamente en su borde superior real (y), no antes")))
  (testing "caso inverso: A abajo de B, sale por arriba de A y entra por abajo de B"
    (let [[[_ y1] [_ y2]] (geom/compute-entry&exit
                            {:x 0 :y 300 :h 60 :w 130}
                            {:x 0 :y 0 :h 60 :w 130})]
      (is (= y1 300) "borde superior real de A, sin corrimiento")
      (is (= y2 60) "borde inferior real de B, sin sobrepasarlo"))))

(deftest compute-exit2point-test
  (testing "punto suelto a la misma altura que el centro: sale por el borde derecho"
    (let [[x y] (geom/compute-exit2point {:x 0 :y 0 :h 50 :w 130} [500 25])]
      (is (= x 130) "borde derecho de la caja (x + w)")
      (is (= y 25) "misma altura que el centro, pendiente 0")))
  (testing "punto suelto abajo y a la derecha: sale por una esquina, no por el centro"
    (let [[x y] (geom/compute-exit2point {:x 0 :y 0 :h 50 :w 130} [200 200])]
      (is (> x 65))
      (is (> y 25)))))

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

(deftest bounding-viewbox-test
  (let [default {:x 0 :y 0 :w 1342 :h 600}]
    (testing "sin cajas (app sin estados todavia): regresa default tal cual"
      (is (= default (geom/bounding-viewbox [] 60 default))))
    (testing "una sola caja chica: el resultado no baja del piso de default"
      (let [vb (geom/bounding-viewbox [{:x 10 :y 10 :w 130 :h 60}] 60 default)]
        (is (= (:w vb) 1342) "w no baja del piso de default")
        (is (= (:h vb) 600) "h no baja del piso de default")
        (is (= (:x vb) -50) "x = min-x - margin")
        (is (= (:y vb) -50) "y = min-y - margin")))
    (testing "estado bien abajo del alto del default (1342x600) -- el caso reportado: se recortaba"
      (let [vb (geom/bounding-viewbox [{:x 0 :y 0 :w 130 :h 60}
                                       {:x 0 :y 900 :w 130 :h 60}] 60 default)]
        (is (>= (+ (:y vb) (:h vb)) 960) "el viewbox debe llegar hasta el borde inferior de la caja de mas abajo (900+60) mas margen")
        (is (= (:y vb) -60) "y = min-y (0) - margin")))
    (testing "varias cajas mas alla del piso de default: el viewbox encuadra desde la mas a la izquierda/arriba hasta la mas a la derecha/abajo"
      (let [vb (geom/bounding-viewbox [{:x 0 :y 0 :w 100 :h 50}
                                       {:x 2000 :y 1500 :w 100 :h 50}] 20 default)]
        (is (= (:x vb) -20))
        (is (= (:y vb) -20))
        (is (= (:w vb) 2140) "(2000+100 - 0) + 2*20")
        (is (= (:h vb) 1590) "(1500+50 - 0) + 2*20")))))
