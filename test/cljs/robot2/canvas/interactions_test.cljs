(ns robot2.canvas.interactions-test
  (:require [cljs.test :refer [deftest is testing]]
            [robot2.canvas.interactions :as inter]))

(deftest wheel->zoom-test
  (testing "sin anchor, ancla en el centro del viewBox (no en la esquina superior-izquierda)"
    (let [{:keys [x y w h]} (inter/wheel->zoom {:x 0 :y 0 :w 1000 :h 600} -120)
          cx (+ x (/ w 2)) cy (+ y (/ h 2))]
      (is (< (Math/abs (- cx 500)) 0.001) "el centro horizontal del viewBox no se mueve")
      (is (< (Math/abs (- cy 300)) 0.001) "el centro vertical del viewBox no se mueve")))
  (testing "con anchor, ese punto queda fijo tras el zoom"
    (let [zoom {:x 0 :y 0 :w 1000 :h 600}
          anchor [200 150]
          ;; trackpad separar dedos -> deltaY NEGATIVO -> exp(+0.001*-240)=0.787
          ;; -> viewBox se achica (zoom in, contenido mas grande)
          {:keys [x y w h] :as new-zoom} (inter/wheel->zoom zoom -240 anchor)]
      (is (< w 1000) "deltaY negativo (separar dedos) -> scale<1 -> viewBox se reduce (zoom in)")
      ;; el punto bajo el cursor, en proporcion al nuevo viewBox, debe seguir
      ;; en la misma posicion relativa que tenia en el viewBox original
      (is (< (Math/abs (- (/ (- 200 x) w) (/ (- 200 0) 1000))) 0.001)
          "la posicion relativa-x del anchor no cambia")
      (is (< (Math/abs (- (/ (- 150 y) h) (/ (- 150 0) 600))) 0.001)
          "la posicion relativa-y del anchor no cambia")))
  (testing "respeta los limites de zoom-bounds (no permite achicar de mas)"
    ;; delta negativo fuerte (mucho zoom-in) desde el limite inferior
    (let [zoom {:x 0 :y 0 :w 200 :h 150}
          {:keys [w h]} (inter/wheel->zoom zoom -1200)]
      (is (= w 200) "no reduce w por debajo de min-w")
      (is (= h 150) "no reduce h por debajo de min-h"))))
