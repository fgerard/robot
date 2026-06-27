(ns robot2.canvas.geometry
  "Matematica pura del diagrama SVG: puntos de entrada/salida de flechas entre
   cajas de estado, trazo de conectores y rotacion de puntas de flecha.

   Extraido sin tocar la formula de svg.cljs (v1) -- esto ya era codigo
   correcto y testeable, simplemente estaba enterrado junto al render y a los
   handlers de eventos. No depende de reagent, re-frame ni del DOM: se puede
   probar con cljs.test sin levantar la app."
  (:require [goog.string :as gstring]
            [goog.string.format]))

(defn compute-entry&exit
  "Dadas dos cajas {:x :y :h :w}, regresa los puntos [[x1 y1] [x2 y2]] donde
   una flecha debe entrar/salir de cada una para apuntar a la otra."
  [{x1 :x y1 :y h1 :h w1 :w}
   {x2 :x y2 :y h2 :h w2 :w}]
  (let [alfa1 (/ h1 w1)
        alfa2 (/ h2 w2)
        x1 (+ x1 (/ w1 2))
        y1 (+ y1 (/ h1 1.6666))
        x2 (+ x2 (/ w2 2))
        y2 (+ y2 (/ h2 1.6666))
        m (if (not= x2 x1)
            (/ (- y2 y1) (- x2 x1)))
        w1' (/ w1 2)
        h1' (/ h1 1.6666)
        w2' (/ w2 2)
        h2' (/ h2 1.6666)

        [x1' y1'] (cond
                    (> x2 x1)
                    (if (<= m 0)
                      (if (<= (Math/abs m) alfa1)
                        [(+ x1 w1') (+ y1 (* w1' m))]
                        [(- x1 (/ h1' m)) (- y1 h1')])
                      (if (<= (Math/abs m) alfa1)
                        [(+ x1 w1') (+ y1 (* w1' m))]
                        [(+ x1 (/ h1' m)) (+ y1 h1')]))
                    (< x2 x1)
                    (if (<= m 0)
                      (if (<= (Math/abs m) alfa1)
                        [(- x1 w1') (- y1 (* w1' m))]
                        [(+ x1 (/ h1' m)) (+ y1 h1')])
                      (if (<= (Math/abs m) alfa1)
                        [(- x1 w1') (- y1 (* w1' m))]
                        [(- x1 (/ h1' m)) (- y1 h1')]))
                    (= x2 x1)
                    (if (>= y1 y2)
                      [x1 (- y1 h1')]
                      [x1 (+ y1 h1')]))
        [x2' y2'] (cond
                    (> x1 x2)
                    (if (<= m 0)
                      (if (<= (Math/abs m) alfa2)
                        [(+ x2 w2') (+ y2 (* w2' m))]
                        [(- x2 (/ h2' m)) (- y2 h2')])
                      (if (<= (Math/abs m) alfa2)
                        [(+ x2 w2') (+ y2 (* w2' m))]
                        [(+ x2 (/ h2' m)) (+ y2 h2')]))
                    (< x1 x2)
                    (if (<= m 0)
                      (if (<= (Math/abs m) alfa2)
                        [(- x2 w2') (- y2 (* w2' m))]
                        [(+ x2 (/ h2' m)) (+ y2 h2')])
                      (if (<= (Math/abs m) alfa2)
                        [(- x2 w2') (- y2 (* w2' m))]
                        [(- x2 (/ h2' m)) (- y2 h2')]))
                    (= x1 x2)
                    (if (>= y2 y1)
                      [x2 (- y2 h2')]
                      [x2 (+ y2 h2')]))]
    [[x1' y1'] [x2' y2']]))

(defn compute-exit2point
  "Punto donde una flecha debe salir de la caja {:x :y :h :w} para apuntar a
   un punto suelto [x2 y2] (usado mientras se esta arrastrando un conector
   nuevo, antes de soltarlo sobre otro estado)."
  [{x1 :x y1 :y h1 :h w1 :w} [x2 y2]]
  (let [alfa1 (/ h1 w1)
        x1 (+ x1 (/ w1 2))
        y1 (+ y1 (/ h1 2))
        m (if (not= x2 x1)
            (/ (- y2 y1) (- x2 x1)))
        w1' (/ w1 2)
        h1' (/ h1 2)]
    (cond
      (> x2 x1)
      (if (<= m 0)
        (if (<= (Math/abs m) alfa1)
          [(+ x1 w1') (+ y1 (* w1' m))]
          [(- x1 (/ h1' m)) (- y1 h1')])
        (if (<= (Math/abs m) alfa1)
          [(+ x1 w1') (+ y1 (* w1' m))]
          [(+ x1 (/ h1' m)) (+ y1 h1')]))
      (< x2 x1)
      (if (<= m 0)
        (if (<= (Math/abs m) alfa1)
          [(- x1 w1') (- y1 (* w1' m))]
          [(+ x1 (/ h1' m)) (+ y1 h1')])
        (if (<= (Math/abs m) alfa1)
          [(- x1 w1') (- y1 (* w1' m))]
          [(- x1 (/ h1' m)) (- y1 h1')]))
      (= x2 x1)
      (if (>= y1 y2)
        [x1 (- y1 h1')]
        [x1 (+ y1 h1')]))))

(defn connect2points
  "Trazo en forma de Z (o de S) entre dos puntos, alternando un tramo recto en
   el eje dominante y un tramo en el eje perpendicular."
  [[x1 y1] [x2 y2]]
  (let [deltaX (- x2 x1)
        deltaY (- y2 y1)
        by-x? (> (Math/abs deltaX) (Math/abs deltaY))
        deltas (if by-x?
                 [[(/ deltaX 2) 0] [0 deltaY] [(/ deltaX 2) 0]]
                 [[0 (/ deltaY 2)] [deltaX 0] [0 (/ deltaY 2)]])]
    (second
      (reduce
        (fn [[[x y] ps] [dx dy]]
          (let [x1 (+ x dx)
                y1 (+ y dy)]
            [[x1 y1] (conj ps [x1 y1])]))
        [[x1 y1] []]
        deltas))))

(defn connecting-path
  "Path SVG (atributo `d`) de una linea quebrada entre dos puntos."
  [[x1 y1] [x2 y2]]
  (let [pts (connect2points [x1 y1] [x2 y2])
        pts (flatten (concat [x1 y1] pts))]
    (apply gstring/format "M%d,%dL%d,%dL%d,%dL%d,%d" pts)))

(defn position&rotate
  "Atributo `transform` para posicionar y rotar la punta de flecha en (x2,y2)
   apuntando en la direccion de (x1,y1) -> (x2,y2)."
  [x1 y1 x2 y2]
  (let [m (/ (- y2 y1) (- x2 x1))
        rot-init (if (> x1 x2) 270 90)
        rotation (- (* (.atan js/Math m) (/ 180 3.1416)) rot-init)]
    (gstring/format "translate(%d %d) rotate(%d 0 0)" x2 y2 rotation)))
