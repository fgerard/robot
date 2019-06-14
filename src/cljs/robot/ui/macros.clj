(ns robot.ui.macros)

(defmacro sfn [& fun]
  (let [mutator (concat '(mutator) fun)
        letfn-mut [mutator]
        letfn-mut (concat '(letfn) `(~letfn-mut))
        code (concat letfn-mut '((snapshot-event (mutator db e) e)))
        code (concat '(fn [db e]) `(~code))]
        ;code (concat code '((mutator (snapshot-event db e) e)))]
    code))

(comment
 (fn [db e]
  (letfn [(mutator [db [_ ruta x y]]
            (let [canvas (conj (vec (-> ruta butlast butlast)) :connecting)]
              (let [result (assoc-in db canvas {:elem (last ruta), :x x, :y y})]
                result)))]
    (snapshot-event (mutator db e) e)))

 (fn [db e] (letfn [(mutator [db [_ ruta x y]]
                     (let [canvas (conj (vec (-> ruta butlast butlast)) :connecting)]
                       (let [result (assoc-in db canvas {:elem (last ruta), :x x, :y y})]
                         result)))]
             (snapshot-event (mutator db e) e))))
