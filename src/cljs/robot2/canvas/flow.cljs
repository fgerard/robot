(ns robot2.canvas.flow
  "Edicion pura del grafo de transiciones (:flow) de un estado: quitar un
   destino, reconectar una flecha a otro estado, eliminar un estado y arreglar
   las flechas que apuntaban a el. Nada de esto toca re-frame ni el DOM, por
   lo que es trivial de probar con cljs.test.")

(defn remove-flow-to
  "Quita de `flow` cualquier transicion hacia `id`. Si el ultimo elemento que
   queda tenia una regex condicional, se vuelve el default (sin regex) porque
   ya no puede haber un elemento despues de el."
  [flow id]
  (let [flow (remove (fn [[state]] (= state id)) flow)
        [last-id re] (last flow)]
    (if re
      (conj (vec (butlast flow)) [last-id])
      (vec flow))))

(defn reconnect-flow
  "Mueve la transicion que apuntaba a `old-state` para que apunte a
   `new-state`, preservando su regex. Si `new-state` ya es destino de otra
   transicion del mismo estado, no se permite (un estado no puede tener dos
   flechas hacia el mismo destino) y se regresa `flow` sin cambios."
  [flow old-state new-state]
  (let [existing-destinations (into #{} (map first flow))]
    (if (existing-destinations new-state)
      flow
      (mapv (fn [[state re]]
              (let [dest (if (= state old-state) new-state state)]
                (if re [dest re] [dest])))
            flow))))

(defn add-connection
  "Agrega una transicion nueva hacia `id` al final de `flow`. Las
   transiciones previas sin regex se marcan como \"undefined\" porque ya no
   son la ultima (y solo la ultima puede ser el default implicito)."
  [flow id]
  (conj (mapv (fn [[s re]] (if re [s re] [s "undefined"])) flow)
        [id]))

(defn fix-flow
  "Quita transiciones hacia `id` de un solo `flow` (usado al borrar un
   estado, vea `delete-state`)."
  [flow id]
  (let [new-flow (vec (remove (fn [[other-id]] (= other-id id)) flow))]
    (if (and (seq new-flow) (= 2 (count (last new-flow))))
      (conj (vec (butlast new-flow)) [(first (last new-flow))])
      new-flow)))

(defn delete-state
  "Quita `id` del mapa de estados y arregla el :flow de todos los demas
   estados que le apuntaban."
  [states id]
  (reduce
    (fn [new-states [state-id state-conf]]
      (if (= state-id id)
        new-states
        (assoc new-states state-id (update state-conf :flow fix-flow id))))
    {}
    states))
