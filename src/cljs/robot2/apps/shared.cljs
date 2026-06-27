(ns robot2.apps.shared
  "Lista de seleccion de un solo elemento, reusada por loaded/stored/ready
   para elegir una app de su respectiva coleccion. Antes
   (robot.ui.apps-loaded/-stored/-ready) cada uno copiaba el mismo bloque de
   ~30 lineas de sub + selection-list, cambiando solo el titulo y el endpoint."
  (:require [re-frame.core :as re-frame]
            [re-com.core :as re-com]))

(defn single-select-list
  "items: coleccion de nombres (string). selected: set de 0 o 1 elemento
   seleccionado (asi es como re-com/selection-list maneja single-select).
   on-change-event: vector de evento al que se le agrega `selection` al
   dispatchar (uno de los nombres seleccionados, o vacio si se deselecciono)."
  [{:keys [title items selected on-change-event]}]
  (let [choices (mapv (fn [name] {:label name}) (sort items))]
    [re-com/v-box
     :width "100%"
     :children
     [[re-com/title :label title :level :level2]
      [re-com/box
       :height "100%"
       :child
       [re-com/selection-list
        :choices choices
        :model (or selected #{})
        :id-fn :label
        :height "150px"
        :multi-select? false
        :on-change (fn [selection] (re-frame/dispatch (conj on-change-event selection)))]]]]))
