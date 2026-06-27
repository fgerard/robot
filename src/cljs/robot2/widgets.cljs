(ns robot2.widgets
  "Piezas de UI compartidas entre features: botones de icono usados en varias
   pantallas, el dialogo generico \"nombre nuevo\" (crear app/instancia), el
   editor libre de parametros clave-valor, y el unico regex de nombre valido
   (antes repetido mas de 10 veces, identico, por todo robot.ui.robot-control
   y robot.ui.state-editor)."
  (:require [clojure.string :as string]
            [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [re-com.core :as re-com]
            [robot2.undo :as undo]))

(def NAME-RE #"[a-zA-Z][A-Za-z-_0-9]*")

(defn valid-name? [s] (boolean (re-matches NAME-RE s)))

(defn name-status
  "Status :success/:error/nil para un re-com input que debe contener un
   nombre valido (vacio = sin marcar, ni exito ni error)."
  [s]
  (when (seq s) (if (valid-name? s) :success :error)))

;; --- botones de icono ---------------------------------------------------------
(defn md-button
  ([class-str] (md-button class-str nil))
  ([class-str click-evt]
   (let [attrs (cond-> {:class class-str}
                 click-evt (assoc :on-click (fn [] (re-frame/dispatch click-evt))))]
     [:div {:style {:float "left" :padding-right "5px" :margin-right "5px"}}
      [:i attrs]])))

(defn md-cycle-button
  "Boton que va rotando entre `states-vec` (cada vez que se hace click) y
   muestra la clase de `class-vec` correspondiente al estado actual."
  [class-vec states-vec state-atm]
  (let [class-str (nth class-vec (.indexOf states-vec @state-atm))
        click-fn (fn [e]
                   (.preventDefault e) (.stopPropagation e)
                   (let [p (inc (.indexOf states-vec @state-atm))]
                     (reset! state-atm (nth (cycle states-vec) p))))]
    [:div {:class "btn-cursor" :style {:padding-left "5px" :padding-right "5px" :margin-right "10px"}
           :on-click click-fn}
     [:i {:class class-str}]]))

(defn md-open-close-button [open-icon close-icon element open-set-atm]
  (let [click-fn (fn []
                   (if (@open-set-atm element)
                     (swap! open-set-atm disj element)
                     (swap! open-set-atm conj element)))]
    [:div {:style {:padding-left "5px" :padding-right "5px" :margin-right "10px" :cursor "pointer"}
           :on-click click-fn}
     [:i {:class (if (@open-set-atm element) close-icon open-icon) :style {:width "100%"}}]]))

(defn md-select-one-button [selected-icon not-selected-icon element selected-atm]
  (let [click-fn (fn []
                   (if (= @selected-atm element)
                     (reset! selected-atm nil)
                     (reset! selected-atm element)))]
    [:div {:style {:padding-left "5px" :padding-right "5px" :margin-right "10px"}}
     [:i {:class (if (= @selected-atm element) selected-icon not-selected-icon) :on-click click-fn}]]))

;; --- dialogo generico "nombre nuevo" ------------------------------------------
(defn dialog-new
  "type: texto descriptivo (\"application\", \"instance\"...) para los labels.
   taken-names: set de nombres ya usados (para deshabilitar el Ok si coincide).
   on-create: evento (vector) al que se le agrega el nombre nuevo: dispatch
   (conj on-create new-name)."
  [{:keys [type taken-names on-create on-cancel]}]
  (let [name-atm (reagent/atom "")]
    (fn [{:keys [type taken-names on-create on-cancel]}]
      [re-com/border
       :border "5px solid #559" :radius "25px"
       :child
       [re-com/v-box
        :padding "10px" :style {:background-color "#d4e3f7" :border-radius "19px"}
        :children
        [[re-com/title :label (str "New " (string/capitalize type)) :level :level2]
         [re-com/gap :size "0.5em"]
         [re-com/h-box
          :children [[re-com/title :label "Name:" :level :level3]
                      [re-com/info-button
                       :info (str "Name of the " type " to be created, it must be an alphanumeric and does not contain blank spaces")]]]
         [re-com/input-text
          :model name-atm :width "13em" :status (name-status @name-atm)
          :placeholder (string/capitalize (str type " name")) :change-on-blur? false
          :on-change (fn [n] (reset! name-atm n))]
         [re-com/line :color "#ddd" :style {:margin "10px 0 10px"}]
         [re-com/h-box
          :gap "30px" :justify :center
          :children [[re-com/button :label "Cancel" :on-click on-cancel]
                     [re-com/button
                      :label "Ok" :class "btn-primary"
                      :disabled? (or (nil? (seq @name-atm)) (taken-names @name-atm) (not (valid-name? @name-atm)))
                      :on-click (fn []
                                  (on-cancel)
                                  (re-frame/dispatch (conj on-create @name-atm)))]]]]]])))

;; --- editor libre de parametros clave -> valor (app-params / instance-params)
(re-frame/reg-event-db
  :params/set!
  [undo/track]
  (fn [db [_ path k v]]
    (assoc-in db (concat [:applications :editable] path [(keyword k)]) v)))

(re-frame/reg-event-db
  :params/remove!
  [undo/track]
  (fn [db [_ path k]]
    (update-in db (concat [:applications :editable] path) dissoc (keyword k))))

(defn edit-params
  "params: mapa a mostrar. path: donde viven dentro de :applications/editable
   (nil = solo lectura, p.ej. viendo los parametros en runtime de una
   instancia). width/with-click controlan presentacion."
  ([params path width] (edit-params params path width true))
  ([params path width with-click]
   (let [entry (reagent/atom {:k "" :v ""})]
     (fn [params path width with-click]
       (let [{:keys [k v]} @entry
             filter-pred (if path
                           (constantly true)
                           (fn [[pk pv]] (and (re-find (re-pattern (or k "")) (str pk))
                                              (re-find (re-pattern (or v "")) (str pv)))))
             view-params (into {} (filter filter-pred params))]
         [re-com/border
          :border "1px" :radius "5px" :width width
          :child
          [re-com/v-box
           :width "100%" :class "app-param-panel"
           :children
           [[re-com/h-box
             :width "100%" :class "add-panel"
             :children
             [[re-com/input-text
               :placeholder "Name" :model k :width "25%" :change-on-blur? false
               :status (name-status k)
               :on-change (fn [txt] (swap! entry assoc :k txt))]
              [re-com/input-text
               :placeholder "Value" :width "58%" :model v :change-on-blur? false
               :on-change (fn [txt]
                            (if (and (= txt (:v @entry)) (valid-name? (:k @entry)))
                              (do (re-frame/dispatch [:params/set! path @entry])
                                  (reset! entry {:k "" :v ""}))
                              (swap! entry assoc :v txt)))]
              [re-com/md-icon-button
               :md-icon-name "zmdi-close-circle-o" :tooltip "Clear boxes"
               :on-click (fn [] (reset! entry {:k "" :v ""}))]
              (when path
                [re-com/md-icon-button
                 :md-icon-name "zmdi-plus-circle-o" :tooltip "Add parameter"
                 :disabled? (or (not (valid-name? k)) (nil? (seq k)) (nil? (seq v)))
                 :on-click (fn []
                             (re-frame/dispatch [:params/set! path @entry])
                             (reset! entry {:k "" :v ""}))])]]
            [re-com/scroller
             :v-scroll :on :height "7em"
             :child
             [re-com/v-box
              :class "param-list"
              :children
              (doall
                (for [[pk pv] (sort (into [] view-params))]
                  ^{:key (str pk)}
                  [re-com/h-box
                   :style {:cursor "pointer"}
                   :attr {:on-click (fn [_] (when with-click (reset! entry {:k (subs (str pk) 1) :v (str pv)})))}
                   :children
                   [[re-com/label :label (str pk) :width "30%" :class "app-param-key"]
                    (cond
                      (re-find #"[Pp][Aa][Ss][Ss]|[Pp][Ww][Dd]|[Cc][Ll][Aa][Vv][Ee]" (str pk))
                      [re-com/label :label "*****" :width "60%"]
                      :else
                      [re-com/label :label (str pv) :width "60%"])
                    (when path
                      [re-com/md-icon-button
                       :md-icon-name "zmdi-delete" :tooltip "Delete parameter" :size :smaller
                       :on-click (fn [e]
                                   (.preventDefault e) (.stopPropagation e)
                                   (re-frame/dispatch [:params/remove! path pk]))])]]))]]]]])))))
