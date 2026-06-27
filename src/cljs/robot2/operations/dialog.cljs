(ns robot2.operations.dialog
  "Dialogo de configuracion de una operacion (clic en el engrane de un
   estado) y dialogo de edicion de la regex de una transicion (clic en la
   etiqueta de una flecha). Antes vivian en robot.ui.state-editor junto con
   las 500+ lineas del catalogo de operaciones; ahora el catalogo es un dato
   cargado en runtime (robot2.operations.registry) y este archivo solo se
   encarga del render del formulario."
  (:require [clojure.string :as string]
            [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [re-com.core :as re-com]
            [cljs.reader :as reader]
            [robot2.undo :as undo]
            [robot2.widgets :as widgets]
            [robot2.operations.registry :as registry]
            [cljsjs.codemirror]
            [cljsjs.codemirror.mode.javascript]
            [cljsjs.codemirror.mode.clojure]
            cljsjs.codemirror.addon.edit.closebrackets
            cljsjs.codemirror.addon.edit.matchbrackets))

(def KEYWORD-RE #":[a-zA-ZñÑ][a-zA-ZñÑ0-9\-_]*")

;; --- eventos: mutan el diagrama, se trackean para undo --------------------
(re-frame/reg-event-db
  :canvas/add-state!
  [undo/track]
  (fn [db [_ app-id state-id opr-id]]
    (cond-> db
      true (update-in [:applications :editable app-id :states] assoc state-id
                       {:operation opr-id :conf {} :diagram {:corner [100 100]} :flow []})
      (nil? (get-in db [:applications :editable app-id :init-state]))
      (assoc-in [:applications :editable app-id :init-state] state-id))))

(re-frame/reg-event-db
  :canvas/update-conf!
  [undo/track]
  (fn [db [_ app-id state-id conf]]
    (assoc-in db [:applications :editable app-id :states state-id :conf] conf)))

(re-frame/reg-event-db
  :canvas/init-state!
  [undo/track]
  (fn [db [_ app-id state-id]]
    (assoc-in db [:applications :editable app-id :init-state] state-id)))

(re-frame/reg-event-db
  :canvas/change-opr-type!
  [undo/track]
  (fn [db [_ app-id state-id opr-type]]
    (update-in db [:applications :editable app-id :states state-id] assoc :operation opr-type)))

;; --- validacion --------------------------------------------------------------
(defn- valid-conf? [flds conf]
  (every? (fn [{:keys [kwd re type]}]
            (if (not= type :params)
              (or (re-matches re (get conf kwd ""))
                  (re-matches KEYWORD-RE (get conf kwd "")))
              true))
          flds))

;; --- editor de codigo (CodeMirror) -------------------------------------------
(defn- code-editor [{:keys [value mode onchange]}]
  (reagent/create-class
    {:display-name "code-editor"
     :component-did-mount
     (fn [this]
       (let [dom (reagent/dom-node this)
             cm (js/CodeMirror dom (clj->js {:value value :mode mode :tabMode "indent"
                                              :autoCloseBrackets true :matchBrackets true
                                              :lineNumbers true}))]
         (.setSize cm 700 300)
         (when onchange (.on cm "change" onchange))))
     :reagent-render (fn [_] [:div])}))

;; --- editor de parametros libres (mapa nombre -> valor) ----------------------
(defn- edit-params-comp [kwd conf-atm]
  (let [entry (reagent/atom {:k "" :v ""})]
    (fn [kwd conf-atm]
      (swap! conf-atm update kwd (fn [v] (cond (string? v) (reader/read-string v) (some? v) v :else {})))
      (let [{:keys [k v]} @entry
            params (get @conf-atm kwd {})
            valid-name? (or (widgets/valid-name? k) (re-matches KEYWORD-RE k))]
        [re-com/border
         :border "1px" :radius "5px" :width "100%" :height "10em"
         :style {:border "1px solid lightgrey" :margin-top "0.5em"}
         :child
         [re-com/v-box
          :width "100%"
          :children
          [[re-com/h-box
            :width "100%"
            :children [[re-com/input-text
                        :placeholder "Param name" :width "30%" :model k :change-on-blur? false
                        :status (when (seq k) (if valid-name? :success :error))
                        :on-change (fn [txt] (swap! entry assoc :k txt))]
                       [re-com/input-text
                        :placeholder "Value" :width "57%" :model v :change-on-blur? false
                        :on-change (fn [txt] (swap! entry assoc :v txt))]
                       [re-com/md-icon-button
                        :md-icon-name "zmdi-plus-circle-o" :tooltip "Add parameter"
                        :disabled? (or (not valid-name?) (nil? (seq k)) (nil? (seq v)))
                        :on-click (fn []
                                    (reset! entry {:k "" :v ""})
                                    (swap! conf-atm assoc-in [kwd k] v))]
                       [re-com/md-icon-button
                        :md-icon-name "zmdi-close-circle-o" :tooltip "Clear boxes"
                        :on-click (fn [] (reset! entry {:k "" :v ""}))]]]
           [re-com/scroller
            :v-scroll :on :height "7em"
            :child [re-com/v-box
                    :children (doall
                                (for [[pk pv] (sort (into [] params))]
                                  ^{:key (str pk)}
                                  [re-com/h-box
                                   :children [[re-com/label :label pk :width "30%"]
                                              [re-com/label :label pv :width "60%"]
                                              [re-com/md-circle-icon-button
                                               :md-icon-name "zmdi-delete" :size :smaller
                                               :on-click (fn [] (swap! conf-atm update kwd dissoc pk))]]]))]]]]]))))

(defn- field-input [{:keys [kwd re label info placeholder type mode]} conf-atm]
  [:div
   [re-com/h-box :children [[:label label] [re-com/gap :size "0.5em"] [re-com/info-button :info info]]]
   (case type
     :code-editor [code-editor {:mode mode
                                 :value (kwd @conf-atm (if (= mode "javascript")
                                                          "function(ctx) {\n return true;\n}"
                                                          "(fn [ctx]\n true)"))
                                 :onchange (fn [e] (swap! conf-atm assoc kwd (.getValue e)))}]
     :params [edit-params-comp kwd conf-atm]
     [re-com/input-text
      :model (kwd @conf-atm "") :width "100%" :placeholder placeholder
      :status (if (or (re-matches re (kwd @conf-atm "")) (re-matches KEYWORD-RE (kwd @conf-atm ""))) :success :error)
      :change-on-blur? false
      :on-change (fn [txt] (swap! conf-atm assoc kwd txt))])
   [re-com/gap :size "0.5em"]])

(defn- operation-form [app-id state-id init-state? {:keys [operation flds title]} conf-atm close!]
  (let [status (valid-conf? flds @conf-atm)]
    [re-com/v-box
     :width "100%"
     :children
     [[re-com/title :label (or title (string/capitalize (name operation))) :level :level3]
      [re-com/v-box
       :class "form-group" :style {:margin-bottom "1em"}
       :children
       (conj (mapv #(field-input % conf-atm) flds)
             [re-com/checkbox
              :label "Set as initial:" :model init-state?
              :on-change (fn [is-init?] (when is-init? (re-frame/dispatch [:canvas/init-state! app-id state-id])))])]
      [re-com/h-box
       :gap "30px" :justify :center
       :children [[re-com/button :label "Cancel" :on-click close!]
                  [re-com/button :label "Ok" :class "btn-primary" :disabled? (not status)
                   :on-click (fn []
                               (re-frame/dispatch [:canvas/update-conf! app-id state-id @conf-atm])
                               (close!))]]]]]))

(defn operation-modal
  "app-id, state-id: identifican el estado cuya operacion se edita.
   init-state: id del estado inicial actual de la app (para el checkbox).
   state-data: {:operation :conf :diagram :flow} del estado.
   close!: callback sin argumentos para cerrar el dialogo (estado local del
   canvas, ver robot2.canvas.interactions/close-dialog!)."
  [app-id state-id init-state {:keys [operation conf] :as state-data} close!]
  (let [schema (re-frame/subscribe [:operations/schema])
        raw-ops (re-frame/subscribe [:operations/raw])
        conf-atm (reagent/atom conf)]
    (fn [app-id state-id init-state {:keys [operation conf] :as state-data} close!]
      (let [opr-choices (registry/choices @raw-ops)
            schema-entry (registry/schema-for @schema operation)]
        [re-com/modal-panel
         :backdrop-color "grey" :backdrop-opacity 0.4 :wrap-nicely? false
         :child
         [re-com/border
          :border "5px solid #559" :radius "15px"
          :child
          [re-com/v-box
           :width "740px" :style {:background-color "#d4e3f7" :border-radius "15px" :padding "1em"}
           :children
           [[re-com/title :label (str (string/capitalize (name operation)) " operation") :level :level2]
            [re-com/title :label (str "(" state-id ")") :style {:font-weight "bold"} :level :level3]
            [re-com/title :label "Operation type: " :level :level3]
            [re-com/single-dropdown
             :choices opr-choices :model (name operation) :width "100%"
             :on-change (fn [id] (re-frame/dispatch [:canvas/change-opr-type! app-id state-id (keyword id)]))
             :render-fn (fn [{:keys [id image]}]
                          [re-com/h-box
                           :children [[:img {:src (str "images/icons/operations/" image) :width "25px" :height "25px"}]
                                      [re-com/gap :size "0.5em"] [:p id]]])]
            (if schema-entry
              [operation-form app-id state-id (= state-id init-state)
               (assoc schema-entry :operation operation) conf-atm close!]
              [re-com/title :label "Loading operation catalog..." :level :level4])]]]]))))

(defn operations-toolbar
  "Selector de tipo de operacion + nombre + boton 'agregar al canvas'. Antes
   `operations-ctrl` en robot.ui.robot-control."
  [app-id current-state-ids]
  (let [raw-ops (re-frame/subscribe [:operations/raw])
        opr-id-atm (reagent/atom nil)
        state-id-atm (reagent/atom "")]
    (fn [app-id current-state-ids]
      (let [opr-choices (registry/choices @raw-ops)
            invalid-name? (or (not (widgets/valid-name? @state-id-atm))
                               (nil? (seq @opr-id-atm))
                               (current-state-ids (keyword @state-id-atm))
                               (nil? (seq app-id))
                               (nil? (seq @state-id-atm)))
            add! (fn [] (re-frame/dispatch [:canvas/add-state! app-id (keyword @state-id-atm) (keyword @opr-id-atm)]))]
        [re-com/h-box
         :class "opr-container"
         :children
         [[re-com/single-dropdown
           :choices opr-choices :model opr-id-atm :class "opr-selector" :placeholder "Operation type" :width "30%"
           :on-change (fn [id] (reset! opr-id-atm id))
           :render-fn (fn [{:keys [id image]}]
                        [re-com/h-box
                         :children [[:img {:src (str "images/icons/operations/" image) :width "25px" :height "25px"}]
                                    [re-com/gap :size "0.5em"] [:p id]]])]
          [re-com/gap :size "0.2em"]
          [re-com/input-text
           :model state-id-atm :width "30%" :placeholder "Operation name" :change-on-blur? false
           :status (widgets/name-status @state-id-atm)
           :on-change (fn [txt]
                        (if (and (= txt @state-id-atm) (widgets/valid-name? @state-id-atm))
                          (add!)
                          (reset! state-id-atm txt)))]
          [re-com/gap :size "1em"]
          [re-com/md-icon-button
           :md-icon-name "zmdi-plus" :size :regular :class "add-btn"
           :tooltip (if invalid-name? "Select a valid name to enable creation" "Create new operation in canvas")
           :disabled? invalid-name?
           :on-click add!]]]))))

(defn flow-re-modal
  "Edicion de la regex de la transicion app-id/orig-state -> other-state.
   current-re: valor actual (string) ya resuelto por quien llama (ver
   robot2.canvas.render, que ya tiene `states` a la mano)."
  [app-id orig-state other-state current-re close!]
  (let [new-re-txt (reagent/atom current-re)]
    (fn [app-id orig-state other-state current-re close!]
      [re-com/modal-panel
       :backdrop-color "grey" :backdrop-opacity 0.4 :wrap-nicely? false
       :child
       [re-com/border
        :border "5px solid #559" :radius "25px"
        :child
        [re-com/v-box
         :padding "10px" :style {:background-color "#d4e3f7" :border-radius "19px"}
         :children
         [[re-com/title :label "Transition" :level :level2]
          [re-com/title :label (str "Rule for transition from " orig-state " to " other-state) :level :level3]
          [re-com/input-text
           :model @new-re-txt :width "100%" :placeholder "Enter regex"
           :on-change (fn [txt] (reset! new-re-txt txt))]
          [re-com/h-box
           :gap "30px" :justify :center
           :children [[re-com/button :label "Cancel" :on-click close!]
                      [re-com/button :label "Ok" :class "btn-primary"
                       :on-click (fn []
                                   (re-frame/dispatch [:canvas/update-flow-re! app-id orig-state other-state @new-re-txt])
                                   (close!))]]]]]]])))
