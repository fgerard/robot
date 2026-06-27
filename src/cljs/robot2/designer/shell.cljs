(ns robot2.designer.shell
  "Chrome de la pestana Designer: selector de app, toolbar (nueva app,
   import/export, undo/redo, save), panel de parametros + instancias, y el
   canvas. Antes era el `defmethod main-tab-panel :designer` de 250 lineas
   dentro de robot.ui.robot-control, junto con dos pantallas mas sin relacion."
  (:require [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [re-com.core :as re-com]
            [robot2.undo :as undo]
            [robot2.widgets :as widgets]
            [robot2.operations.dialog :as opr-dialog]
            [robot2.canvas.render :as canvas]
            [robot2.designer.params :as params]
            [robot2.apps.stored :as apps-stored]
            [robot2.apps.loaded :as apps-loaded]))

(re-frame/reg-sub :applications/editable (fn [db _] (get-in db [:applications :editable])))
(re-frame/reg-sub :designer/ctrl (fn [db _] (:designer/ctrl db)))

(re-frame/reg-event-db
  :designer/create-app!
  [undo/track]
  (fn [db [_ app-name]]
    (-> db
        (assoc-in [:applications :editable app-name]
                  {:app-params {} :instances {} :init-state nil :states {}})
        (assoc-in [:designer/ctrl :app] app-name))))

(re-frame/reg-event-db
  :designer/create-inst!
  [undo/track]
  (fn [db [_ app-id inst-name]]
    (assoc-in db [:applications :editable app-id :instances inst-name] {})))

(defn- import-dialog [close!]
  (let [name-atm (reagent/atom "")
        file-atm (reagent/atom "Please select a file")]
    (fn [close!]
      [re-com/modal-panel
       :backdrop-color "grey" :backdrop-opacity 0.4 :wrap-nicely? false
       :child
       [re-com/border
        :border "5px solid #559" :radius "25px"
        :child
        [re-com/v-box
         :padding "10px" :style {:background-color "#d4e3f7" :border-radius "19px"}
         :children
         [[re-com/title :label "Import application" :level :level2]
          [re-com/h-box
           :children [[re-com/title :label "Application name:" :level :level3]
                      [re-com/info-button :info "Name of the application to be imported, it must be an alphanumeric and does not contain blank spaces"]]]
          [re-com/input-text
           :model name-atm :width "13em" :placeholder "Application" :change-on-blur? false
           :status (widgets/name-status @name-atm)
           :on-change (fn [txt] (reset! name-atm txt))]
          [re-com/h-box
           :children [[re-com/title :label "File:" :level :level3]
                      [re-com/info-button :info "Edn file to be imported"]]]
          [re-com/h-box
           :style {:position "relative" :cursor "pointer" :margin-bottom "0.5em"}
           :children
           [[:input {:type "file" :id "imported-file"
                     :style {:position "absolute" :z-index "2" :opacity "0" :width "100%" :height "100%" :cursor "pointer"}
                     :onChange (fn [_]
                                 (let [files (.-files (.getElementById js/document "imported-file"))
                                       first-file (when files (aget files 0))]
                                   (when first-file (reset! file-atm (.-name first-file)))))}]
            [re-com/md-icon-button :md-icon-name "zmdi-folder" :size :larger :tooltip "Select file"]
            [re-com/gap :size "0.5em"]
            [re-com/title :label @file-atm]]]
          [re-com/h-box
           :gap "30px" :justify :center
           :children
           [[re-com/button :label "Cancel" :on-click close!]
            [re-com/button
             :label "Ok" :class "btn-primary"
             :disabled? (or (nil? (seq @name-atm)) (not (widgets/valid-name? @name-atm)) (= "Please select a file" @file-atm))
             :on-click (fn []
                         (let [js-file (-> (.getElementById js/document "imported-file") .-files (aget 0))
                               reader (js/FileReader.)]
                           (set! (.-onload reader)
                                 (fn [e]
                                   (re-frame/dispatch [:api/import-app {:file (.-result (.-target e)) :app-name @name-atm}])
                                   (close!)))
                           (.readAsText reader js-file)))]]]]]]])))

(defn- toolbar [app app-names running?]
  [re-com/h-box
   :class "toolbar-container" :width "100%"
   :children
   [[:div {:class "app-container"}
     [re-com/single-dropdown
      :class "app-selector" :choices (mapv (fn [a] {:id a :label a}) (sort app-names))
      :model app :width "100%" :placeholder "Select an app"
      :on-change (fn [id] (re-frame/dispatch [:reset! [:designer/ctrl :app] id]))]]
    [re-com/gap :size "0.5em"]
    [re-com/md-icon-button
     :class "new-app-btn" :md-icon-name "zmdi-plus" :tooltip "New application"
     :on-click (fn [] (re-frame/dispatch [:reset! [:designer/ctrl :new-app] :create]))]
    [re-com/gap :class "con-gap" :size "0.5em"]
    [re-com/md-icon-button
     :md-icon-name "zmdi-cloud-upload" :class "import-btn" :tooltip "Import"
     :on-click (fn [] (re-frame/dispatch [:reset! [:designer/ctrl :import] :import]))]
    [re-com/gap :size "0.5em"]
    [re-com/md-icon-button
     :md-icon-name "zmdi-cloud-download" :class "export-btn" :tooltip "Export"
     :disabled? (nil? (seq app))
     :on-click (fn [] (.open js/window (str "/application/" app)))]
    [re-com/gap :class "con-gap" :size "0.5em"]
    [re-com/md-icon-button :md-icon-name "zmdi-undo" :class "undo-btn" :tooltip "Undo"
     :on-click (fn [] (re-frame/dispatch [:undo]))]
    [re-com/gap :size "0.5em"]
    [re-com/md-icon-button :md-icon-name "zmdi-redo" :class "redo-btn" :tooltip "Redo"
     :on-click (fn [] (re-frame/dispatch [:redo]))]
    [re-com/gap :class "con-gap" :size "0.5em"]
    [re-com/md-icon-button
     :md-icon-name "zmdi-save" :class (if running? "save-off-btn" "save-btn")
     :tooltip (if running? "Stop application before saving" "Save")
     :disabled? (or (nil? (seq app)) running?)
     :on-click (fn [] (re-frame/dispatch [:api/save-app app]))]]])

(defn designer-tab []
  (let [editable (re-frame/subscribe [:applications/editable])
        ready (re-frame/subscribe [:apps/ready])
        ctrl (re-frame/subscribe [:designer/ctrl])
        watch-instance-atm (reagent/atom nil)
        open-instances-atm (reagent/atom #{})
        new-instance-atm (reagent/atom "")]
    (fn []
      (let [editable @editable
            ready @ready
            {:keys [app new-app import]} @ctrl
            app-names (set (keys editable))
            current-state-ids (set (keys (get-in editable [app :states])))
            running? (boolean (seq (params/get-running-instances ready app)))]
        [re-com/h-box
         :width "100%" :height "85%"
         :children
         [[re-com/h-split
           :class "split-container" :initial-split "25%"
           :panel-1
           [re-com/v-box
            :class "vertical-box" :width "100%"
            :children
            [[re-com/h-box
              :children [[apps-stored/apps-stored-com] [re-com/gap :size "1em"] [apps-loaded/apps-loaded-com]]]
             [toolbar app app-names running?]
             (when (seq app)
               [params/app-params-panel app editable ready watch-instance-atm open-instances-atm new-instance-atm])]]
           :panel-2
           [re-com/h-split
            :class "internal-split" :initial-split "80%"
            :panel-1
            [re-com/v-box
             :width "100%"
             :children [[opr-dialog/operations-toolbar app current-state-ids]
                        [canvas/svg-comp editable ready "100%" "100%" app @watch-instance-atm]]]
            :panel-2
            (if (seq @watch-instance-atm)
              [re-com/scroller
               :h-scroll :auto :v-scroll :auto :width "100%"
               :child [widgets/edit-params (get-in ready [app @watch-instance-atm]) nil "100%" false]]
              [re-com/title :label "Please select an instance to watch!"])]]
          (when (= :create new-app)
            [re-com/modal-panel
             :backdrop-color "grey" :backdrop-opacity 0.4 :wrap-nicely? false
             :backdrop-on-click (fn [] (re-frame/dispatch [:reset! [:designer/ctrl :new-app] nil]))
             :child [widgets/dialog-new
                     {:type "application" :taken-names app-names
                      :on-create [:designer/create-app!]
                      :on-cancel (fn [] (re-frame/dispatch [:reset! [:designer/ctrl :new-app] nil]))}]])
          (when import
            [import-dialog (fn [] (re-frame/dispatch [:reset! [:designer/ctrl :import] nil]))])]]))))
