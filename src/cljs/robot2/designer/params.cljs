(ns robot2.designer.params
  "Edicion de los parametros de una app y de sus instancias dentro del
   designer. Antes vivia mezclado en robot.ui.robot-control junto con la
   consola y el panel de usuarios."
  (:require [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [re-com.core :as re-com]
            [robot2.widgets :as widgets]
            [robot2.console.views :refer [instance-mood]]))

(defn instance-params [app-id watch-instance-atm open-instances-atm inst-id inst-params
                        {:robot/keys [status mood current] :or {status :stopped} :as running}]
  (let [mood (instance-mood mood)
        bad? (= :bad mood)]
    [re-com/v-box
     :width "100%" :class "instance"
     :children
     [[re-com/h-box
       :class "instance-main"
       :children
       [[widgets/md-select-one-button "zmdi zmdi-eye mdc-text-green btn-cursor" "zmdi zmdi-eye-off mdc-text-grey btn-cursor" inst-id watch-instance-atm]
        [widgets/md-open-close-button "zmdi zmdi-caret-right btn-cursor" "zmdi zmdi-caret-down btn-cursor" inst-id open-instances-atm]
        [re-com/label :width "50%" :label inst-id]
        [re-com/gap :size "1em"]
        (case status
          :running [widgets/md-button "zmdi zmdi-settings zmdi-hc-spin btn-cursor btn-console" [:api/stop app-id inst-id]]
          :stopped [widgets/md-button "zmdi zmdi-settings-off btn-cursor btn-console" [:api/start app-id inst-id]]
          :transitioning [widgets/md-button "zmdi zmdi-settings mdc-text-grey btn-cursor btn-console"])
        (cond
          (and (= status :running) bad?) [widgets/md-button "zmdi zmdi-thumb-down mdc-text-red"]
          (and (= status :running) (= mood :good)) [widgets/md-button "zmdi zmdi-thumb-up mdc-text-green"]
          :else [widgets/md-button "zmdi zmdi-more"])
        [re-com/label :width "25%" :label current :class "instance-label"]
        [re-com/md-icon-button
         :md-icon-name "zmdi-close-circle-o" :class "eraser-btn"
         :tooltip "Delete instance"
         :on-click (fn [] (re-frame/dispatch [:api/remove-inst app-id inst-id]))]]]
      (when (@open-instances-atm inst-id)
        [re-com/h-box
         :width "100%"
         :children [[re-com/gap :size "2em"]
                    [widgets/edit-params inst-params [app-id :instances inst-id] "92%" "7em" true]]])]]))

(defn get-running-instances [ready app-name]
  (filter #(= :running (:robot/status %)) (vals (get ready app-name))))

(defn app-params-panel [app editable ready watch-instance-atm open-instances-atm new-instance-atm]
  (let [regex-inst widgets/NAME-RE
        inst-ids (keys (get-in editable [app :instances]))
        ;; sin entrada en ready es :stopped, igual que en instance-params. Las
        ;; :transitioning no caen en ninguna de las dos: no hay que picarlas.
        status-of (fn [inst] (get-in ready [app inst :robot/status] :stopped))
        running (filter #(= :running (status-of %)) inst-ids)
        stopped (filter #(= :stopped (status-of %)) inst-ids)]
    ;; v-split entre Application Parameters (arriba) y Application Instances
    ;; (abajo) -- :initial-split 50 arranca a la mitad, y la manija entre
    ;; los dos paneles se puede arrastrar con el mouse para darle mas
    ;; espacio a uno u otro.
    [re-com/v-split
     :height "100%" :width "100%" :initial-split 50
     :panel-1
     [re-com/v-box
      :height "100%" :width "100%"
      :children
      [[re-com/h-box
        :class "instance-main"
        :children [[re-com/title :label "Application Parameters" :class "title"]
                   [re-com/gap :size "5em"]
                   [re-com/label :width "25%" :label app :class "instance-label"]
                   [re-com/gap :size "0.5em"]
                   [re-com/md-icon-button
                    :md-icon-name "zmdi-close-circle-o" :class "eraser-btn"
                    :tooltip "Delete application"
                    :on-click (fn [] (re-frame/dispatch [:api/remove-app app]))]]]
       [re-com/scroller
        :h-scroll :off :v-scroll :auto :size "1"
        :child
        [widgets/edit-params (get-in editable [app :app-params]) [app :app-params] "100%" "100%" true]]]]
     :panel-2
     [re-com/v-box
      :height "100%" :width "100%"
      :children
      [[re-com/h-box
        :class "instance-main"
        :children
        [[re-com/title :label "Application Instances" :class "title"]
         [re-com/gap :size "1em"]
         [re-com/md-icon-button
          :md-icon-name "zmdi-play-circle-outline" :class "add-btn"
          :tooltip "Start all instances"
          :disabled? (empty? stopped)
          :on-click (fn [] (doseq [inst stopped]
                             (re-frame/dispatch [:api/start app inst])))]
         [re-com/md-icon-button
          :md-icon-name "zmdi-stop" :class "add-btn"
          :tooltip "Stop all instances"
          :disabled? (empty? running)
          :on-click (fn [] (doseq [inst running]
                             (re-frame/dispatch [:api/stop app inst])))]]]
       [re-com/h-box
        :class "add-instance"
        :children
        [[re-com/input-text
          :placeholder "Instance Name" :width "87%" :model new-instance-atm :change-on-blur? false
          :status (widgets/name-status @new-instance-atm)
          :on-change (fn [name]
                       (if (and (= name @new-instance-atm) (re-matches regex-inst @new-instance-atm))
                         (re-frame/dispatch [:designer/create-inst! app @new-instance-atm])
                         (reset! new-instance-atm name)))]
         [re-com/md-icon-button
          :md-icon-name "zmdi-plus" :class "add-btn" :tooltip "New instance"
          :on-click (fn []
                      (when (re-matches regex-inst @new-instance-atm)
                        (re-frame/dispatch [:designer/create-inst! app @new-instance-atm])))]]]
       [re-com/scroller
        :h-scroll :off :v-scroll :auto :size "1"
        :child
        [re-com/v-box
         :style {:margin-bottom "20px"}
         :children
         (doall
           (for [[inst-id inst-params] (sort (get-in editable [app :instances]))]
             ^{:key (str inst-id)}
             [instance-params app watch-instance-atm open-instances-atm inst-id inst-params
              (get-in ready [app inst-id])]))]]]]]))
