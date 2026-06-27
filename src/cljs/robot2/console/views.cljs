(ns robot2.console.views
  "Dashboard de la pestana Console: todas las apps/instancias 'ready', con
   filtros de nombre/estado/mood y botones de start/stop. Antes era
   `inst-control` dentro del namespace-dios robot.ui.robot-control."
  (:require [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [re-com.core :as re-com]
            [robot2.widgets :as widgets]))

(defn instance-mood [mood]
  (cond
    (keyword? mood) mood
    (empty? mood) :unknown
    (some #(= % :bad) (map (comp first second) mood)) :bad
    :else :good))

(defn- calc-inst-summary [insts]
  (reduce-kv (fn [[stopped good bad] _inst-name {:robot/keys [status mood]}]
               (if (= :running status)
                 (let [mood (instance-mood mood)]
                   [(dec stopped) (if (= mood :good) (inc good) good) (if (= mood :bad) (inc bad) bad)])
                 [stopped good bad]))
             [(count insts) 0 0]
             insts))

(defn- summary-badge [n background]
  [:div.summary-container [:div.summary {:style {:background background}} (str n)]])

(defn- app-badges [summaries app]
  (let [[running good bad] (get summaries app)]
    [(summary-badge bad (if (zero? bad) "white" "red"))
     (summary-badge good (if (zero? good) "white" "green"))
     (summary-badge running (if (zero? running) "white" "#999"))]))

(defn- instance-row [app inst status]
  (let [{:robot/keys [status mood current] :or {status :stopped mood {} current :none}} status
        mood (instance-mood mood)
        bad? (= :bad mood)
        action-button (case status
                         :running [widgets/md-button "zmdi zmdi-settings zmdi-hc-spin btn-cursor btn-console" [:api/stop app inst]]
                         :stopped [widgets/md-button "zmdi zmdi-settings-off btn-cursor btn-console" [:api/start app inst]]
                         :transitioning [widgets/md-button "zmdi zmdi-settings mdc-text-grey btn-cursor btn-console"])
        mood-button (cond
                      (and (= status :running) bad?) [widgets/md-button "zmdi zmdi-thumb-down mdc-text-red"]
                      (and (= status :running) (= mood :good)) [widgets/md-button "zmdi zmdi-thumb-up mdc-text-green"]
                      :else [widgets/md-button "zmdi zmdi-more"])]
    ^{:key (str app inst)}
    [re-com/h-box
     :width "100%"
     :children [[re-com/gap :size "335px"]
                [:div {:class (str "instance-name-container" (when bad? "-bad"))}
                 [re-com/label :label inst :class (str "instance-name" (when bad? "-bad"))]]
                [:span {:class "btn-container"} action-button mood-button]
                [re-com/label :label (if (= current "none") "-" current) :width "auto"
                 :class (str "current-state" (when bad? "-bad"))]]]))

(defn- app-row [app insts open-instances-atm app-open-fltr badges bad?]
  ^{:key (str app)}
  [re-com/h-box
   :width "688px"
   :class (str "apps-container" (when bad? "-bad"))
   :children
   [[re-com/v-box
     :class "apps-rows" :width "100%"
     :children
     [[re-com/h-box
       :width "100%" :class "app-container"
       :children [[re-com/h-box :class "console-status" :children badges]
                  [re-com/gap :size "5px"]
                  [widgets/md-open-close-button
                   "zmdi zmdi-caret-right btn-cursor" "zmdi zmdi-caret-down btn-cursor"
                   insts open-instances-atm]
                  [re-com/label :label app :class "app-name" :width "12em"]
                  [:div.separator-0] [:div.separator-1]
                  [re-com/gap :size "7.5rem"]]]
      (when (@app-open-fltr app)
        (doall (for [[inst status] insts] (instance-row app inst status))))]]]])

(defn inst-control []
  (let [ready (re-frame/subscribe [:apps/ready])
        open-instances-atm (reagent/atom #{})
        app-fltr (reagent/atom "")
        inst-fltr (reagent/atom "")
        status-fltr (reagent/atom :menu)
        mood-fltr (reagent/atom :menu)
        app-open-fltr (reagent/atom #{})]
    (fn []
      (let [ready @ready
            summaries (reduce-kv (fn [r app insts] (assoc r app (calc-inst-summary insts))) {} ready)
            bad-apps (into {} (map (fn [app] [app (nth (get summaries app) 2)]) (keys ready)))
            insts (->> (reduce-kv (fn [result app instances]
                                     (concat result (map (fn [[inst status]] [app inst status]) instances)))
                                   []
                                   ready)
                       sort
                       (filter (fn [[app inst {:robot/keys [status mood]}]]
                                 (and (or (empty? @app-fltr) (re-find (re-pattern @app-fltr) app))
                                      (or (empty? @inst-fltr) (re-find (re-pattern @inst-fltr) inst))
                                      (or (= :menu @status-fltr) (= status @status-fltr))
                                      (or (= :menu @mood-fltr) (= (instance-mood mood) @mood-fltr))))))
            apps (reduce (fn [result [app inst status]] (update result app conj [inst status]))
                         (sorted-map)
                         insts)]
        [re-com/scroller
         :v-scroll :auto
         :style {:margin-bottom "8%" :border "0px"}
         :child
         [re-com/v-box
          :width "100%" :class "console-container"
          :children
          [[re-com/h-box
            :class "header-bar"
            :children [[re-com/title :class "console-header status" :label "STATUS"]
                       [re-com/title :class "console-header applications" :label "APPLICATIONS"]
                       [re-com/title :class "console-header instances" :label "INSTANCES"]
                       [re-com/h-box :class "console-header icons"
                        :children [[widgets/md-cycle-button
                                    ["zmdi zmdi-menu" "zmdi zmdi-settings" "zmdi zmdi-settings zmdi-hc-spin btn-cursor"]
                                    [:menu :stopped :running] status-fltr]
                                   [widgets/md-cycle-button
                                    ["zmdi zmdi-menu" "zmdi zmdi-thumb-down mdc-text-red" "zmdi zmdi-thumb-up mdc-text-green" "zmdi zmdi-more"]
                                    [:menu :bad :good :unknown] mood-fltr]]]
                       [re-com/title :class "console-header state" :label "STATE"]]]
           [re-com/h-box
            :width "100%"
            :children [[re-com/gap :size "151px"]
                       [re-com/input-text
                        :placeholder "App name" :class "app-name-filter" :width "auto"
                        :model @app-fltr :change-on-blur? false
                        :on-change (fn [txt] (reset! app-fltr txt))]
                       [re-com/gap :size "1px"]
                       [re-com/input-text
                        :placeholder "Instance name" :class "app-instance-filter" :width "auto"
                        :model @inst-fltr :change-on-blur? false
                        :on-change (fn [txt] (reset! inst-fltr txt))]]]
           (doall
             (for [[app insts] apps]
               (app-row app insts open-instances-atm app-open-fltr (app-badges summaries app) (pos? (bad-apps app)))))
           [:div {:style {:border-top "1px solid #EEEEEE"}} ""]]]]))))
