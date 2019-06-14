(ns robot.ui.robot-control
  (:require-macros [robot.ui.macros :refer [sfn]]
                   [cljs.core.async.macros :refer [go]])
  (:require [re-frame.core :as re-frame]
            [re-com.core :as re-com]
            [reagent.core :as reagent :refer [atom create-class]]
            [robot.ui.db :as db]
            [cljs.pprint :refer [pprint]]
            [cljs.core.async :refer [<!]]
            [cljs-http.client :as http]
            [robot.ui.apps-stored :as ui-apps-stored]
            [robot.ui.apps-loaded :as ui-apps-loaded]
            [robot.ui.apps-ready :as ui-apps-ready]
            [robot.ui.svg :as svg]
            ))

;;;;;;;;;;;;;;;;;;;; SUBSCRIPTIONS

(re-frame/reg-sub
  [:applications :editable]
  (fn [db [path]]
    (get-in db path)))

(re-frame/reg-sub
  [:operations]
  (fn [db [path]]
    (get-in db path)))



;;;;;;;;;;;;;;;;;;;; HANDLERS

(re-frame/reg-event-db
  [:applications :editable]                                 ;:application2db
  (fn [{:keys [applications] :as db} [path new-application-name new-application]]
    (re-frame/dispatch [:load-applications])
    (assoc-in db (conj path new-application-name) new-application)))

(re-frame/reg-event-db
  :create-app
  (fn [db [_ new-app-name path]]
    (-> db
        (assoc-in
          [:applications :editable new-app-name]
          {:app-params {}
           :instances  {}
           :init-state nil
           :states     {}})
        (assoc-in
          [:designer :ctrl :app] new-app-name))))

(re-frame/reg-event-db
  :create-inst
  (fn [db [_ new-inst-name path]]
    (assoc-in
      db
      (concat [:applications :editable] path [new-inst-name])
      {})))

(re-frame/reg-event-db
  :pretty-print                                 ;:application2db
  (fn [db [_]]
    (pprint db)
    db))

;;;;;;;;;;;;;;;;;;;; COMPONENTS

(defn md-button
  ([class-str]
   (md-button class-str nil))
  ([class-str click-evt]
   (let [attrs (cond-> {:class class-str}
                       click-evt (assoc :on-click (fn []
                                                    (re-frame/dispatch click-evt))))]
     [:div {:style {:float         "left"
                    :padding-left  "0px"
                    :padding-right "5px"
                    :margin-right  "5px"
                    }}
      [:i attrs]]))
  ([class-vec states-vec states-atm]
   (let [class-str (nth class-vec (.indexOf states-vec @states-atm))
         click-fn (fn [e]
                    (.preventDefault e)
                    (.stopPropagation e)
                    (let [p (inc (.indexOf states-vec @states-atm))]
                      (reset! states-atm (nth (cycle states-vec) p))))]
     [:div {:class    "btn-cursor"
            :style    {:padding-left  "5px"
                       :padding-right "5px"
                       :margin-right  "10px"
                       }
            :on-click click-fn}
      [:i {:class class-str}]])))

(defn md-open-close-button
  ([open-icon close-icon element open-elem-set-atm]
   (let [click-fn (fn []
                    (if (@open-elem-set-atm element)
                      (swap! open-elem-set-atm disj element)
                      (swap! open-elem-set-atm conj element)))]
     [md-open-close-button open-icon close-icon element open-elem-set-atm click-fn]))
  ([open-icon close-icon element open-elem-set-atm click-fn]
   (let [class-str (if (@open-elem-set-atm element) close-icon open-icon)]
     [:div {:style    {:padding-left  "5px"
                       :padding-right "5px"
                       :margin-right  "10px"
                       :cursor        "pointer"}
            :on-click click-fn}
      [:i {:class class-str
           :style {:width "100%"}}]])))

(defn md-select-one-button
  ([selected-icon not-selected-icon element selected-elem-atm]
   (let [class-str (if (= @selected-elem-atm element) selected-icon not-selected-icon)
         click-fn (fn []
                    (if (= @selected-elem-atm element)
                      (reset! selected-elem-atm nil)
                      (reset! selected-elem-atm element)))]
     [:div {:style {:padding-left  "5px"
                    :padding-right "5px"
                    :margin-right  "10px"
                    }}
      [:i {:class    class-str
           :on-click click-fn}]])))

(defn instance-mood? [mood]
  (if (keyword? mood)
    (do
      (println "LLEGO UN KEYWORD " mood)
      mood)
    (if (nil? (seq mood))
      :unknown
      (if (some #(= % :bad) (map (comp first second) mood))
        :bad
        :good))))

(defmulti main-tab-panel (fn [id] id) :default :default)

(defmethod main-tab-panel :default [id]
  (re-frame/dispatch [:pprint])
  [re-com/title :label (str "Main tab id [" id "] unknown") :level :level1])

(defn calc-inst-summary [insts]
  (let [inst-count (count (keys insts))]
    (reduce-kv (fn [[stopped running-good running-bad :as params]
                    inst-name {status :robot/status
                               mood   :robot/mood}]
                 (let [mood (instance-mood? mood)]
                   (if (= :running status)
                     [(dec stopped)
                      (if (= mood :good) (inc running-good) running-good)
                      (if (= mood :bad) (inc running-bad) running-bad)]
                     params)))
               [inst-count 0 0]
               insts)))

(defn calc-summary [apps]
  (reduce-kv (fn [summary app insts]
               (assoc summary app (calc-inst-summary insts)))
             {}
             apps))

(defn summary-image [n background]
  [:div.summary-container
   [:div.summary {:style {:background     background}}
    (str n)]])

(defn inst-control []
  (let [ready (re-frame/subscribe [[:applications :ready]])
        open-instances-atm (reagent/atom #{})
        app-fltr (reagent/atom "")
        inst-fltr (reagent/atom "")
        status-fltr (reagent/atom :menu)
        mood-fltr (reagent/atom :menu)
        app-open-fltr (reagent/atom #{})]
    (fn []
      (let [app-summary (calc-summary @ready)
            app-ready (into {}
                            (map (fn [app]
                                   (let [[running good bad] (get app-summary app)]
                                     [app [(summary-image bad (if (= 0 bad) "white" "red"))
                                           (summary-image good (if (= 0 good) "white" "green"))
                                           (summary-image running (if (= 0 running) "white" "#999"))
                                           ]]))
                                 (keys @ready)))
            bad-apps (into {}
                           (map (fn [app]
                                  (let [[running good bad] (get app-summary app)]
                                    [app bad]))
                                (keys @ready)))
            insts (vec
                    (sort
                      (reduce-kv (fn [result app instances]
                                   (concat result (map (fn [[inst status]]
                                                         [app inst status]) instances)))
                                 []
                                 @ready)))
            insts (filter (fn [[app inst {status :robot/status mood :robot/mood}]]
                            ;(println "XXXXX: " status @status-fltr mood @mood-fltr)

                            (and (or (nil? (seq @app-fltr)) (re-find (re-pattern @app-fltr) app))
                                 (or (nil? (seq @inst-fltr)) (re-find (re-pattern @inst-fltr) inst))
                                 (or (= :menu @status-fltr) (= status @status-fltr))
                                 (or (= :menu @mood-fltr) (= (instance-mood? mood) @mood-fltr)
                                     )))
                          insts)
            apps (reduce (fn [result [app inst status]]
                           (update result app conj [inst status]))
                         (sorted-map)
                         insts)]
        [re-com/scroller
         :v-scroll :auto
         :style {:margin-bottom "8%" :border "0px"}
         :child [re-com/v-box
                 :width "100%"
                 :class "console-container"
                 :children [[re-com/h-box
                             :class "header-bar"
                             :children [
                                        [re-com/title :class "console-header status" :label "STATUS"]
                                        [re-com/title :class "console-header applications" :label "APPLICATIONS"]
                                        [re-com/title :class "console-header instances" :label "INSTANCES"]
                                        [re-com/h-box :class "console-header icons"
                                         :children [(md-button ["zmdi zmdi-menu" "zmdi zmdi-settings" "zmdi zmdi-settings zmdi-hc-spin btn-cursor"]
                                                               [:menu :stopped :running]
                                                               status-fltr)
                                                    (md-button ["zmdi zmdi-menu" "zmdi zmdi-thumb-down mdc-text-red" "zmdi zmdi-thumb-up mdc-text-green" "zmdi zmdi-more"]
                                                               [:menu :bad :good :unknown]
                                                               mood-fltr)]]
                                        [re-com/title :class "console-header state" :label "STATE"]]]
                            [re-com/h-box
                             :width "100%"
                             :children [[re-com/gap :size "151px"]
                                        [re-com/input-text
                                         :placeholder "App name"
                                         :class "app-name-filter"
                                         :width "auto"
                                         :attr {:max-length "30"}
                                         :model @app-fltr
                                         :change-on-blur? false
                                         :on-change (fn [txt]
                                                      (reset! app-fltr txt))]
                                        [re-com/gap :size "1px"]
                                        [re-com/input-text
                                         :placeholder "Instance name"
                                         :class "app-instance-filter"
                                         :width "auto"
                                         :model @inst-fltr
                                         :attr {:max-length "30"}
                                         :change-on-blur? false
                                         :on-change (fn [txt]
                                                      (reset! inst-fltr txt))]
                                        ]]
                            (doall
                              (for [[app insts] apps]
                                ^{:key (str app)}
                                [re-com/h-box
                                 :width "688px"
                                 :class (str "apps-container"
                                             (if (> (bad-apps app) 0) "-bad"))
                                 :children [[re-com/v-box
                                             :class "apps-rows"
                                             :width "100%"
                                             :children [[re-com/h-box
                                                         :width "100%"
                                                         :class "app-container"
                                                         :children [
                                                                    [re-com/h-box
                                                                     :class "console-status"
                                                                     :children (concat []
                                                                                       (app-ready app))]
                                                                    [re-com/gap :size "5px"]
                                                                    [md-open-close-button 
                                                                     "zmdi zmdi-caret-right btn-cursor" 
                                                                     "zmdi zmdi-caret-down btn-cursor" 
                                                                     insts 
                                                                     open-instances-atm 
                                                                     (fn [] 
                                                                       (if (@app-open-fltr app)
                                                                         (swap! app-open-fltr disj app)
                                                                         (swap! app-open-fltr conj app))
                                                                       (if (@open-instances-atm insts)
                                                                         (swap! open-instances-atm disj insts)
                                                                         (swap! open-instances-atm conj insts))
                                                                       )]
                                                                    [re-com/label
                                                                     :label app
                                                                     :class "app-name"
                                                                     :width "12em"]                                                                    
                                                                    [:div.separator-0]
                                                                    [:div.separator-1]
                                                                    [re-com/gap :size "7.5rem"]

                                                                    ]]
                                                        (if (@app-open-fltr app)
                                                          (doall
                                                            (for [[inst {status :robot/status mood :robot/mood current :robot/current
                                                                         :or    {status :stopped mood {} current :none}}] insts]
                                                              (let [mood (instance-mood? mood)]
                                                                ^{:key (str app inst)}
                                                                [re-com/h-box
                                                                 :width "100%"
                                                                 :children [[re-com/gap :size "335px"]
                                                                            [:div {:class (str "instance-name-container" (if (= :bad mood)
                                                                                                                           "-bad"))}
                                                                             [re-com/label :label inst :class (str "instance-name" (if (= :bad mood)
                                                                                                                                     "-bad"))]]
                                                                            [:span
                                                                             {:class "btn-container"}
                                                                             (condp = status
                                                                               :running (md-button "zmdi zmdi-settings zmdi-hc-spin btn-cursor btn-console" [:stop app inst]) ;zmdi-hc-2x
                                                                               :stopped (md-button "zmdi zmdi-settings-off btn-cursor btn-console" [:start app inst]) ;zmdi-hc-2x
                                                                               :transitioning (md-button "zmdi zmdi-settings  mdc-text-grey btn-cursor btn-console")) ;zmdi-hc-2x]
                                                                             (cond
                                                                               (and (= status :running) (= :bad mood))
                                                                               (md-button "zmdi zmdi-thumb-down mdc-text-red")
                                                                               (and (= status :running) (= :good mood))
                                                                               (md-button "zmdi zmdi-thumb-up mdc-text-green")
                                                                               :OTHERWIZE
                                                                               (md-button "zmdi zmdi-more")
                                                                               )]
                                                                            [re-com/label :label (if (= current "none") "-" current) :width "auto" :class
                                                                             (str "current-state" (if (= :bad mood)
                                                                                                    "-bad"))
                                                                             ]
                                                                            ]]))))]]]]))
                            [:div {:style {:border-top "1px solid #EEEEEE"}} ""]]]]
        ))))


(defmethod main-tab-panel :console [id]
  (fn [id]
    [re-com/h-box
     :width "100%"
     ;:height "100%"
     :children [[inst-control]]]))

(defn dialog-new [new-key taken-names type path event-k]
  (let [name-atm (reagent/atom "")]
    (fn [new-atm taken-names type path event-k]
      [re-com/border
       :border "5px solid #559"                             ;
       :radius "25px"
       ;:width "15em"
       ;:height "10em"
       :child [re-com/v-box
               :padding "10px"
               :style {:background-color "#d4e3f7" :border-radius "19px"}
               :children [[re-com/title :label (str "New " (clojure.string/capitalize type)) :level :level2]
                          [re-com/gap :size "0.5em"]
                          [re-com/h-box
                           :children [[re-com/title :label (str "Name:") :level :level3]
                                      [re-com/info-button
                                       :info (str "Name of the " type " to be created, it must be an alphanumeric and does not contain blank spaces")]]]
                          [re-com/v-box
                           :class "form-group"
                           :children [[re-com/input-text
                                       :model name-atm
                                       :width "13em"
                                       :status (if (= @name-atm "")
                                                 nil
                                                 (if (re-matches #"[a-zA-Z][A-Za-z-_0-9]*" @name-atm)
                                                   :success
                                                   :error))
                                       :placeholder (clojure.string/capitalize (str type " name"))
                                       :change-on-blur? false
                                       :class "form-control"
                                       :style {:border-radius "5px"}
                                       :on-change (fn [name]
                                                    (reset! name-atm name))]]]
                          [re-com/line :color "#ddd" :style {:margin "10px 0 10px"}]
                          [re-com/h-box
                           :gap "30px"
                           :justify :center
                           :children [[re-com/button
                                       :label "Cancel"
                                       :on-click (fn []
                                                   ;(reset! new-atm nil)
                                                   (re-frame/dispatch [:reset! [:designer :ctrl new-key] nil]))]
                                      [re-com/button
                                       :label "Ok"
                                       :class "btn-primary"
                                       :disabled? (or (nil? (seq @name-atm)) (taken-names @name-atm)
                                                      (not (re-matches #"[a-zA-Z][A-Za-z-_0-9]*" @name-atm)))
                                       :on-click (fn []
                                                   ;(reset! new-atm nil)
                                                   (re-frame/dispatch [:reset! [:designer :ctrl new-key] nil])
                                                   (re-frame/dispatch [event-k @name-atm path]))]]]]]])))


(defn dialog-import []
  (let [name-atm (reagent/atom "")
        file-atm (reagent/atom "Please select a file")]
    (fn []
      [re-com/border
       :border "5px solid #559"                             ;
       :radius "25px"
       :child [re-com/v-box
               :padding "10px"
               :style {:background-color "#d4e3f7" :border-radius "19px"}
               :children [
                          [re-com/title :label "Import application" :level :level2]
                          [re-com/h-box
                           :children [[re-com/title :label (str "Application name:") :level :level3]
                                      [re-com/info-button
                                       :info (str "Name of the application to be imported, it must be an alphanumeric and does not contain blank spaces")]]]
                          [re-com/input-text
                           :model name-atm
                           :width "13em"
                           :placeholder "Application"
                           :change-on-blur? false
                           :status (if (= @name-atm "")
                                     nil
                                     (if (re-matches #"[a-zA-Z][A-Za-z-_0-9]*" @name-atm)
                                       :success
                                       :error))
                           :attr {:pattern    "[a-zA-Z][A-Za-z-_0-9]*"
                                  :max-length "30"
                                  }
                           :style {:padding-left "10px" :padding-right "5px"}
                           :on-change (fn [txt]
                                        (reset! name-atm txt))]
                          [re-com/h-box
                           :children [[re-com/title :label (str "File:") :level :level3]
                                      [re-com/info-button
                                       :info (str "Edn file to be imported")]]]
                          [re-com/h-box
                           :style {:position      "relative"
                                   :cursor        "pointer"
                                   :margin-bottom "0.5em"}
                           :children [
                                      [:input
                                       {:type     "file"
                                        :id       "imported-file"
                                        :onChange (fn [e]
                                                    (let [file (.getElementById js/document "imported-file")
                                                          files (.-files file)
                                                          first-file (if files
                                                                       (aget files 0))
                                                          filename (if first-file
                                                                     (.-name first-file))]
                                                      (if filename
                                                        (reset! file-atm filename))
                                                      ))
                                        :style    {:position "absolute"
                                                   :z-index  "2"
                                                   :opacity  "0"
                                                   :width    "100%"
                                                   :height   "100%"
                                                   :cursor   "pointer"}}
                                       ]
                                      [re-com/md-icon-button
                                       :md-icon-name "zmdi-folder"
                                       :size :larger
                                       :style {:margin-top "5px"}
                                       :tooltip "Select file"
                                       :tooltip-position :right-center
                                       :style {:cursor "pointer"}
                                       ]
                                      [re-com/gap :size "0.5em"]
                                      [re-com/title :label @file-atm
                                       :style {:cursor "pointer"}]]]

                          [re-com/h-box
                           :gap "30px"
                           :justify :center
                           :children [[re-com/button
                                       :label "Cancel"
                                       :on-click (fn []
                                                   (re-frame/dispatch [:reset! [:designer :ctrl :import] nil]))]
                                      [re-com/button
                                       :label "Ok"
                                       :class "btn-primary"
                                       :disabled? (or (nil? (seq @name-atm))
                                                      (not (re-matches #"[a-zA-Z][A-Za-z-_0-9]*" @name-atm))
                                                      (= "Please select a file" @file-atm))
                                       :on-click (fn []
                                                   (let [js-file (-> (.getElementById js/document "imported-file") .-files (aget 0))
                                                         reader (js/FileReader.)]
                                                     (aset reader "onload"
                                                           (fn [e]
                                                             (re-frame/dispatch [:import {:file (.-result (.-target e)) :app-name @name-atm}])))
                                                     (.readAsText reader js-file)))]]]
                          ]]])))




(defn app-ctrl [app editable-app-ops]
  [re-com/h-box
   :width "auto"
   :children [[re-com/single-dropdown
               :choices editable-app-ops
               :model app
               :width "90%"
               :on-change (fn [id]
                            ;(reset! app-atm id)
                            (re-frame/dispatch [:reset! [:designer :ctrl :app] id]))
               :placeholder "Select an app"]
              [re-com/gap :size "1em"]
              [re-com/md-icon-button
               :md-icon-name "zmdi-plus"
               :size :regular
               :style {:margin-top "5px"}
               ;:emphasise? true
               :tooltip "New application"
               :on-click (fn []
                           ;(reset! new-app-atm :create)
                           (re-frame/dispatch [:reset! [:designer :ctrl :new-app] :create])
                           )]
              ;[re-com/gap :size "3em"]
              ;[re-com/button
              ; :label "Save"
              ; :class "rc-icon-emphasis"
              ; :disabled? (nil? (seq app))
              ; :on-click (fn []
              ;             (re-frame/dispatch [:save-app app]))]
              ]])

(defn operations-ctrl [app opr-choices opr-id-atm state-id-atm invalid-name?]
  [re-com/h-box
   :class "opr-container"
   :children [[re-com/single-dropdown
               :choices opr-choices
               :model opr-id-atm
               :class "opr-selector"
               :placeholder "Operation type"
               :width "30%"
               :on-change (fn [id]
                            (reset! opr-id-atm id))
               :render-fn (fn [{:keys [id image]}]
                            (println :OPERATIONS  image)
                            [re-com/h-box
                             :children [[:img {:src    (str "images/icons/operations/" image)
                                               :width  "25px"
                                               :height "25px"}]
                                        [re-com/gap
                                         :size "0.5em"]
                                        [:p id
                                         ]]])
               ]
              [re-com/gap
               :size "0.2em"]
              [re-com/input-text
               :model state-id-atm
               :width "30%"
               :placeholder "Operation name"
               :change-on-blur? false
               ;(not (re-matches #"[a-zA-Z][A-Za-z-_0-9]*" @state-id-atm))
               :status (if (= @state-id-atm "")
                         nil
                         (if (re-matches #"[a-zA-Z][A-Za-z-_0-9]*" @state-id-atm)
                           :success
                           :error))
               :attr {:pattern    "[a-zA-Z][A-Za-z-_0-9]*"
                      :max-length "30"
                      }
               :style {:padding-left "10px" :padding-right "5px"
                       }
               :on-change (fn [txt]
                            (if (and (= txt @state-id-atm)
                                     (re-matches #"[a-zA-Z][A-Za-z-_0-9]*" @state-id-atm))
                              (re-frame/dispatch [:add-state
                                                  app
                                                  (keyword @state-id-atm)
                                                  (keyword @opr-id-atm)])
                              (reset! state-id-atm txt)))]
              [re-com/gap
               :size "1em"]
              [re-com/md-icon-button
               :md-icon-name "zmdi-plus"
               :size :regular
               :class "add-btn"
               :tooltip (if invalid-name?
                          "Select a valid name to enable creation"
                          "Create new operation in canvas")
               :tooltip-position :right-center
               :disabled? invalid-name?
               :on-click (fn []
                           (re-frame/dispatch [:add-state
                                               app
                                               (keyword @state-id-atm)
                                               (keyword @opr-id-atm)])
                           ;dispatch par aumentar state al editable
                           )]]])

(defn status-of-txt [txt regex]
  (cond
    (nil? (seq txt))
    nil

    (re-matches regex txt)
    :success

    :OTHERWIZE
    :error))

;(def hidden-parameters #{:robot/current :robot/mood :robot/previous :robot/status})

(defn edit-params
  ([params path width]
   (edit-params params path width "10em" true))
  ([params path width height with-click]
   (let [entry-k-v (reagent/atom {:k "" :v ""})
         k-re #"^[a-zA-Z0-9\-_&]*$"]
     (fn [params path width height]
       (let [{:keys [k v]} @entry-k-v
             d-filter (if path identity (fn [[p-k p-v]]
                                          (and (re-find (re-pattern (or k "")) (str p-k))
                                               (re-find (re-pattern (or v "")) (str p-v)))))
             view-params (into {} (filter d-filter params))]
         [re-com/border
          :border "1px"
          :radius "5px"
          :width width
          ;:height height
          :child [re-com/v-box
                  :width "100%"
                  :class "app-param-panel"
                  :children [[re-com/h-box
                              :width "100%"
                              :class "add-panel"
                              :children (let [uuid-key      (random-uuid)
                                              input-key [re-com/input-text
                                                           :placeholder "Name"
                                                           :model k
                                        ;:validation-regex #"^[a-zA-Z0-9\-_&]*$"
                                                           :status-icon? false
                                                           :change-on-blur? false
                                                           :status (status-of-txt k k-re)
                                                           :width "25%"
                                                           :status (if (= k "")
                                                                     nil
                                                                     (if (re-matches #"[a-zA-Z][A-Za-z-_0-9]*" k)
                                                                       :success
                                                                       :error))
                                                           :attr {:id uuid-key :max-length "30"}
                                                           :style {:padding-right "5px"}
                                                           :on-change (fn [txt]
                                                                        (swap! entry-k-v assoc :k txt))]
                                              input-val [re-com/input-text
                                                           :placeholder "Value"
                                                           :width "58%"
                                                           :model v
                                                           :change-on-blur? false
                                                           :style {:padding-right "5px" :margin-left "5px"}
                                                           :on-change (fn [txt]
                                                                        (if (and (= txt (get @entry-k-v :v))
                                                                                 (re-matches #"[a-zA-Z][A-Za-z-_0-9]*" (get @entry-k-v :k)))
                                                                          (do
                                                                            (re-frame/dispatch [:add-param path @entry-k-v])
                                                                            (reset! entry-k-v {:k "" :v ""})
                                                                            (.focus (.getElementById js/document uuid-key)))
                                                                          (swap! entry-k-v assoc :v txt)))]
                                              eraser-button [re-com/md-icon-button
                                                             :md-icon-name "zmdi-close-circle-o"
                                                             :class "eraser-btn"
                                                             :size :regular
                                                             :tooltip "Clear boxes"
                                                             :on-click (fn []
                                                                         (reset! entry-k-v {:k "" :v ""}))]]
                                          [input-key
                                           input-val
                                           eraser-button
                                           (when path
                                             [re-com/md-icon-button
                                              :md-icon-name "zmdi-plus-circle-o"
                                              :class "add-btn"
                                              :size :regular
                                              :tooltip "Add parameter"
                                              :disabled? (or
                                                          (not (re-matches #"[a-zA-Z][A-Za-z-_0-9]*" k))
                                                          (nil? (seq k)) (nil? (seq v)))
                                              :on-click (fn []
                                                          (re-frame/dispatch [:add-param path @entry-k-v])
                                                          (reset! entry-k-v {:k "" :v ""})
                                                          (.focus (.getElementById js/document uuid-key)))])])]
                             [re-com/v-box
                              :class "param-list"
                              :children [(doall
                                          (for [[k v] (sort (into [] view-params))]
                                            ^{:key (str k)}
                                            [re-com/h-box
                                             :style {:cursor "pointer"}
                                             :attr {:on-click (fn [e]
                                                                (if with-click
                                                                  (reset! entry-k-v {:k (subs (str k) 1) :v (str v)})))}
                                             :children [[re-com/gap :size "0.2em"]
                                                        [re-com/label
                                                         :label (str k)
                                                         :class "app-param-key"
                                                         :width "30%"
                                                         :style {:overflow "hidden"
                                                                 :flex     "initial"
                                                                 }]
                                                        [re-com/gap :size "0.2em"]
                                                        (cond
                                                          (= :robot/selenium-err k)
                                                          (let [data (str "data:image/png;base64," v)]
                                                            [:a {:href data :target "_blank"}
                                                             [:img {:src data :height "100px"}]])

                                                          (re-find #"[Pp][Aa][Ss][Ss]|[Pp][Ww][Dd]|[Cc][Ll][Aa][Vv][Ee]" (str k))
                                                          [re-com/label
                                                           :label "*****"
                                                           :class "app-param-value"
                                                           :width "60%"]

                                                          :else
                                                          [re-com/label
                                                           :label (str v)
                                                           :class "app-param-value"
                                                           :on-click (fn []
                                                                       (if with-click
                                                                         (reset! entry-k-v {:k (subs (str k) 1) :v (str v)})))
                                                           :width "60%"])
                                                        (when path
                                                          [re-com/md-icon-button
                                                           :md-icon-name "zmdi-delete"
                                                           :class "delete-btn"
                                                           :tooltip "Delete parameter"
                                                           :tooltip-position :right-center
                                                           :size :smaller
                                                           :style {:overflow    "hidden"
                                                                   :flex        "initial"}
                                                           :on-click (fn [e]
                                                                       (.preventDefault e)
                                                                       (.stopPropagation e)
                                                                       (re-frame/dispatch [:rm-param path k]))])]]))]]
                             #_[re-com/scroller
                              :h-scroll (if with-click :off :on)
                              :v-scroll (if with-click :off :on)
                              ;:height "100%"

                              :width "100%"
                              :child ]]]])))))

(defn instance-params [app-id watch-instance-atm open-instances-atm inst-id inst-params
                       {status :robot/status mood :robot/mood current :robot/current :or {status :stopped} :as running}]
  (let [mood (instance-mood? mood)]
    [re-com/v-box
     :width "100%"
     :class "instance"
     :children [[re-com/h-box
                 :class "instance-main"
                 :children [[md-select-one-button "zmdi zmdi-eye mdc-text-green btn-cursor" "zmdi zmdi-eye-off mdc-text-grey btn-cursor" inst-id watch-instance-atm]
                            [md-open-close-button "zmdi zmdi-caret-right btn-cursor" "zmdi zmdi-caret-down btn-cursor" inst-id open-instances-atm]
                            [re-com/label
                             :width "50%"
                             :label inst-id]
                            [re-com/gap :size "1em"]
                            (condp = status
                              :running (md-button "zmdi zmdi-settings zmdi-hc-spin btn-cursor btn-console" [:stop app-id inst-id]) ;zmdi-hc-2x
                              :stopped (md-button "zmdi zmdi-settings-off btn-cursor btn-console" [:start app-id inst-id]) ;zmdi-hc-2x
                              :transitioning (md-button "zmdi zmdi-settings  mdc-text-grey btn-cursor btn-console"))
                            ;(condp = status
                            ;  :running (md-button "zmdi zmdi-settings btn-cursor" [:stop app-id inst-id]) ;zmdi-hc-2x
                            ;  :stopped (md-button "zmdi zmdi-settings-off btn-cursor" [:start app-id inst-id]) ;zmdi-hc-2x
                            ;  :transitioning (md-button "zmdi zmdi-settings mdc-text-grey btn-cursor"))
                            (cond
                              (and (= status :running) (= :bad mood))
                              (md-button "zmdi zmdi-thumb-down mdc-text-red")
                              (and (= status :running) (= :good mood))
                              (md-button "zmdi zmdi-thumb-up mdc-text-green")
                              :OTHERWIZE
                              (md-button "zmdi zmdi-more"))
                            [re-com/label :width "25%" :label current :class "instance-label"]

                            ]]
                (if (@open-instances-atm inst-id)
                  [re-com/h-box
                   :width "100%"
                   :children [[re-com/gap :size "2em"]
                              [edit-params inst-params [app-id :instances inst-id] "92%"]]])]]))

(defn get-running-instances [state app-name]
  (filter
    (fn [instance]
      (= :running (:robot/status instance)))
    (doall
      (map
        (fn [instance]
          (get-in state [app-name instance]))
        (keys (get-in state [app-name]))))))

(defmethod main-tab-panel :designer [id]
  (let [editable-apps (re-frame/subscribe [[:applications :editable]])
        ready-apps (re-frame/subscribe [[:applications :ready]])
        operations (re-frame/subscribe [[:operations]])
        designer-ctrl (re-frame/subscribe [[:designer :ctrl]])
        watch-instance-atm (reagent/atom nil)
        open-instances-atm (reagent/atom #{})
        opr-id-atm (reagent/atom nil)
        state-id-atm (reagent/atom "")
        new-instance-atm (reagent/atom "")]
    (fn [id]
      (let [opr-choices (reduce (fn [result [opr {:keys [image fields]}]]
                                  (conj result {:id    (name opr)
                                                :label (name opr)
                                                :image image}))
                                []
                                (sort @operations))
            editable @editable-apps
            ready @ready-apps
            {:keys [app new-app new-inst import] :as d-ctrl} @designer-ctrl
            app-names (into #{} (keys editable))
            editable-app-ops (mapv (fn [[app _]]
                                     {:id app :label app})
                                   (sort editable))
            current-state-ids (into #{} (keys (get-in editable [app :states])))
            invalid-name? (or (not (re-matches #"[a-zA-Z][A-Za-z-_0-9]*" @state-id-atm))
                              (nil? (seq @opr-id-atm))
                              (current-state-ids (keyword @state-id-atm))
                              (nil? (seq app))
                              (nil? (seq @state-id-atm)))
            inst-names (if (seq app)
                         (into #{} (keys (get-in editable [app :instances]))))]
        [re-com/h-box
         :width "100%"
         :height "85%"
         :children [[re-com/h-split
                     :class "split-container"
                     :panel-1 [re-com/v-box
                               :class "vertical-box"
                               :width "100%"
                               :children [[re-com/h-box
                                           :class "toolbar-container"
                                           :width "100%"
                                           :children [[:div
                                                       {:class "app-container"}
                                                       [re-com/single-dropdown
                                                        :class "app-selector"
                                                        :choices editable-app-ops
                                                        :model app
                                                        :width "100%"
                                                        :on-change (fn [id]
                                                                     (re-frame/dispatch [:reset! [:designer :ctrl :app] id]))
                                                        :placeholder "Select an app"]]

                                                      [re-com/gap
                                                       :size "0.5em"]
                                                      [re-com/md-icon-button
                                                       :class "new-app-btn"
                                                       :md-icon-name "zmdi-plus"
                                                       :size :regular
                                                       :tooltip "New application"
                                                       :on-click (fn []
                                                                   ;(reset! new-app-atm :create)
                                                                   (re-frame/dispatch [:reset! [:designer :ctrl :new-app] :create])
                                                                   )]
                                                      
                                                      #_[re-com/md-icon-button
                                                       :class "delete-app-btn"
                                                       :md-icon-name "zmdi-plus"
                                                       :size :regular
                                                       :tooltip "Remove application"
                                                       :on-click (fn []
                                                                   ;(reset! new-app-atm :create)
                                                                   (re-frame/dispatch [:pretty-print nil])
                                                                   )]
                                                      [re-com/gap
                                                       :class "con-gap"
                                                       :size "0.5em"]
                                                      [re-com/md-icon-button
                                                       :md-icon-name "zmdi-cloud-upload"
                                                       :class "import-btn"
                                                       :tooltip "Import"
                                                       :on-click (fn []
                                                                   (re-frame/dispatch [:import-app-dlg]))]
                                                      [re-com/gap
                                                       :size "0.5em"]
                                                      [re-com/md-icon-button
                                                       :md-icon-name "zmdi-cloud-download"
                                                       :class "export-btn"
                                                       :tooltip "Export"
                                                       :disabled? (nil? (seq app))
                                                       :on-click (fn []
                                                                   (.open js/window (str "/application/" app)))]
                                                      [re-com/gap
                                                       :class "con-gap"
                                                       :size "0.5em"]
                                                      [re-com/md-icon-button
                                                       :md-icon-name "zmdi-undo"
                                                       :class "undo-btn"
                                                       :tooltip "Undo"
                                                       :on-click (fn []
                                                                   (re-frame/dispatch [:undo]))]
                                                      [re-com/gap
                                                       :size "0.5em"]
                                                      [re-com/md-icon-button
                                                       :tooltip "Redo"
                                                       :class "redo-btn"
                                                       :md-icon-name "zmdi-redo"
                                                       :on-click (fn []
                                                                   (re-frame/dispatch [:redo]))]
                                                      [re-com/gap
                                                       :class "con-gap"
                                                       :size "0.5em"]
                                                      [re-com/md-icon-button
                                                       :md-icon-name "zmdi-save"
                                                       :class (if (some identity (get-running-instances ready app))
                                                                "save-off-btn"
                                                                "save-btn")
                                                       :tooltip (if (some identity (get-running-instances ready app))
                                                                  "Stop application before saving"
                                                                  "Save")
                                                       :disabled? (or
                                                                    (nil? (seq app))
                                                                    (some identity (get-running-instances ready app)))
                                                       :on-click (fn []
                                                                   (re-frame/dispatch [:save-app app]))]
                                                      ]]

                                          (if (and app (seq app))
                                            (let [regex-inst #"[a-zA-Z][A-Za-z-_0-9]*"]
                                              [re-com/scroller
                                               :h-scroll :off
                                               :v-scroll :on
                                               :width "100%"
                                               :child
                                               [re-com/v-box
                                                :style {:margin-bottom "20px"}
                                                :children [[re-com/title
                                                            :label "Application Parameters"
                                                            :class "title"]
                                                           [edit-params (get-in editable [app :app-params]) [app :app-params] "100%"]
                                                           [re-com/title
                                                            :label "Application Instances"
                                                            :class "title"]
                                                           [re-com/h-box
                                                            :class "add-instance"
                                                            :children [[re-com/input-text
                                                                        :placeholder "Instance Name"
                                                                        :width "87%"
                                                                        :model new-instance-atm
                                                                        :change-on-blur? false
                                                                        :style {:padding-right "5px" :margin-left "5px"}
                                                                        :status (if (= @new-instance-atm "")
                                                                                  nil
                                                                                  (if (re-matches regex-inst @new-instance-atm)
                                                                                    :success
                                                                                    :error))
                                                                        :on-change (fn [name]
                                                                                     (if (and (= name @new-instance-atm)
                                                                                              (re-matches regex-inst @new-instance-atm))
                                                                                       (re-frame/dispatch [:create-inst @new-instance-atm [app :instances]])
                                                                                       (reset! new-instance-atm name)))]
                                                                       [re-com/md-icon-button
                                                                        :md-icon-name "zmdi-plus"
                                                                        :class "add-btn"
                                                                        :size :regular
                                                                        :tooltip "New instance"
                                                                        :on-click (fn []
                                                                                    (if (re-matches regex-inst @new-instance-atm)
                                                                                      (re-frame/dispatch [:create-inst @new-instance-atm [app :instances]])))]]]
                                                           [re-com/v-box
                                                                    :children [(doall
                                                                                (for [[inst-id inst-params] (sort (get-in editable [app :instances]))]
                                                                                  ^{:key (str inst-id)}
                                                                                  [instance-params app watch-instance-atm open-instances-atm inst-id inst-params
                                                                                   (get-in ready [app inst-id])]))]]
                                                           #_[re-com/scroller
                                                            :height "50vh"
                                                            :class "inst-scroll"
                                                            :h-scroll :off
                                                            :v-scroll :on
                                                            :child ]]]]))
                                          ]]
                     :panel-2 [re-com/h-split
                               :class "internal-split"
                               :panel-1 [re-com/v-box
                                         :width "100%"
                                         :children [[operations-ctrl app opr-choices opr-id-atm state-id-atm invalid-name?]
                                                    [svg/svg app @watch-instance-atm]]]
                               :panel-2 (if (and @watch-instance-atm (seq @watch-instance-atm))
                                          [re-com/box
                                           :width "100%"
                                           ;:height "30em"
                                           :style {:margin-bottom "20px"}
                                           :child
                                           [re-com/scroller
                                            :h-scroll :off
                                            :v-scroll :on
                                            :child [edit-params (get-in ready [app @watch-instance-atm]) nil "50em" "30em" false]]]
                                          [re-com/title :label "Please select an instance to watch!"])
                               :initial-split "80%"]
                     :initial-split "25%"]
                    (if (= :create new-app)
                      [re-com/modal-panel
                       :backdrop-color "grey"
                       :backdrop-opacity 0.4
                       :wrap-nicely? false
                       :backdrop-on-click (fn []
                                            ;(reset! new-app-atm nil)
                                            (re-frame/dispatch [:reset! [:designer :ctrl :new-app] nil])
                                            )
                       :style {:font-family "'Helvetica Neue', Helvetica, Arial, sans-serif" :font-size "15px"}
                       :child [dialog-new :new-app app-names "application" [app] :create-app]])
                    (if (= :create new-inst)
                      [re-com/modal-panel
                       :backdrop-color "grey"
                       :backdrop-opacity 0.4
                       :wrap-nicely? false
                       :backdrop-on-click (fn []
                                            ;(reset! new-inst-atm nil)
                                            (re-frame/dispatch [:reset! [:designer :ctrl :new-inst] nil]))
                       :style {:font-family "'Helvetica Neue', Helvetica, Arial, sans-serif" :font-size "15px"}
                       :child [dialog-new :new-inst inst-names "instance" [app :instances] :create-inst]])
                    (if import
                      [re-com/modal-panel
                       :backdrop-color "grey"
                       :backdrop-opacity 0.4
                       :wrap-nicely? false
                       :backdrop-on-click (fn []
                                            (re-frame/dispatch [:reset! [:designer :ctrl :import] nil]))
                       :style {:font-family "'Helvetica Neue', Helvetica, Arial, sans-serif" :font-size "15px"}
                       :child [dialog-import]])
                    ]]))))

(defn edit-users [users]
  (let [entry (reagent/atom {:email "" :pass "" :admin false})
        re #"^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"]
    (fn [users]
      [re-com/border
       :border "1px"
       :radius "5px"
       :width "44em"
       :height "85%"
       :class "config-tab"
       :child [re-com/v-box
               :width "100%"
               :children [[re-com/gap :size "1em"]
                          [re-com/h-box
                           :children [[re-com/gap :size "1em"]
                                      [re-com/v-box
                                       :width "100%"
                                       :children [
                                                  [:div {:class "title"} "Users"]
                                                  [re-com/h-box
                                                   :class "btn-container"
                                                   :children [[re-com/md-icon-button
                                                               :class "btn-undo"
                                                               :md-icon-name "zmdi-undo"
                                                               :tooltip "Undo"
                                                               :on-click (fn []
                                                                           (re-frame/dispatch [:undo]))]
                                                              [re-com/gap
                                                               :size "0.5em"]
                                                              [re-com/md-icon-button
                                                               :class "btn-redo"
                                                               :tooltip "Redo"
                                                               :md-icon-name "zmdi-redo"
                                                               :on-click (fn []
                                                                           (re-frame/dispatch [:redo]))]
                                                              [re-com/gap :size "0.5em"]
                                                              ]]]]]
                           ]
                          [re-com/gap :size "1em"]
                          [re-com/h-box
                           :class "input-container"
                           :children [[re-com/gap :size "0.5em"]
                                      [re-com/input-text
                                       :placeholder "User email"
                                       :width "15rem"
                                       :height "2rem"
                                       :class "mail-input"
                                       :model (:email @entry)
                                       :attr {:max-length "60"}
                                       ;:validation-regex #"^[a-zA-Z0-9\-_&]*$"
                                       :status-icon? false
                                       :change-on-blur? false
                                       :status (if (= (:email @entry) "")
                                                 nil
                                                 (if (re-matches re (:email @entry)) :success :error))
                                       :on-change (fn [txt]
                                                    (swap! entry assoc :email txt))]
                                      [re-com/gap
                                       :size "3rem"]
                                      [re-com/input-text
                                       :placeholder "User password"
                                       :width "15rem"
                                       :height "2rem"
                                       :class "mail-input"
                                       :attr {:max-length "20"}
                                       :input-type :password
                                       :model (:pass @entry "NOHAYPASSWD")
                                       :status-icon? false
                                       :change-on-blur? false
                                       :status (if (= (:pass @entry) "NOHAYPASSWD")
                                                 nil
                                                 (if (re-matches #".{0,20}" (:pass @entry "")) :success :error))
                                       :on-change (fn [txt]
                                                    (if (not= txt "NOHAYPASSWD")
                                                      (swap! entry assoc :pass txt)))]
                                      [re-com/gap
                                       :size "3rem"]
                                      [re-com/checkbox
                                       :model (:admin @entry)
                                       :label "admin"
                                       :on-change (fn [new-val]
                                                    (swap! entry assoc :admin new-val))
                                       :style {:flex     "initial"
                                               :overflow "hidden"}]

                                      [re-com/md-icon-button
                                       :class "btn-add-user"
                                       :md-icon-name "zmdi-plus"
                                       :tooltip "Add user"
                                       :style {:margin-left "0.5em" :margin-top "0.2em"}
                                       :disabled? (or (nil? (seq (:email @entry)))
                                                      (not (re-matches #".{0,20}" (:pass @entry "")))
                                                      (not (re-matches re (:email @entry))))
                                       :on-click (fn []
                                                   (re-frame/dispatch [:add-user @entry])
                                                   (re-frame/dispatch [:save-users])
                                                   (reset! entry {:email "" :pass ""}))
                                       ]]]
                          [re-com/scroller
                           :h-scroll :off
                           :v-scroll :on
                           :height "20em"
                           :child [re-com/v-box
                                   :children [(doall
                                                (for [[email {:keys [hpass admin]}] (sort (into [] users))]
                                                  ^{:key (str email)}
                                                  [re-com/h-box
                                                   :class "user-row"
                                                   :children [[re-com/label
                                                               :class "user"
                                                               :label email
                                                               :width "15rem"
                                                               :on-click (fn []
                                                                           (reset! entry {:email email :hpass hpass :admin admin}))
                                                               :style {:flex     "initial"
                                                                       :overflow "hidden"}]
                                                              [re-com/gap
                                                               :size "3rem"]
                                                              [re-com/label
                                                               :class "password"
                                                               :label "*********"
                                                               :width "14rem"
                                                               :on-click (fn []
                                                                           (reset! entry {:email email :hpass hpass :admin admin}))
                                                               :style {:flex     "initial"
                                                                       :overflow "hidden"}]
                                                              [re-com/gap
                                                               :size "1rem"]
                                                              [re-com/checkbox
                                                               :model admin
                                                               :label "admin"
                                                               :on-change (fn [new-val]
                                                                            (reset! entry {:email email :hpass hpass :admin new-val})
                                                                            (re-frame/dispatch [:add-user @entry])
                                                                            (re-frame/dispatch [:save-users]))
                                                               :style {:flex     "initial"
                                                                       :overflow "hidden"}]
                                                              [re-com/gap
                                                               :size "1rem"]
                                                              [re-com/md-icon-button
                                                               :md-icon-name "zmdi-delete"
                                                               :class "btn-remove"
                                                               :tooltip "Remove user"
                                                               :style {:margin-left "0.5em" :margin-top "0.2em"}
                                                               :size :smaller
                                                               :on-click (fn []
                                                                           (re-frame/dispatch [:rm-user email])
                                                                           (re-frame/dispatch [:save-users]))]
                                                              [re-com/gap
                                                               :size "2rem"]]]))]]]]]])))


(defmethod main-tab-panel :users [id]
  (let [users (re-frame/subscribe [:users])]
    (re-frame/dispatch [:load-users])
    (fn [id]
      [re-com/h-box
       :height "100%"
       :width "100%"
       :children [[re-com/gap :size "1em"]
                  [edit-users @users]]])))
