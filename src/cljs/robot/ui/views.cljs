(ns robot.ui.views
  (:require [cljs.pprint :refer [pprint]]
            [re-frame.core :as re-frame]
            [re-com.core :as re-com]
            [reagent.core :as reagent :refer [atom create-class]]
            [robot.ui.events :refer [create-log]]
            [robot.ui.robot-control :as ui-robot-control]))

(reagent/force-update-all)

(defn app-title []
  (comment let [name (re-frame/subscribe [:name])]
           (fn []
             [re-com/title
              :label "ups"
              :level :level2]))
  (fn []
    [:img
     {:src "images/logo-header.png"
      :alt "iwRobot"}]))

(defn user []
  (let [uid (re-frame/subscribe [[:control :uid]])]
    (fn []
      [re-com/title
       :label @uid
       :level :level4
       :class "user-email"
       ])))

(defn title []
  (let []
    (fn []
      [re-com/h-box
       :width "100%"
       :class "header"
       :children [[re-com/h-box
                   :class "logo-container"
                   :children [[app-title]]]
                  [:div {:class "user-info"}
                   [re-com/h-box
                    :children [[user]
                               [re-com/md-icon-button
                                :class "logout-btn"
                                :md-icon-name "zmdi-directions-run"
                                :tooltip "Logout"
                                :tooltip-position :right-center
                                :on-click (fn [event]
                                            (re-frame/dispatch [:logout]))]
                               [re-com/gap :size "5px"]]]]
                  ]])))

(defn work-area []
  (let [admin? (re-frame/subscribe [[:control :admin]])
        main-tab-atm (reagent/atom :console)]
    (fn []
      [re-com/v-box
       :height "100%"  
       :width "100%"
       :children [[re-com/horizontal-tabs
                   :model @main-tab-atm
                   :tabs (cond-> [{:id :console :label "Console"}]
                                 (or true @admin?) (conj {:id :designer :label "Designer"})
                                 @admin? (conj {:id :users :label "Configuration"}))
                   :style {:color "#559"}
                   :on-change (fn [selection]
                                (reset! main-tab-atm selection))]
                  [(ui-robot-control/main-tab-panel @main-tab-atm)]
                  ]])))


(defn g-button [clickfn]
  [:div.googleButton.googleButtonLightBlue
   {:onClick clickfn
    :style   {:height        "36px"
              :width         "200px"
              :margin-bottom "1em"
              :margin-right  "0.4em"}}
   [:div.googleButtonContentWrapper
    [:div.googleButtonIcon
     {:style {:padding "8px"}}
     [:div.googleButtonSvgImageWithFallback.googleButtonIconImage.googleButtonIconImage18
      {:style {:width  "18px"
               :height "18px"}}
      [:svg.googleButtonSvg
       {:viewBox "0 0 48 48",
        :height  "18px",
        :width   "18px",
        :version "1.1"}
       [:g
        [:path
         {:d
                "M24 9.5c3.54 0 6.71 1.22 9.21 3.6l6.85-6.85C35.9 2.38 30.47 0 24 0 14.62 0 6.51 5.38 2.56 13.22l7.98 6.19C12.43 13.72 17.74 9.5 24 9.5z",
          :fill "#EA4335"}]
        [:path
         {:d
                "M46.98 24.55c0-1.57-.15-3.09-.38-4.55H24v9.02h12.94c-.58 2.96-2.26 5.48-4.78 7.18l7.73 6c4.51-4.18 7.09-10.36 7.09-17.65z",
          :fill "#4285F4"}]
        [:path
         {:d
                "M10.53 28.59c-.48-1.45-.76-2.99-.76-4.59s.27-3.14.76-4.59l-7.98-6.19C.92 16.46 0 20.12 0 24c0 3.88.92 7.54 2.56 10.78l7.97-6.19z",
          :fill "#FBBC05"}]
        [:path
         {:d
                "M24 48c6.48 0 11.93-2.13 15.89-5.81l-7.73-6c-2.15 1.45-4.92 2.3-8.16 2.3-6.26 0-11.57-4.22-13.47-9.91l-7.98 6.19C6.51 42.62 14.62 48 24 48z",
          :fill "#34A853"}]
        [:path {:d "M0 0h48v48H0z", :fill "none"}]]]]]
    [:span.googleButtonContents
     {:style {:font-size   "13px"
              :line-height "34px"}}
     [:span "Sign in with Google"]]]]
  )


(defn login-dialog []
  (let [init-data {:email ""
                   :pass  ""
                   }
        re #"^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
        form-data (reagent/atom init-data)
        process-ok (fn [event]
                     (.preventDefault event)
                     (re-frame/dispatch [:login {:uid  (:email @form-data)
                                                 :pass (:pass @form-data)}])
                     false)]
    (fn []
      [re-com/box
       :align :center
       :justify :center
       :class "login-dialog-container"
       :child [re-com/h-box
               :justify :center
               :children [[re-com/gap :size "300px" :width "30%"]
                          [re-com/border
                           :class "login-dialog-box"
                           :child [:form
                                   {:on-submit process-ok}
                                   [re-com/v-box
                                    :class "login-dialog-v-box"
                                    :children [
                                               [:img {:src   "images/logo-login.png"
                                                      :class "logo"}]
                                               [re-com/gap :size "1em"]
                                               [re-com/v-box
                                                :class "form-group"
                                                :children [
                                                           [re-com/input-text
                                                            :model (:email @form-data)
                                                            :on-change #(swap! form-data assoc :email %)
                                                            :placeholder "Enter email"
                                                            :class "form-control email"
                                                            :change-on-blur? true
                                                            :attr {:id         "pf-email"
                                                                   :type       "email"
                                                                   :required   "required"
                                                                   :max-length "60"
                                                                   }]]]
                                               [re-com/v-box
                                                :class "form-group"
                                                :children [
                                                           [re-com/input-text
                                                            :model (:pass @form-data)
                                                            :on-change #(swap! form-data assoc :pass %)
                                                            :placeholder "Enter password"
                                                            :class "form-control password"
                                                            :change-on-blur? false
                                                            :attr {:id         "pf-password"
                                                                   :type       "password"
                                                                   :max-length "20"
                                                                   :required   "required"}]]]
                                               [re-com/v-box
                                                :children [[re-com/h-box
                                                            :width "100%"
                                                            :gap "40px"
                                                            :justify :center
                                                            :children [[re-com/button
                                                                        :label "Login"
                                                                        :attr {:type "submit"}
                                                                        :style {:width "200px"}
                                                                        :class "btn-primary login-btn"
                                                                        ]]]
                                                           [re-com/gap :size "1.5em"]
                                                           [re-com/h-box
                                                            :gap "10px"
                                                            :justify :center
                                                            :children [[:div
                                                                        {:class          "g-signin2"
                                                                         :data-onsuccess "loginFromGoog"
                                                                         :data-prompt    "select_account"
                                                                         :style          {:display "none"}
                                                                         }
                                                                        ]
                                                                       [g-button
                                                                        (fn login []
                                                                          (let [do-login (fn [uid pass]
                                                                                           (re-frame/dispatch [:login {:uid (str uid) :pass pass}]))
                                                                                log-error-message (fn [text]
                                                                                                    (re-frame/dispatch (create-log :error 500 text)))]
                                                                            (js/GOOGlogin do-login log-error-message)))]
                                                                       ]]]]
                                               ]]]]]]])))




(defn logger []
  (let [log (re-frame/subscribe [[:log]])
        ts-agt (reagent/atom "")
        level-agt (reagent/atom :all)
        status-agt (reagent/atom "")
        msg-agt (reagent/atom "")
        pat-build (fn [txt] (if (and txt (seq txt)) (re-pattern txt)))
        level-color (fn [level]
                      (get {:info  "green"
                            :warn  "#b59429"
                            :error "red"} level))
        extract-agt (fn [agt]
                      (let [val @agt]
                        (if (seq (str val))
                          val)))]
    (fn []
      (let [log-list @log
            ts (extract-agt ts-agt)
            ts-pat (pat-build ts)
            level @level-agt
            status (extract-agt status-agt)
            status-pat (pat-build status)
            msg (extract-agt msg-agt)
            msg-pat (pat-build msg)
            filter-pred (fn [[ts level-entry status msg]]
                          (and
                            (or (nil? ts-pat) (re-find ts-pat ts))
                            (or (= :all level) (= level-entry level))
                            (or (nil? status-pat) (re-find status-pat (str status)))
                            (or (nil? msg-pat) (re-find msg-pat msg))))
            log-list (filter filter-pred log-list)]
        [re-com/v-box
         :class "logger"
         :width "100%"
         :height "95%"
         :children [[re-com/h-box
                     :children [[re-com/input-text
                                 :placeholder "T1231"
                                 :width "20%"
                                 :height "2em"
                                 :class "transaction-filter"
                                 :style {:font-size     "13px"
                                         :border-radius "0px 0px 0px 0px"}
                                 :model @ts-agt
                                 :change-on-blur? false
                                 :on-change (fn [ts-txt]
                                              (reset! ts-agt ts-txt)
                                              ;(re-frame/dispatch [:update-db [:control :log :ts] ts-txt])
                                              )]
                                [re-com/single-dropdown
                                 :choices [{:id :all} {:id :error} {:id :warn} {:id :info}]
                                 :label-fn :id
                                 :class "level-filter"
                                 :style {:font-size     "13px"
                                         :height "2rem"
                                         :border-radius "0px 0px 0px 0px"
                                         :border-left   "0px"}
                                 :model @level-agt
                                 :width "5%"
                                 :on-change (fn [new-level]
                                              (reset! level-agt new-level)
                                              ;(re-frame/dispatch [:update-db [:control :log :level] new-level])
                                              )]
                                [re-com/input-text
                                 :placeholder "200"
                                 :width "5%"
                                 :height "2em"
                                 :class "status-code-filter"
                                 :style {:font-size     "13px"
                                         :border-radius "0px 0px 0px 0px"
                                         :border-left   "0px"}
                                 :model @status-agt
                                 :change-on-blur? false
                                 :on-change (fn [status-txt]
                                              (reset! status-agt status-txt)
                                              ;(re-frame/dispatch [:update-db [:control :log :status] status-txt])
                                              )]
                                [re-com/input-text
                                 :placeholder ".*error.*"
                                 :width "70%"
                                 :height "2em"
                                 :class "text-filter"
                                 :style {:font-size     "13px"
                                         :border-radius "0px 0px 0px 0px"
                                         :border-left   "0px"}
                                 :model @msg-agt
                                 :change-on-blur? false
                                 :on-change (fn [msg-txt]
                                              (reset! msg-agt msg-txt)
                                              ;(re-frame/dispatch [:update-db [:control :log :msg] msg-txt])
                                              )]]]
                    [re-com/scroller
                     :v-scroll :auto
                     ;:h-scroll :none
                     :height "100%"
                     :child [re-com/v-box
                             :children [(for [[ts level status msg millis] log-list]
                                          ^{:key millis}
                                          [re-com/h-box
                                           :width "100%"
                                           :style {:color (level-color level) :font-size "12px"}
                                           :children [[re-com/label
                                                       :label ts
                                                       :width "20%"]
                                                      [re-com/label
                                                       :label level
                                                       :width "5%"
                                                       ]
                                                      [re-com/label
                                                       :label status
                                                       :width "5%"]
                                                      [re-com/label
                                                       :label msg
                                                       :width "70%"]
                                                      ]])]]]]]))))

(defn main-panel []
  (let [registered-uid (re-frame/subscribe [[:control :uid]])]
    (fn []
      [re-com/v-box
       :children [
                  [re-com/v-split
                   :width "100%"
                   :height "100vh"
                   :class "split-vertical"
                   :style {:border "0px"
                           :margin "0px"}
                   :initial-split "80%"
                   :panel-1
                   (if-let [uid @registered-uid]
                     [re-com/v-box
                      :width "100%"
                      :height "90%"
                      :children [[title]
                                 [work-area]
                                 ]]
                     [login-dialog])
                   :panel-2 [re-com/v-box

                             :children [[logger]]
                             :width "100%"
                             :height "100%"
                             ]
                   ]
                  ]])))
