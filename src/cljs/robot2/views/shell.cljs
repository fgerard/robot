(ns robot2.views.shell
  "Cascaron de la app: barra de titulo, login, logger, y el switch entre las
   3 pestanas (Console/Designer/Configuration). Antes robot.ui.views, mas el
   `defmulti main-tab-panel` de robot.ui.robot-control -- aqui la pestana
   activa vive en el app-db (:ui/main-tab) en vez de un atomo reagent local,
   asi que es inspeccionable/serializable igual que el resto del estado."
  (:require [reagent.core :as reagent]
            [reagent.dom :refer [force-update-all dom-node]]
            [re-frame.core :as re-frame]
            [re-com.core :as re-com]
            [robot2.events.core :refer [create-log]]
            [robot2.interop :as interop]
            [robot2.console.views :as console]
            [robot2.designer.shell :as designer]
            [robot2.users.views :as users]))

(force-update-all)

(defn- app-title []
  [:img {:src "images/logo-header.png" :alt "Robot"}])

(defn- user []
  (let [uid (re-frame/subscribe [:control/uid])]
    (fn [] [re-com/title :label @uid :level :level4 :class "user-email"])))

;; floating? true -> boton flotante fijo en la esquina (pantalla de login, que
;; no tiene header); floating? false -> boton normal dentro del flujo del
;; header, junto al logout (evita que el flotante se encime con el logo).
(defn- theme-toggle [floating?]
  (let [theme (re-frame/subscribe [:ui/theme])]
    (fn [floating?]
      (let [dark? (= @theme "dark")]
        [:button
         {:type "button"
          :class (str "theme-toggle-btn" (when-not floating? " theme-toggle-btn--inline"))
          :title (if dark? "Cambiar a modo claro" "Cambiar a modo oscuro")
          :on-click (fn [] (re-frame/dispatch [:ui/set-theme (if dark? "light" "dark")]))}
         [:i {:class (str "zmdi " (if dark? "zmdi-sun" "zmdi-brightness-2"))}]]))))

(defn- title []
  [re-com/h-box
   :width "100%" :class "header"
   :children
   [[re-com/h-box :class "logo-container" :children [[app-title]]]
    [:div {:class "user-info"}
     [re-com/h-box
      :children [[user]
                 [theme-toggle false]
                 [re-com/md-icon-button
                  :class "logout-btn" :md-icon-name "zmdi-directions-run"
                  :tooltip "Logout" :tooltip-position :right-center
                  :on-click (fn [] (re-frame/dispatch [:api/logout]))]
                 [re-com/gap :size "5px"]]]]]])

(defn- g-button [on-success on-error]
  (reagent/create-class
    {:component-did-mount (fn [this] (interop/google-render-button! (dom-node this) on-success on-error))
     :reagent-render (fn [_ _] [:div {:style {:margin-bottom "1em" :margin-right "0.4em"}}])}))

(defn- login-dialog []
  (let [form-data   (reagent/atom {:email "" :pass ""})
        show-pass?  (reagent/atom false)
        process-ok  (fn [event]
                      (.preventDefault event)
                      (re-frame/dispatch [:api/login {:uid (:email @form-data) :pass (:pass @form-data)}])
                      false)]
    (fn []
      [re-com/box
       :align :center :justify :center :class "login-dialog-container"
       :child
       [re-com/h-box
        :justify :center
        :children
        [[re-com/gap :size "300px" :width "30%"]
         [re-com/border
          :class "login-dialog-box"
          :child
          [:form {:on-submit process-ok}
           [re-com/v-box
            :class "login-dialog-v-box"
            :children
            [[:img {:src "images/logo-login.png" :class "logo"}]
             [re-com/gap :size "1em"]
             [re-com/v-box
              :class "form-group"
              :children [[re-com/input-text
                          :model (:email @form-data) :placeholder "Enter email" :class "form-control email"
                          :change-on-blur? true
                          :attr {:id "pf-email" :type "email" :required "required" :max-length "60"}
                          :on-change #(swap! form-data assoc :email %)]]]
             [re-com/v-box
              :class "form-group"
              :children [[re-com/h-box
                          :align :center :style {:position "relative"}
                          :children [[re-com/input-text
                                      :model (:pass @form-data) :placeholder "Enter password"
                                      :class "form-control password" :width "100%"
                                      :change-on-blur? false
                                      :attr {:id "pf-password"
                                             :type (if @show-pass? "text" "password")
                                             :max-length "20" :required "required"}
                                      :on-change #(swap! form-data assoc :pass %)]
                                     [:i {:class (if @show-pass? "zmdi zmdi-eye" "zmdi zmdi-eye-off")
                                          :title (if @show-pass? "Ocultar contraseña" "Ver contraseña")
                                          :on-click #(swap! show-pass? not)
                                          :style {:position "absolute" :right "10px" :cursor "pointer"
                                                  :color "#888" :font-size "18px" :user-select "none"}}]]]]]
             [re-com/v-box
              :children
              [[re-com/h-box
                :width "100%" :gap "40px" :justify :center
                :children [[re-com/button :label "Login" :attr {:type "submit"} :style {:width "200px"} :class "btn-primary login-btn"]]]
               [re-com/gap :size "1.5em"]
               [re-com/h-box
                :gap "10px" :justify :center
                :children
                [[g-button
                  (fn [uid pass] (re-frame/dispatch [:api/login {:uid (str uid) :pass pass}]))
                  (fn [text] (re-frame/dispatch (create-log :error 500 text)))]]]]]]]]]]]])))

(defn- logger []
  (let [log (re-frame/subscribe [:log])
        ts-atm (reagent/atom "") level-atm (reagent/atom :all)
        status-atm (reagent/atom "") msg-atm (reagent/atom "")
        pat (fn [s] (when (seq s) (re-pattern s)))
        level-color {:info "#519E3C" :warn "#E7AE22" :error "#E3383C"}]
    (fn []
      (let [ts-pat (pat @ts-atm) status-pat (pat (str @status-atm)) msg-pat (pat @msg-atm)
            entries (filter (fn [[ts level status msg]]
                              (and (or (nil? ts-pat) (re-find ts-pat ts))
                                   (or (= :all @level-atm) (= level @level-atm))
                                   (or (nil? status-pat) (re-find status-pat (str status)))
                                   (or (nil? msg-pat) (re-find msg-pat msg))))
                            @log)]
        [re-com/v-box
         :class "logger" :width "100%" :height "95%"
         :children
         [[re-com/h-box
           :children
           [[re-com/input-text
             :placeholder "T1231" :width "20%" :height "2em" :class "transaction-filter" :change-on-blur? false
             :model @ts-atm :on-change (fn [v] (reset! ts-atm v))]
            [re-com/single-dropdown
             :choices [{:id :all} {:id :error} {:id :warn} {:id :info}] :label-fn :id
             :class "level-filter" :width "5%" :model @level-atm
             :on-change (fn [v] (reset! level-atm v))]
            [re-com/input-text
             :placeholder "200" :width "5%" :height "2em" :class "status-code-filter" :change-on-blur? false
             :model @status-atm :on-change (fn [v] (reset! status-atm v))]
            [re-com/input-text
             :placeholder ".*error.*" :width "70%" :height "2em" :class "text-filter" :change-on-blur? false
             :model @msg-atm :on-change (fn [v] (reset! msg-atm v))]]]
          [re-com/scroller
           :v-scroll :auto :height "100%"
           :child
           [re-com/v-box
            :children
            (doall
              (for [[ts level status msg millis] entries]
                ^{:key millis}
                [re-com/h-box
                 :width "100%" :style {:color (get level-color level) :font-size "12px"}
                 :children [[re-com/label :label ts :width "20%"]
                            [re-com/label :label level :width "5%"]
                            [re-com/label :label status :width "5%"]
                            [re-com/label :label msg :width "70%"]]]))]]]]))))

(defn- work-area []
  (let [admin? (re-frame/subscribe [:control/admin])
        active-tab (re-frame/subscribe [:ui/main-tab])]
    (fn []
      (let [tabs (cond-> [{:id :console :label "Console"} {:id :designer :label "Designer"}]
                   @admin? (conj {:id :users :label "Configuration"}))]
        [re-com/v-box
         :height "100%" :width "100%"
         :children
         [[re-com/horizontal-tabs
           :model @active-tab :tabs tabs :style {:color "#559"}
           :on-change (fn [tab] (re-frame/dispatch [:ui/select-tab! tab]))]
          (case @active-tab
            :console [console/inst-control]
            :designer [designer/designer-tab]
            :users [users/users-tab]
            [re-com/title :label (str "Unknown tab: " @active-tab)])]]))))

(defn- error-dialog []
  (let [err (re-frame/subscribe [:ui/error-dialog])]
    (fn []
      (when-let [{:keys [status body]} @err]
        [re-com/modal-panel
         :child
         [re-com/v-box
          :width "480px" :padding "1.5em" :gap "1em"
          :style {:background "white" :border-radius "6px"}
          :children
          [[re-com/h-box
            :align :center :gap "0.6em"
            :children [[:span {:style {:color "#c0392b" :font-size "22px"}} "⚠"]
                       [re-com/title :label (str "Error " status) :level :level3
                        :style {:color "#c0392b" :margin "0"}]]]
           [re-com/label :label body
            :style {:white-space "pre-wrap" :font-family "monospace"
                    :background "#fef0f0" :padding "0.8em" :border-radius "4px"
                    :color "#333" :max-height "300px" :overflow-y "auto"}]
           [re-com/h-box :justify :end
            :children [[re-com/button
                        :label "Cerrar"
                        :class "btn-danger"
                        :on-click (fn [] (re-frame/dispatch [:reset! [:ui/error-dialog] nil]))]]]]]
         :backdrop-on-click (fn [] (re-frame/dispatch [:reset! [:ui/error-dialog] nil]))]))))

(defn main-panel []
  (let [registered-uid (re-frame/subscribe [:control/uid])]
    (fn []
      [:<>
       (when-not @registered-uid [theme-toggle true])
       [re-com/v-split
        :width "100%" :height "100vh" :class "split-vertical" :style {:border "0px" :margin "0px"}
        :initial-split "80%"
        :panel-1 [re-com/v-box
                  :width "100%"
                  :children (if @registered-uid [[title] [work-area]] [[login-dialog]])]
        :panel-2 [re-com/v-box :width "100%" :children [[logger]]]]
       [error-dialog]])))
