(ns robot2.users.views
  "Panel de administracion de usuarios (pestana Configuration). Antes era
   `edit-users` dentro de robot.ui.robot-control."
  (:require [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [re-com.core :as re-com]
            [cljs-hash.goog :as gh]
            [robot2.undo :as undo]))

(def EMAIL-RE #"^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$")

(re-frame/reg-sub :users (fn [db _] (:users db)))

(re-frame/reg-event-fx
  :users/add!
  [undo/track]
  (fn [{:keys [db]} [_ {:keys [email pass hpass admin]}]]
    (let [hpass (if (and (not (seq pass)) hpass) hpass (gh/sha1-hex pass))]
      {:db (assoc-in db [:users email] {:hpass hpass :admin admin})
       :dispatch [:api/save-users]})))

(re-frame/reg-event-fx
  :users/remove!
  [undo/track]
  (fn [{:keys [db]} [_ email]]
    {:db (update db :users dissoc email)
     :dispatch [:api/save-users]}))

(defn- new-user-form [entry]
  [re-com/h-box
   :class "input-container"
   :children
   [[re-com/gap :size "0.5em"]
    [re-com/input-text
     :placeholder "User email" :width "15rem" :height "2rem" :class "mail-input"
     :model (:email @entry) :attr {:max-length "60"} :status-icon? false :change-on-blur? false
     :status (when (seq (:email @entry)) (if (re-matches EMAIL-RE (:email @entry)) :success :error))
     :on-change (fn [txt] (swap! entry assoc :email txt))]
    [re-com/gap :size "3rem"]
    [re-com/input-text
     :placeholder "User password" :width "15rem" :height "2rem" :class "mail-input"
     :attr {:max-length "20"} :input-type :password
     :model (:pass @entry "") :status-icon? false :change-on-blur? false
     :on-change (fn [txt] (swap! entry assoc :pass txt))]
    [re-com/gap :size "3rem"]
    [re-com/checkbox
     :model (:admin @entry) :label "admin"
     :on-change (fn [v] (swap! entry assoc :admin v))]
    [re-com/md-icon-button
     :class "btn-add-user" :md-icon-name "zmdi-plus" :tooltip "Add user"
     :disabled? (or (nil? (seq (:email @entry))) (not (re-matches EMAIL-RE (:email @entry))))
     :on-click (fn []
                 (re-frame/dispatch [:users/add! @entry])
                 (reset! entry {:email "" :pass ""}))]]])

(defn- user-row [entry email {:keys [hpass admin]}]
  ^{:key (str email)}
  [re-com/h-box
   :class "user-row"
   :children
   [[re-com/label :class "user" :label email :width "15rem"
     :on-click (fn [] (reset! entry {:email email :hpass hpass :admin admin}))]
    [re-com/gap :size "3rem"]
    [re-com/label :class "password" :label "*********" :width "14rem"
     :on-click (fn [] (reset! entry {:email email :hpass hpass :admin admin}))]
    [re-com/gap :size "1rem"]
    [re-com/checkbox
     :model admin :label "admin"
     :on-change (fn [v] (re-frame/dispatch [:users/add! {:email email :hpass hpass :admin v}]))]
    [re-com/gap :size "1rem"]
    [re-com/md-icon-button
     :md-icon-name "zmdi-delete" :class "btn-remove" :tooltip "Remove user" :size :smaller
     :on-click (fn [] (re-frame/dispatch [:users/remove! email]))]
    [re-com/gap :size "2rem"]]])

(defn users-tab []
  (let [users (re-frame/subscribe [:users])
        entry (reagent/atom {:email "" :pass "" :admin false})]
    (re-frame/dispatch [:api/load-users])
    (fn []
      [re-com/h-box
       :height "100%" :width "100%"
       :children
       [[re-com/gap :size "1em"]
        [re-com/border
         :border "1px" :radius "5px" :width "44em" :height "85%" :class "config-tab"
         :child
         [re-com/v-box
          :width "100%"
          :children
          [[re-com/gap :size "1em"]
           [re-com/h-box
            :children [[re-com/gap :size "1em"]
                       [re-com/v-box
                        :width "100%"
                        :children [[:div {:class "title"} "Users"]
                                   [re-com/h-box
                                    :class "btn-container"
                                    :children [[re-com/md-icon-button :class "btn-undo" :md-icon-name "zmdi-undo" :tooltip "Undo"
                                                :on-click (fn [] (re-frame/dispatch [:undo]))]
                                               [re-com/gap :size "0.5em"]
                                               [re-com/md-icon-button :class "btn-redo" :md-icon-name "zmdi-redo" :tooltip "Redo"
                                                :on-click (fn [] (re-frame/dispatch [:redo]))]
                                               [re-com/gap :size "0.5em"]]]]]]]
           [re-com/gap :size "1em"]
           [new-user-form entry]
           [re-com/scroller
            :h-scroll :off :v-scroll :on :height "20em"
            :child [re-com/v-box
                    :children (doall (for [[email u] (sort (into [] @users))] (user-row entry email u)))]]]]]]])))
