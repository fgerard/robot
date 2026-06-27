(ns keyval.konservedb
  (:refer-clojure  :exclude [get])
  (:require [byte-streams :as bs]
            [integrant.core :as ig]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [clojure.java.shell :as shell]
            [konserve.filestore :refer [new-fs-store list-keys]]
            [konserve.core :as k]
            [clojure.core.async :as async :refer [<!! go <!]]))

(defn create-konservedb
  [directory]
  (<!! (new-fs-store directory)))

(defn destroy-konservedb
  [directory]
  (log/warn "Please delete manualy this directory to erase a konserve db: " directory))

(defn get
  ([konservedb k]
   (<!! (k/get-in konservedb k)))
  ([konservedb k default-val]
   (<!! (k/get-in konservedb k default-val))))

; :apps :status :running-info :allowed-users

; [:apps id-app]
; [:running-info app-id inst-id]
; [:stats app-id inst-id]
; [:allowed-users]

(defn put [konservedb k v]
  (go
   (let [[old new] (<! (k/update-in konservedb k (fn [_] v)))]
     (log/info :new " value stored in " k)
     (if (= (first k) :apps)
       (<! (k/update konservedb :id-apps (fn [apps]
                                           (conj (or apps #{}) (second k)))))))))

(defn delete [konservedb k]
  (go
   (<! (k/assoc-in konservedb k nil))
   (if (= (first k) :apps)
     (<! (k/update konservedb :id-apps (fn [apps]
                                         (disj (or apps #{}) (second k))))))))

(defn get-apps [konservedb]
  (sort
   (<!! (k/get konservedb :id-apps))))

(defmethod ig/init-key :keyval.konservedb/factory [_ {:keys [db-path]}]
  (create-konservedb db-path))
