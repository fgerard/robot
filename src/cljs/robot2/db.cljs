(ns robot2.db)

(def DEF-ATTR
  {:fill "transparent"
   :stroke "#b0b0b0"
   :strokeWidth 3
   :classname "draggable"})

;; Forma del estado raiz (resumen):
;;
;; :url-base          string, base de las llamadas REST/sente
;; :ui/main-tab        :console | :designer | :users   (antes vivia en un atomo reagent local)
;; :control            {:uid ... :admin ...}
;; :communication      handle de sente (chsk, ch-recv, send-fn, state)
;; :log                lista de [ts level status msg millis]
;; :users              {email {:hpass ... :admin ...}}
;; :operations/schema  catalogo de tipos de operacion, cargado en runtime desde operations-schema.edn
;; :operations         catalogo crudo que regresa el backend (/operations)
;; :applications
;;   :stored           #{app-name}
;;   :loaded           #{app-name}
;;   :ready            {app-name {inst-name instance-status}}
;;   :editable         {app-name {:app-params {} :instances {} :init-state kw :states {} :svg-ctrl {...}}}
;;   :ctrl             {:loaded {:selected #{}} :stored {:selected #{}} :ready {:selected #{} :instances #{}}}
;; :designer/ctrl      {:app nil :new-app nil :new-inst nil :import nil}
;; :history            {:curr-idx 0 :snapshots (list db ...)}

(def default-db
  {:name "Robot 2.0"
   :ui/main-tab :console
   :users {}
   :log (list)
   :operations {}
   :operations/schema nil
   :applications {:stored #{}
                   :loaded #{}
                   :ready {}
                   :editable {}
                   :ctrl {:loaded {:selected nil}
                          :stored {:selected nil}
                          :ready {:selected nil
                                  :instances nil}}}
   :designer/ctrl {:app nil
                    :new-app nil
                    :import nil}})
