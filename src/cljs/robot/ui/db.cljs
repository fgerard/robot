(ns robot.ui.db
  (:require [goog.string :as gstring]
            [goog.string.format]
            [fipp.edn :refer (pprint) :rename {pprint fipp}]))

(def DEF-ATTR {:fill "transparent"
               :stroke "#b0b0b0"
               :strokeWidth 3
               :classname "draggable"})

(comment
 [:account-stats :anomaly-by-percentile :anomaly-by-stdev :batch :by :changed :concurrent-metter
  :counter :debug :decorated :default :dump-every :elastic-store :error :ewma-timeless :forward
  :info :join :kafka-send :matcher :mixer :moving-time-window :percentiles :printe :rate
  :reduce-with :reinjected :remove :rollup :smap :split :store :time-stampit :to-file :unfold
  :warn :where :with])

#_(vec
   (map
    (fn [n]
      (let [n (name n)
            l (count n)]

        (if (and (> l 5) (= \. (nth n (- l 4))))
          (do
            (keyword (subs n 0 (- l 5))))
          (keyword n))))
    s))

#_(pprint
   (reduce
    (fn [r k]
      (let [n (name k)
            names (map (fn [i]
                         (str n i ".svg")) (range 1 5))]
        (assoc r k (vec names))))
    {}
    [:account-stats :anomaly-by-percentile :anomaly-by-stdev :batch :by1 :changed :concurrent-metter
     :counter :debug :decorated :default :dump-every :elastic-store :error :ewma-timeless :forward
     :info :join :kafka-send :matcher :mixer :moving-time-window :percentiles :printe :rate
     :reduce-with :reinjected :remove :rollup :smap :split :store :time-stampit :to-file :unfold
     :warn :where :with]))


(def default-db
  {:name "iwRobot 1.0.2"
   :main-tab {:selected :designer}
   :users {"fgerard@interware.com.mx" {:roles []}
           }
   :applications {:ctrl-x {:stored {:selected nil} ;"prueba2"}
                         :loaded {:selected nil} ;"prueba2"}
                         :ready {:selected nil ;"prueba2"
                                 :instances nil}} ;#{"uno" "tres"}}}
                  :stored-x ["prueba1" "prueba2" "prueba3"]
                  :loaded-x ["prueba2" "prueba3"],
                  :ready-x {"prueba3" {"uno" {:robot/status :stopped,
                                              :robot/mood {},
                                              :robot/current :prueba}}}
                  :editable-x {"nueva" {:app-params {}
                                        :instances {}
                                        :init-state nil
                                        :states {}}
                               "prueba2" {:svg-ctrl {:zoom {:x 0 :y 0 :w 1342 :h 600}}
                                          :app-params {:delta 5000, :r-count 10},
                                          :instances {"uno" {:uri "localhost",
                                                             :puerto 22,
                                                             :r-count 3
                                                             :mhost "smtp.gmail.com"
                                                             :muser "una_cuenta_asignada_al_robot@gmail.com"
                                                             :mpass "y su respectivo password"
                                                             :msubject "Mensaje del nuevo robot"}},
                                          :init-state :inicio,
                                          :states {:inicio
                                                   {:operation :sleep,
                                                    :conf {:delay :delta},
                                                    :diagram {:corner [100 100]
                                                              :links [[:prueba 50]]},
                                                    :flow [[:prueba]]},
                                                   :prueba
                                                   {:operation :socket,
                                                    :conf
                                                    {:host :uri,
                                                     :port :puerto,
                                                     :retry-count :r-count,
                                                     :retry-delay 2000,
                                                     :timeout 500},
                                                    :diagram {:corner [200 100]
                                                              :links [[:mal 50] [:get-it 50]]},
                                                    :flow [[:mal "Exception"] [:get-it]]},
                                                   :get-it
                                                   {:operation :get-http,
                                                    :conf {:uri "http://www.google.com"
                                                           :retry-count 3
                                                           :retry-delay 5000
                                                           :timeout 1000}
                                                    :center [300 300]
                                                    :flow [[:bien "^302"] [:mal]]}
                                                   :bien
                                                   {:operation :switch-good
                                                    :conf {}
                                                    :center [0 0]
                                                    :flow [[:inicio]]}
                                                   :mal
                                                   {:operation :switch-bad
                                                    :conf {:minutes 1}
                                                    :center [0 0]
                                                    :flow [[:mail "send"] [:hora]]
                                                    }
                                                   :mail
                                                   {:operation :send-mail,
                                                    :conf {:from :muser
                                                           :to ["fgerard@interware.com.mx"]
                                                           :subject :msubject
                                                           :body "Mensaje de la app :robot/app en la instancia :robot/instance current :robot/current status :robot/status mood :robot/mood"
                                                           :host :mhost
                                                           :user :muser
                                                           :pass :mpass
                                                           :ssl true
                                                           :retry-count :r-count,
                                                           :retry-delay 2000,
                                                           :timeout 20000},
                                                    :center [150 150],
                                                    :flow [[:hora]]}
                                                   :http
                                                   {:operation :http,
                                                    :conf {:url "http://www.interware.com.mx/login"
                                                           :method "POST"
                                                           :params {"user" "test"
                                                                    "password" "123456"}
                                                           :retry-count :r-count,
                                                           :retry-delay 2000,
                                                           :timeout 20000},
                                                    :center [150 150],
                                                    :flow [[:hora]]}
                                                   :hora
                                                   {:operation :date-time
                                                    :conf {:format "HH:mm"}
                                                    :center [200 200]
                                                    :flow [[:inicio]]}}}
                             }}
   }) ;{[1 2] {:type :path :cmd ""}}
