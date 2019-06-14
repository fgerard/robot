(ns robot.migrate.util
  (:require [clojure.pprint :as pp]
            [clojure.string :as S]))


(defn migrate-QM-cliente [app-store3-path new-robot-edn-path]
  (let [app-store (read-string (slurp app-store3-path))
        app-param-keys [:channel :hostname :port :qmanager]
        app-names (filter #(.startsWith (name %) "Q") (-> app-store :configuration keys))
        _ (println :app-names (pr-str app-names))
        app-paths (reduce
                   (fn [paths app-key]
                     (conj paths [:configuration app-key]))
                   []
                   app-names)
        _ (println :app-paths (pr-str app-paths))
        app-params (reduce
                    (fn [apps-params app-path]
                      (let [app (get-in app-store app-path)
                            app-params (:parameters app)
                            inst-params (reduce
                                         (fn [params param-k]
                                           (assoc params param-k (str (param-k app-params))))
                                         {}
                                         app-param-keys)
                            app-name (S/replace (:qmanager inst-params) "." "_")
                            ;app (-> apps-params (get app-name {}))
                            q-idx (-> apps-params (get app-name {}) (get :queue-count 0))
                            queues (into
                                    {}
                                    (map-indexed
                                     (fn [idx [inst-k {:keys [param-map]}]]
                                       [(keyword (format "Q%02d" (+ (inc idx) q-idx))) (:queue param-map)])
                                     (:instances app)))
                            q-idx (+ q-idx (count (:instances app)))
                            _ (println :queues queues)
                            ]
                        (update apps-params app-name #(merge % inst-params queues {:queue-count q-idx}))))
                    {}
                    app-paths)
        new-app (-> (read-string (slurp new-robot-edn-path))
                    (assoc :instances app-params))
        ]
    (spit (str new-robot-edn-path ".2") (with-out-str (pp/pprint new-app)))
    (pp/pprint new-app)))


;(-> a :configuration :QM_UDIS :parameters keys)
;(:email :service-route :password :- :channel :mail-host :hostname :mail-port :port :recipients :qmanager :file-route :user)

;(-> a :configuration :QM_UDIS :instances :QL_RES_UDIS :param-map :queue)
