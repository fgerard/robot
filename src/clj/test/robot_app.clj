(ns clj.test.robot-app
  (:require [clojure.data.json]))


(fn [ctx]
  (try
    (let [cameras (-> (slurp "http://vision-stream:8045/cameras")
                      (clojure.data.json/read-str :key-fn keyword)
                      :Cameras)
          running (-> (slurp "http://vision-stream:8045/running/cameras")
                      (clojure.data.json/read-str :key-fn keyword)
                      :cameras
                      set)
          ips (into [] (->> cameras
                            (filter #(running (:name %)))
                            (map (fn [{:keys [alias videoURL]}]
                                   (let [[_ ip] (re-find #".*@([0-9]+\.[0-9]+\.[0-9]+\.[0-9]+).*" videoURL)]
                                     [alias ip])))))]
      {:ips (into [] (take 5 ips)) :ips-cnt (count ips) :camara-mala #{}})
    (catch Exception e (str "Error: " (.getMessage e)))))

(fn [{:keys [ips]}]
  (let [[alias ip] (first ips)
        ips (rest (ips))]
    {:ips ips :recorre-ips (or ip :done) :recorre-ips-name alias :ips-cnt (inc (count ips))}))
