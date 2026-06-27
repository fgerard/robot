(fn [{:keys [output-path relevantes-date value] :as ctx}]
  (letfn [(format-time [timestamp]
            (let [formatter (java.text.SimpleDateFormat. "HH_mm_ss")
                  tz (java.util.TimeZone/getTimeZone "GMT-06:00")]
              (.setTimeZone formatter tz)
              (.format formatter (java.util.Date. timestamp))))

          (format-timestamp [timestamp]
            (let [instant (java.time.Instant/ofEpochMilli timestamp)
                  zone-id (java.time.ZoneId/of "GMT-06:00")
                  zoned-time (java.time.ZonedDateTime/ofInstant instant zone-id)
                  formatter (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd_HH.mm.ss")]
              (.format zoned-time formatter)))

          (parse-time [time-str]
            (.getTime (clojure.instant/read-instant-date time-str)))

          (base64-to-bytes
            [b64-string]
            (.decode (java.util.Base64/getDecoder) (.getBytes b64-string "UTF-8")))

          (save-image
            [b64-string output-path]
            (with-open [out (clojure.java.io/output-stream output-path)]
              (.write out (base64-to-bytes b64-string)))
            (println (str "✅ Imagen guardada en: " output-path)))

          (lista-archivos [path]
            (let [dir (clojure.java.io/file path)]
              (when (.isDirectory dir)
                (.listFiles dir))))

          (purge-tmp-files [path]
            (let [tmp-files (lista-archivos path)]
              (doseq [f tmp-files]
                (when (and (.isFile f)
                           (or (.endsWith (.getName f) ".jpg")
                               (.endsWith (.getName f) ".png")))
                  (try
                    (.delete f)
                    (println (str "🗑️ Archivo temporal eliminado: " (.getAbsolutePath f)))
                    (catch Exception e
                      (println (str "No se pudo eliminar el archivo temporal: " (.getAbsolutePath f) " Error: " (.getMessage e)))))))))

          (filtrar-registros [path archivo d-value]
            (purge-tmp-files path)
            (let [image-paths (with-open [rdr (clojure.java.io/reader archivo)]
                                (reduce
                                 (fn [result line]
                                   (let [m (clojure.edn/read-string line)
                                         {:keys [value accuracy type http-status aiTime camera clip image]} m
                                         type (name type)
                                         T (format-time  aiTime)]
                                     (if (and aiTime clip (=  (.toLowerCase d-value) (.toLowerCase value)))
                                       (let [clip-file (str path "/" camera "-" value "-" (format-timestamp aiTime) "-" accuracy  ".jpg")
                                             image-file (str path "/" camera "-" value "-" (format-timestamp aiTime) ".jpg")]
                                         (println aiTime " " value " " accuracy " " type " "  http-status " " camera)
                                         (save-image clip clip-file)
                                         (when image (save-image image image-file))
                                         (concat result [image-file clip-file]))
                                       result)))
                                 []
                                 (line-seq rdr)))]
              image-paths))]
    (let [relevant-file (format "/opt/quantum/event-stream/data/relevantes/relevantes-%s.edn.txt" relevantes-date)
          scan-vector (into [] (filtrar-registros output-path relevant-file value))]
      {:scan-vector scan-vector
       :scan (count scan-vector)
       :relevant-file relevant-file})))
