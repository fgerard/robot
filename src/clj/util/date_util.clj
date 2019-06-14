(ns util.date-util
  (:import (java.text SimpleDateFormat)
           (java.util Calendar)))

(def YMD_PATTERN "yyyy-MM-dd")

(def YMDHMS_PATTERN "yyyy-MM-dd'T'HH:mm:ss")

(defn format-ymdhms [date]
  (let [ymdhms-format (new SimpleDateFormat YMDHMS_PATTERN)
        ymdhms-str    (.format ymdhms-format date)]
    ymdhms-str))

(defn parse-ymdhms [date-str]
  (let [ymdhms-format (new SimpleDateFormat YMDHMS_PATTERN)
        date       (.parse ymdhms-format date-str)]
    date))


(defn format-ymd [date]
  (let [ymd-format (new SimpleDateFormat YMD_PATTERN)
        ymd-str    (.format ymd-format date)]
    ymd-str))

(defn parse-ymd [date-str]
  (let [ymd-format (new SimpleDateFormat YMD_PATTERN)
        date       (.parse ymd-format date-str)]
    date))

(defn past-date [days-offset]
  (let [cal  (Calendar/getInstance)
        _    (.add cal (Calendar/DAY_OF_YEAR) (- days-offset))
        date (.getTime cal)]
    date))

(defn parse-ymdThms-local->Date [date-str]
  (.parse (java.text.SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss") date-str))

(defn format-ymdThms-local->String [date]
  (subs (.format (java.text.SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss zzz") date) 0 19))

(def zone-id (java.time.ZoneId/of "America/Mexico_City"))

(defn hoy
  ([]
   (hoy 0))
  ([dias]
   (->
    (java.time.LocalDate/now zone-id)
    (.plusDays dias))))

(defn create-list-of-dates [end-date days-in-past]
  (mapv
   (fn [back]
     (let [day (.minusDays end-date back)]
       (.toString day)))
   (range 0 (inc days-in-past))))

(defn ahora
  ([]
   (ahora 0))
  ([horas]
   (->
    (java.time.LocalDateTime/now zone-id)
    (.plusHours horas))))

(defn create-list-hourly-dates [end-date hours-in-past]
  (let [fmt (java.time.format.DateTimeFormatter/ofPattern "yyyyMMdd-HH")]
    (mapv
     (fn [back]
       (let [day (.minusHours end-date back)]
         (.format day fmt)))
     (range 0 (inc hours-in-past)))))
