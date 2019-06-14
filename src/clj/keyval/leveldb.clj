(ns keyval.leveldb
  (:refer-clojure  :exclude [get])
  (:require [byte-streams :as bs]
            [integrant.core :as ig]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log])
  (:import (org.fusesource.leveldbjni
            JniDBFactory)
           (org.iq80.leveldb
            WriteBatch
            DBIterator
            Options
            ReadOptions
            WriteOptions
            CompressionType
            DB
            Range)))


 ; HawtJNI tends to leave trash in /tmp, repeated loading of this
 ;; namespace can add up.  We allow the 10 newest files to stick
 ;; around in case there's some sort of contention, since we're only
 ;; trying to put an upper bound on how many of these files stick around.
 (let [tmp-dir (str
                 (System/getProperty "java.io.tmpdir")
                 (System/getProperty "file.separator")
                 "com.factual.clj-leveldb")]
   (let [d (io/file tmp-dir)]
     (log/info "Purging leveldb tmp files @ " (.getCanonicalPath d))
     (doseq [f (->> (.listFiles d)
                 (sort-by #(.lastModified %))
                 reverse
                 (drop 10))]
       (log/info "Purging " (.getName f))
       (.delete f)))
   (System/setProperty "library.leveldbjni.path" tmp-dir))


(defn key-encoder [s]
  (pr-str s))

(defn key-decoder [bytes]
  (if bytes
    (read-string (bs/to-string bytes))))

(defn key-comparator [k1 k2]
  (.compareTo k1 k2))

(defn val-encoder [val]
  (if-not (nil? val)
    (pr-str val)))

(defn val-decoder [bytes]
  (when bytes
    (read-string (bs/to-string bytes))))

(def ^:private option-setters
  {:create-if-missing? #(.createIfMissing ^Options %1 %2)
   :error-if-exists?   #(.errorIfExists ^Options %1 %2)
   :write-buffer-size  #(.writeBufferSize ^Options %1 %2)
   :block-size         #(.blockSize ^Options %1 %2)
   :block-restart-interval #(.blockRestartInterval ^Options %1 %2)
   :max-open-files     #(.maxOpenFiles ^Options %1 %2)
   :cache-size         #(.cacheSize ^Options %1 %2)
   :comparator         #(.comparator ^Options %1 %2)
   :paranoid-checks?   #(.paranoidChecks ^Options %1 %2)
   :compress?          #(.compressionType ^Options %1 (if % CompressionType/SNAPPY CompressionType/NONE))
   :logger             #(.logger ^Options %1 %2)})

(defn create-leveldb
  [directory
   {:keys [create-if-missing?
           error-if-exists?
           write-buffer-size
           block-size
           max-open-files
           cache-size
           comparator
           compress?
           paranoid-checks?
           block-restart-interval
           logger]
    :or {compress? true
         cache-size (* 256 1024 1024)
         block-size (* 16 1024)
         write-buffer-size (* 32 1024 1024)
         create-if-missing? true
         error-if-exists? false}
    :as options}]
  (.open JniDBFactory/factory
    (io/file directory)
    (let [opts (Options.)]
      (doseq [[k v] options]
        (when (and v (contains? option-setters k))
          ((option-setters k) opts v)))
      opts)))

(defn stats [leveldb]
  (.getProperty leveldb "leveldb.stats"))

(defn destroy-leveldb
  [directory]
  (.destroy JniDBFactory/factory
    (io/file directory)
    (Options.)))

(defn repair-leveldb
  [directory]
  (.repair JniDBFactory/factory
    (io/file directory)
    (Options.)))

(defn get
  ([leveldb k]
   (val-decoder
    (.get leveldb (bs/to-byte-array (key-encoder k)))))
  ([leveldb k default-val]
   (if-let [val (get leveldb k)]
     val
     default-val)))

(defn put [leveldb k v]
  (.put leveldb
        (bs/to-byte-array (key-encoder k))
        (bs/to-byte-array (val-encoder v))))

(defn delete [leveldb k]
  (.delete leveldb (bs/to-byte-array (key-encoder k))))

(defn iterator [leveldb]
  (.iterator leveldb))

(defn peek-next-key [iterator]
  (-> iterator .peekNext key key-decoder))

(defn swap_it! [leveldb k upd-fn & args]
  (try
    (let [valor (get leveldb k)
          new-val (apply upd-fn valor args)]
      (if new-val
        (if (not= new-val valor)
          (put leveldb k new-val))
        (delete leveldb k)))
    (catch Exception e
      (.printStackTrace e))))

(defn reduce-db [leveldb reduction-fn init [start end]]
  (let [iter (doto
              (iterator leveldb))]
    (try
      (let [_ (if start
                (.seek iter (bs/to-byte-array (key-encoder start)))
                (.seekToFirst iter))
            iter-seq (repeatedly (fn []
                                   (if (.hasNext iter)
                                     (let [[k v] (.next iter)
                                           k (key-decoder k)
                                           v (val-decoder v)]
                                       (if (or (not end) (< (key-comparator k end) 0))
                                         [k v])))))
            reduction-seq (take-while identity iter-seq)]
        (reduce reduction-fn init reduction-seq))
    (finally
      (.close iter)))))

(defn get-key-range-starting [leveldb prefix-str]
  (let [iter (doto
              (.iterator leveldb)
              (.seek (.getBytes prefix-str "UTF-8")))]
    (try
      (let [iter-seq (repeatedly (fn []
                                   (if (.hasNext iter)
                                     (let [[k v] (.next iter)
                                           k (bs/to-string k)]
                                       (if (.startsWith k prefix-str)
                                         k)))))
            k-seq (take-while identity iter-seq)]
        (into [] (map key-decoder k-seq)))
      (finally
        (.close iter)))))

(defn- intern-create-kv-store [db-path]
  (create-leveldb db-path {:create-if-missing? true}))

(def create-kv-store (memoize intern-create-kv-store))

(defmethod ig/init-key :keyval.leveldb/factory [_ {:keys [db-path]}]
  (create-kv-store db-path))
