(ns robot.util.basic
  (:require [clojure.tools.logging :as log]
            [clojure.string :as S])
  (:import
    (javax.mail Flags Flags$Flag Folder Message Session Store)
    (java.util Properties)))



(defn get-store [session protocol host port email password]
  (println "1." (pr-str host) (class host))
  (println "2." (pr-str port) (class port))
  (println "3." (pr-str email) (class email))
  (println "4." (pr-str session))
  (println "5." (pr-str protocol) (class protocol))
  (println "6." (.getStore session protocol))
  (doto (.getStore session protocol) (.connect host (Integer/parseInt (str port)) email password)))

(defn content2str [content]
  (subs (str ":" content) 1))

(defn open-folder
  ([store folder-name rw]
   (doto (.getFolder store folder-name) (.open (if rw Folder/READ_WRITE Folder/READ_ONLY))))
  ([store folder-name]
   (open-folder store folder-name false)))

(defmulti create-session (fn [proto _ _ _] (cond (S/starts-with? proto "pop") :pop3
                                               (S/starts-with? proto "imap"):imap)))

(defmethod create-session :pop3 [protocol debugging ssl host]
  (let [sf (doto (com.sun.mail.util.MailSSLSocketFactory.)
             (.setTrustedHosts (into-array String [host])))
        props (Properties.)
        _ (println "SSL? " ssl)
        props (doto props
                (.put (str "mail." protocol ".ssl.socketFactory") sf)
                (.put (str "mail." protocol ".ssl.enabled") (if ssl "true" "false")))
        session (Session/getInstance props nil)]
    (.setDebug session debugging)
    session))

(defmethod create-session :imap [protocol debugging ssl host]
  (let [sf (doto (com.sun.mail.util.MailSSLSocketFactory.)
             (.setTrustedHosts (into-array String [host])))
        props (Properties.)
        _ (System/setProperty "javax.net.debug" "all")
        _ (println "SSL? " ssl)
        props (doto props
                (.put (str "mail." protocol ".ssl.socketFactory") sf)
                (.put (str "mail." protocol ".ssl.enabled") (if ssl "true" "false"))
                (.put "mail.store.protocol" protocol))
        _ (log/info props)
        session (Session/getInstance props nil)]
    (.setDebug session debugging)
    session))

(defn arr-to-str [sep arr]
  (reduce
    (fn [a b]
      (let [addr (str (.getAddress b))]
        (str (if (nil? a) addr (str a sep addr))))) nil arr))

(defn extract-message-from-folder [folder idx]
  (let [msg (.getMessage folder idx)
        subject (.getSubject msg)]
    {:from    (arr-to-str "," (.getFrom msg))
     :subject subject
     :content (content2str (.getContent msg))
     :msg     msg}))

(defn get-msg-match
  ([folder cond-fn deleting]
   (let [msg-count (.getMessageCount folder)
         _ (log/debug (pr-str {:folder folder :msg-count msg-count}))]
     (if (> msg-count 0)
       (loop [msgcount msg-count
              n msgcount
              mmap (extract-message-from-folder folder n)]
         (log/debug (str n ") " (:subject mmap) "- [" (:from mmap) "] -" (keys mmap)))
         (if (cond-fn mmap)
           (do
             (log/debug (str "se encontro:" (:subject mmap)))
             (if deleting (.setFlag (:msg mmap) Flags$Flag/DELETED true))
             (dissoc mmap :msg))
           (if (or (= n 1) (> (- msgcount n) 10))
             nil
             (recur msgcount (dec n) (extract-message-from-folder folder (dec n)))))))))
  ([folder cond-fn]
   (get-msg-match false)))

(defn get-mail [{host :host port :port protocol :protocol ssl :ssl email :email password :password folder-name :folder
                 cond-fn :cond-fn debug :debug :or {debug false folder-name "INBOX" protocol "pop3s"} :as pp}]
  (with-open [store (get-store
                     (create-session protocol debug ssl host) protocol host port email password)]                                        
    (let [folder (open-folder store folder-name true)]
      (try
        (get-msg-match folder cond-fn true)
        (finally
          (.close folder true))))))

(defn get-mail-with-subject-and-from-matching
  [{host       :host port :port protocol :protocol email :ssl ssl :email password :password folder-name :folder
    subject-re :subject-re from-re :from-re debug :debug :or {debug false folder "INBOX" protocol "pop3s" from-re ""} :as params}]
  (log/debug (pr-str {:params params}))
  (letfn [(find-subject-and-from [message]
            (let [subject (re-find (re-pattern subject-re) (:subject message))
                  from    (re-find (re-pattern from-re) (:from message))
                  _ (log/debug (pr-str {:from from :subject subject}))]
              (and subject from)))]
    (get-mail (assoc params :cond-fn find-subject-and-from))))

(defn get-mail-with-subject-matching
  [{host       :host port :port protocol :protocol email :ssl ssl :email password :password folder-name :folder
    subject-re :subject-re debug :debug :or {debug false folder "INBOX" protocol "pop3s"} :as params}]
  (log/debug (pr-str {:params params}))
  (get-mail (assoc params :cond-fn (fn [m] (re-matches (re-pattern subject-re) (:subject m))))))

(defn get-mail-with-subject-in-set
  [host port protocol email password subject-set]
  (get-mail {:host    host :port port :protocol protocol
             :email   email :password password
             :cond-fn (fn [m] (subject-set (:subject m)))}))

(defn play-sound [file-path]
  (let [stream (javax.sound.sampled.AudioSystem/getAudioInputStream (java.io.File. file-path))
        format (.getFormat stream)
        info (javax.sound.sampled.DataLine$Info. javax.sound.sampled.Clip format)
        clip (javax.sound.sampled.AudioSystem/getLine info)]
    (.addLineListener clip (reify javax.sound.sampled.LineListener
                             (^void update [this ^javax.sound.sampled.LineEvent event]
                               (when (= javax.sound.sampled.LineEvent$Type/STOP (.getType event))
                                 (.close clip)))))
    (doto clip (.open stream) .start)))
