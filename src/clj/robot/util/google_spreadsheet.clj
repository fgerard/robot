(ns robot.util.google-spreadsheet
  (:require [clojure.java.io :as io] 
            [clojure.tools.logging :as log]            
            [clojure.string :as S])
  (:import
   (java.io File InputStreamReader)
   (java.util Arrays)
   (com.google.api.client.googleapis.javanet GoogleNetHttpTransport)
   (com.google.api.client.json.jackson2 JacksonFactory)
   (com.google.api.client.util.store FileDataStoreFactory)
   (com.google.api.client.googleapis.auth.oauth2 GoogleClientSecrets GoogleAuthorizationCodeFlow$Builder)
   (com.google.api.client.extensions.java6.auth.oauth2 AuthorizationCodeInstalledApp)
   (com.google.api.client.extensions.jetty.auth.oauth2 LocalServerReceiver)
   (com.google.api.services.sheets.v4 Sheets$Builder)
   (com.google.api.services.sheets.v4.model BatchUpdateValuesRequest ValueRange)))

(defn append [client-secret-json oauth-store spreadsheet-id sheet-name as-row? data]
  (let [httpTransport (GoogleNetHttpTransport/newTrustedTransport)
        jsonFactory   (JacksonFactory/getDefaultInstance)]
    (letfn [(authorize []
              (let [_ (io/make-parents oauth-store)
                    dataStoreDir     (File. oauth-store)
                    dataStoreFactory (FileDataStoreFactory. dataStoreDir)
                    scopes           (Arrays/asList (into-array String ["https://www.googleapis.com/auth/spreadsheets"]))
                    clientSecrets    (GoogleClientSecrets/load jsonFactory
                                                               (InputStreamReader.
                                                                (io/input-stream client-secret-json)))
                    flow             (-> (GoogleAuthorizationCodeFlow$Builder. httpTransport jsonFactory clientSecrets scopes)
                                         (.setDataStoreFactory dataStoreFactory)
                                         (.setAccessType "offline")
                                         (.build))
                    credential       (-> (AuthorizationCodeInstalledApp. flow (LocalServerReceiver.))
                                         (.authorize "user"))]
                credential))
            (sheet-service []
              (let [credential (authorize)]
                (-> (Sheets$Builder. httpTransport jsonFactory credential)
                    (.setApplicationName "Robot")
                    (.build))))
            (idx->col [n]
              (let [i (int (/ n 26))
                    f (int (mod n 26))
                    p (if (> i 0) 
                        (str (char (+ (dec i) 65)))
                        "")
                    s (str (char (+ f 65)))]
                (str p s)))
            (write [spreadsheet-id sheet-name as-row? data]
              (let [service (sheet-service)
                    val  (-> (.spreadsheets service)
                             (.values)
                             (.get spreadsheet-id (str sheet-name "!" (if as-row? "A:A" "1:1")))
                             (.execute)
                             (.getValues))
                    idx (cond-> val                            
                          (not as-row?) (first)
                          true          (count)
                          as-row?       (inc))
                    _ (log/debug {:val-raw (pr-str val)})
                    _ (log/debug {:idx-raw idx})
                    idx  (if as-row? idx (idx->col idx))
                    _ (log/info {:idx idx})
                    data (if (and (not as-row?) (> (count data) 1)) 
                           (mapv vector data)    ;[["val1"]["val2"]] col
                           [data])               ;[["val1" "val2"]]  row
                    _ (log/info {:data (pr-str data)})
                    body (-> (BatchUpdateValuesRequest.)
                             (.setValueInputOption "USER_ENTERED")
                             (.setData [(-> (ValueRange.)
                                            (.setRange (str sheet-name "!" idx ":" idx))
                                            (.setValues data))]))]
                (-> (.spreadsheets service)
                    (.values)
                    (.batchUpdate spreadsheet-id body)
                    (.execute))))]
      (write spreadsheet-id sheet-name as-row? data))))
