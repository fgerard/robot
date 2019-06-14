(ns util.crypt-util
  (:require [clojure.data.codec.base64 :as base64]
            [clojure.tools.logging :as log])
  (:import (java.security Signature MessageDigest)
           (javax.crypto Cipher)
           (javax.crypto.spec IvParameterSpec SecretKeySpec)
           (java.util Arrays)
           (javax.xml.bind DatatypeConverter)
           (org.apache.commons.codec.digest DigestUtils)))

(defn as-string [v] (->> v (map char) (apply str)))

(defn as-data [v] (.getBytes (as-string v)))

(def CRYPT-ALGORITHM "AES")

(def DEFAULT-ENCODING "UTF-8")

(def CRYPT-PADDING "/CBC/PKCS5Padding")

(def SIGN-ALGORITHM "SHA256withRSA")

(def CRYPT-TRANSFORMATION (str CRYPT-ALGORITHM CRYPT-PADDING))

(def DIGEST-ALGORITHM "SHA-256")

(def INIT-VECTOR (byte-array (repeat 16 (byte 0x0))))

(def ^:private CRYPTO-KEY (as-data [99 108 111 106 117 114 101 35 67 105 112 104 101 114 47 55]))

(def ^:private secret-key (new SecretKeySpec CRYPTO-KEY CRYPT-ALGORITHM))

(def ^:private symmetric-encrypter (let [result (Cipher/getInstance CRYPT-ALGORITHM)
                                         _      (.init result Cipher/ENCRYPT_MODE secret-key)]
                                     result))

(def ^:private symmetric-decrypter (let [result (Cipher/getInstance CRYPT-ALGORITHM)
                                         _      (.init result Cipher/DECRYPT_MODE secret-key)]
                                     result))

(defn bytes->hex [data]
  (.toLowerCase (DatatypeConverter/printHexBinary data)))

(defn hex-bytes [hex-string]
  (DatatypeConverter/parseHexBinary hex-string))

(defn digest
  ([data algorithm]
   (let [digest      (MessageDigest/getInstance algorithm)
         seed-bytes  data
         upd         (.update digest seed-bytes)
         digest-data (.digest digest)]
     ;(Arrays/copyOf digest-data 16)
     digest-data))
  ([data] (digest data DIGEST-ALGORITHM)))

(defn digest-text
  ([value algorithm] (bytes->hex (digest (.getBytes value DEFAULT-ENCODING) algorithm)))
  ([value] (digest-text value DIGEST-ALGORITHM)))

(defn get-cipher [mode seed]
  (let [key-spec (SecretKeySpec. (digest (.getBytes seed)) CRYPT-ALGORITHM)
        iv-spec  (IvParameterSpec. INIT-VECTOR)
        cipher   (Cipher/getInstance CRYPT-TRANSFORMATION)]
    (.init cipher mode key-spec iv-spec)
    cipher))

(defn encrypt-text [text seed]
  (let [bytes  (.getBytes text DEFAULT-ENCODING)
        cipher (get-cipher Cipher/ENCRYPT_MODE seed)]
    (try
      (new String (base64/encode (.doFinal cipher bytes)) DEFAULT-ENCODING)
      (catch Throwable t
        (throw t)))))

(defn decrypt-text [text seed]
  (let [cipher (get-cipher Cipher/DECRYPT_MODE seed)]
    (try
      (new String (.doFinal cipher (base64/decode (.getBytes text DEFAULT-ENCODING))))
      (catch Throwable t
        (.printStackTrace t)
        (throw t)))))

(defn encrypt [data seed]
  (let [cipher (get-cipher Cipher/ENCRYPT_MODE seed)]
    (try
      (.doFinal cipher data)
      (catch Throwable t
        (.printStackTrace t)
        (throw t)))))

(defn decrypt [data seed]
  (let [cipher (get-cipher Cipher/DECRYPT_MODE seed)]
    (try
      (.doFinal cipher data)
      (catch Throwable t
        (.printStackTrace t)
        (throw t)))))

(defn sencrypt [text]
  (let [encrypted (.doFinal symmetric-encrypter (.getBytes text))
        b64-data  (base64/encode encrypted)]
    (new String b64-data DEFAULT-ENCODING)))

(defn sdecrypt [text]
  (let [decoded   (base64/decode (.getBytes text DEFAULT-ENCODING))
        decrypted (.doFinal symmetric-decrypter decoded)]
    (new String decrypted DEFAULT-ENCODING)))

(defn sign
  ([private-key data]
   (sign SIGN-ALGORITHM private-key data))
  ([alg private-key data]
   (let [;_ (println "Signing with alg: " SIGN-ALGORITHM)
         signature   (doto
                      (Signature/getInstance alg)
                      (.initSign private-key)
                      (.update data))
         signed-data (.sign signature)]
     signed-data)))

(defn sign-text
  ([private-key value]
   (sign-text SIGN-ALGORITHM private-key value))
  ([alg private-key value]
   (let [data        (.getBytes value DEFAULT-ENCODING)
         signed-data (sign alg private-key data)
         signed-b64  (base64/encode signed-data)
         result (new String signed-b64 DEFAULT-ENCODING)]
     ;(println "SELLO SIZE: " (.length result))
     result)))

(defn validate-signature
  ([certificate data signed-data]
   (validate-signature SIGN-ALGORITHM certificate data signed-data))
  ([alg certificate data signed-data]
   (let [signature (doto
                    (Signature/getInstance alg)
                    (.initVerify certificate)
                    (.update data))]
     (.verify signature signed-data))))

(defn validate-text-signature
  ([certificate text b64-signed-text]
   (validate-text-signature SIGN-ALGORITHM certificate text b64-signed-text))
  ([alg certificate text b64-signed-text]
   (let [data        (.getBytes text DEFAULT-ENCODING)
         signed-data (base64/decode (.getBytes b64-signed-text DEFAULT-ENCODING))]
     (validate-signature alg certificate data signed-data))))

(defn sha1 [data-stream]
  (DigestUtils/sha1 data-stream))

(defn sha1-as-hex [data-stream]
  (bytes->hex (DigestUtils/sha1 data-stream)))
