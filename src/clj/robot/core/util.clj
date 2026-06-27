(ns robot.core.util
  (:require [clojure.string :as S]
            [clojure.tools.logging :as log]))

(def keyword-re #":([a-zA-Z][a-zA-Z0-9/\-_]+)")

(defn extract-keywords-seq [text]
  (let [keywords (re-seq keyword-re text)]
    (if (seq keywords)
      (->> keywords
           (map second)
           (map keyword)))))


(defn contextualize-params [param context]
  (if (keyword? param)
    (context param param)
    param))


(defn contextualize [context text]
  (let [text (if (string? text) text (str text))
        keywords (extract-keywords-seq text)
        substitution-map (into
                           {}
                           (filter
                             identity
                             (map (fn [k]
                                    (if-let [val (k context)]
                                      [k val]))
                                  keywords)))]
    ;(log/debug "\n\n-------------------------------------")
    ;(log/debug :substitution-map (pr-str substitution-map))
    ;(log/debug :text (pr-str text))
    (reduce-kv
      (fn [result k v]
        (S/replace result (str k) (str v)))
      text
      substitution-map)))

(defn contextualize-edn [context text]
  (let [text (if (string? text) text (str text))
        keywords (extract-keywords-seq text)
        substitution-map (into
                           {}
                           (map (fn [k]
                                  [k (k context)])
                                keywords))]
    ;(log/debug "\n\n-------------------------------------")
    ;(log/debug :substitution-map (pr-str substitution-map))
    ;(log/debug :text (pr-str text))
    (reduce-kv
      (fn [result k v]
        (S/replace result (str k) (pr-str v)))
      text
      substitution-map)))

(defn contextualize-integer [context param default]
  (let [contextualized-param (contextualize context param)
        contextualized-param (try (Integer/parseInt contextualized-param)
                                  (catch NumberFormatException ex
                                    (log/warn (str "El número " contextualized-param " es inválido, devolviendo valor por defecto: " default))
                                    default))]
    contextualized-param))

(defn contextualize-boolean [context param default]
  (let [contextualized-param (contextualize context param)
        contextualized-param (if (or (= contextualized-param "true") (= contextualized-param "false"))
                               (= contextualized-param "true")
                               (do
                                 (log/warn (str "El booleano " contextualized-param " es inválido, devolviendo valor por defecto: " default))
                                 default))]
    contextualized-param))


(defn concat-tokens [tokens]
  (reduce #(str %1 " " %2) tokens))

(defn contextualize-seq-of-tokens [tokens context]
  (map #(contextualize-params % context) tokens))

(defn convert-text2seq-of-tokens [text]
  (.split (str text) " "))

(defn token2key [token]
  (if (= \: (first token))
    (keyword (subs token 1))
    token))


(defn convert-seq-of-tokens2seq-of-tokens-or-keywords [tokens]
  (map #(token2key %) tokens))

(defn contextualize-text [text context]
  (let [r (concat-tokens
            (contextualize-seq-of-tokens
              (convert-seq-of-tokens2seq-of-tokens-or-keywords
                (convert-text2seq-of-tokens text))
              context))]
    (str r)))
