(ns clj-mktemp.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clj-mktemp.either :as either]))

(def ^:dynamic *tmpdir* (System/getProperty "java.io.tmpdir"))
(def ^:dynamic *mktemp-default-template* "tmp.XXXXXXXXXX")
(def ^:dynamic *mktemp-max-of-retry-fod-creation* 10)

(defn random-alphabets []
  (repeatedly #(char (+ (rand-nth [65 97]) (rand 26)))))

(defn mktemp-fod-name [template ss]
  (let [matcher #"X{1,}"
        replacement (re-find matcher template)
        identifier (apply str (take (count replacement) ss))]
    (if replacement
      (string/replace template matcher identifier)
      template)))

(defn mktemp [& {:keys [template]}]
  (io/file *tmpdir*
           (mktemp-fod-name (or template *mktemp-default-template*) (random-alphabets))))

(defn mktemp! [& {:keys [type template]}]
  (loop [retry-count *mktemp-max-of-retry-fod-creation*]
    (let [fod (mktemp :template template)
          created?
          (if (= type :dir)
            (.mkdir fod)
            (.createNewFile fod))]
      (cond
        created? (either/right fod)
        (pos? retry-count) (recur (dec retry-count))
        :default (either/left
                   (ex-info
                     (format "Failed to create %s %s"
                             fod
                             (if (= type :dir)
                               "directory"
                               "file"))
                     {:type type
                      :fod fod}))))))
