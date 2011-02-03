;   Copyright (c) Seth Schroeder. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;   Source of IRC logs: http://clojure-log.n01se.net/

(ns spelunk-clj-irc.core
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as string]
            [clojure.contrib.except :as except]
            [net.cgrand.enlive-html :as html]
            [spelunk-clj-irc.util :as util]
           )
  (:import [java.net URL]
           [java.io File])
  (:use    [spelunk-clj-irc.types :as types]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Comment [#^String who, #^String what, #^org.joda.time.DateTime when]
  ICSVSerializable
  (getHeader [_] "\"who\",\"what\",\"when\"")
  (toCSV [self]
         (string/join "," (map #(str \"
                                     (string/escape (str (% self)) { \" "\"\"" })
                                     \")
                               [:who :what :when]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *last-person* (atom ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- set-speaker [hash speaker]
  (reset! *last-person* speaker)
  (assoc hash :who speaker))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn preprocess-node [year month day item]
  {:post [(:who %), (:when %), #_(:what %)]} ;; relaxing what constraint, empty messages filtered later
  (reduce (fn [memo node]
            ;; (log/debug "==== Begin node processing")
            ;; ;; hmm.. how to mix clojure.contrib.strint, clojure.pprint, and clojure.contrib.logging?
            ;; (log/trace memo)
            ;; (log/trace node)

            (cond
             (string? node)
             (util/aggregate-string-value memo :what node)

             (= :em (:tag node))
             (set-speaker memo (first (:content node)))

             (= :b (:tag node))
             (let [value (first (:content node))
                   speaker (when (re-find #":" value) (first (string/split value #":")))]
               (if speaker
                 (set-speaker memo speaker)
                 memo))

             (= :a (:tag node))
             (let [{:keys [name href]} (:attrs node)]
               ;; (log/trace (<< "* <a name=\"~{name}\" href=\"~{href}\">"))
               (cond
                (string? name)
                (assoc memo :when (util/irc-to-joda-time year month day name))

                (string? href)
                (util/aggregate-string-value memo :what href)

                :default
                (throw (Exception. (str "I have no idea what this anchor tag is! " node)))))

             :default
             (throw (Exception. (str "I have no idea how to handle this node: " node)))))

          {:who @*last-person*}
          item))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; will return nil for valid nodes which are just empty messages
(defn node-to-comment [year month day node]
  (try
    (let [preprocessed-node (preprocess-node year month day node)
          {:keys [who when what]} preprocessed-node]
      (if (and who when what)
        (Comment. who what when)
        nil))
    (catch Throwable ex
       (println "puked on this node")
       (pp/pprint node)
       (throw ex))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn html-to-comments [html-data]
  (let [[month day year] (util/grab-log-date html-data)]
    (remove nil? (map #(node-to-comment year month day (:content %))
                      (html/select html-data [:#main :p])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn url-to-comments [url]
  (html-to-comments (html/html-resource url)))

(defn url-to-csv [url]
  (map #(toCSV %) (url-to-comments url)))

(defn url-to-csv-file [url]
  (let [data (url-to-csv url)
        destination (File. (string/replace (str url) ".html" ".csv"))]
    (when-not (.exists destination)
      (println url "   =>   " destination)
      (with-open [os (io/output-stream destination)]
        (.write os (.getBytes (str (getHeader (Comment. nil nil nil)) "\n")))
        (doseq [row data]
          (.write os (.getBytes (str row "\n"))))))))

(defn dir-to-csv [dir]
  (let [dir_url (File. dir)]
    (doseq [x (filter #(re-matches #"^.+\.html$" %) (remove nil? (.list dir_url)))]
      (url-to-csv-file (File. dir x)))))

