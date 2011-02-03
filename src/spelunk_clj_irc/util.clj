;   Copyright (c) Seth Schroeder. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;   Source of IRC logs: http://clojure-log.n01se.net/

(ns spelunk-clj-irc.util
  (:require [clj-time.format :as date]
            [clj-time.core :as time]
            [clojure.string :as string]
            [net.cgrand.enlive-html :as html]))

(def _time-pattern #"^(\d\d):(\d\d)(.)?$")

(def _date-pattern
     #"^.+- (\p{Alpha}{3})\p{Space}(\p{Digit}{2})\p{Space}+(\p{Digit}{4})$")

(def _formatter (date/formatter "yyyy MMM DD HH mm ss"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn grab-log-date [data]
  {:post [(seq %)]}
  (let [raw-title (first (:content (first (html/select data [:head :title]))))]
    (rest (re-matches _date-pattern raw-title))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; handles all of the following:
;; hh : hours
;; mm : minutes
;; a-z? : a character a-z, sometimes. Don't blame me for the data format.
(defn normalize-time-component [hhmma-z?]
  (let [act_val (first (drop-while nil? [hhmma-z? "a"]))]
    (if (re-matches #"^\d\d$" act_val)
      act_val
      ;; seconds aren't available in the data, and synthesizing them seems risky.
      ;; might be better to increment seconds per message per minute...
      #_(format "%02d" (- (.hashCode act_val) (.hashCode "a")))
      "00")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is probably stupidly expensive. Also smells like refactoring.
;; um, what? (date/parse (date/formatter "yyyy MMM DD") "2008 Mar 08")
;; #<DateTime 2008-01-08T00:00:00.000Z>
;; after identifying that problem, get rid of this abomination:
(def _shameful-hack (zipmap ["alas, the shame"
                             "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                             "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]
                            (map str (range 13))))
(defn irc-to-joda-time [year month day hhmma-z]
  (let [[hour minute second] (map normalize-time-component
                                  (rest (first (re-seq _time-pattern hhmma-z))))]
    (apply time/date-time
           (map #(Integer/parseInt %) [year (_shameful-hack month) day hour minute second]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this smells like refactoring... string/join is awkward
(defn aggregate-string-value [hash key in-str]
  (let [prev-str (get hash key)
        cur-str  (when (string? in-str) (string/trim in-str))
        strs     (remove string/blank? [prev-str cur-str])]
    (assoc hash key (case (count strs)
                          2 (string/join " " strs)
                          1 (first strs)
                          0 nil))))
