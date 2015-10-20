(ns clj-i18n.core
  #?(:cljs (:require-macros [clj-i18n.macros :refer [translate generate-translations!]])
     :clj (:require [clj-i18n.macros :refer [translate]])))

(declare format)
(declare pluralize)

(defn- any-padded [ch]
  (fn [s & {:keys [count] :or {count 2}}]
    (apply str (take-last count (apply str (concat (repeat count ch) s))))))

(defn- get-year [date]
  (.getFullYear date))
(defn- get-month [date]
  (inc (.getMonth date)))
(defn- get-date [date]
  (.getDate date))
(defn- get-hours [date]
  (.getHours date))
(defn- get-minutes [date]
  (.getMinutes date))
(defn- get-seconds [date]
  (.getSeconds date))
(defn- get-milliseconds [date]
  (.getMilliseconds date))
(defn- get-timezone-offset [date]
  (let [tz (.getTimezoneOffset date)
        sign (if (< tz 0) -1 1)
        tz (* sign tz)
        th ((any-padded "0") (str (quot tz 60)))
        tm ((any-padded "0") (str (mod tz 60)))]
    (str (when (< sign 0) "-") th tm)))
(defn- get-weekday [date]
  (.getDay date))

(def ^:private flags-map
  {"-" (fn [x &] (apply str x))
   "_" (any-padded " ")
   "0" (any-padded "0")
   "^" (fn [origin-string &] (.toUpperCase (apply str origin-string)))
   "#" (fn [origin-string &]
         (loop [letters origin-string res []]
           (if-let [first-char (first letters)]
             (let [lower-first-char (.toLowerCase first-char)
                   new-char (if (= first-char lower-first-char)
                              (.toUpperCase first-char)
                              lower-first-char)]
               (recur (rest letters) (conj res new-char)))
             (apply str res))))
   ":" (fn [tz-offset &]
         (apply str tz-offset))})

;; Main formatter
(defmulti formatter (fn [_ [_ _ _ conversion]] (str conversion)))
;; Date
(defmethod formatter "Y" [date _]
  (get-year date))
(defmethod formatter "C" [date _]
  (quot (get-year date) 100))
(defmethod formatter "y" [date _]
  (mod (get-year date) 100))
(defmethod formatter "m" [date [flags _ _ _]]
  (let [flags-composition (apply comp (map flags-map (.split flags "")))
        month-str (str (get-month date))]
    (flags-composition month-str)))
(defmethod formatter "B" [date _]
  (let [month-names (translate :date :month_names)]
    (str (get month-names (get-month date)))))
(defmethod formatter "b" [date _]
  (let [short-month-names (translate :date :abbr_month_names)]
    (str (get short-month-names (get-month date)))))
(defmethod formatter "h" [date _]
  (formatter date [nil nil nil "b"]))
(defmethod formatter "d" [date [flags _ _ _]]
  (let [flags-composition (apply comp (map flags-map (.split flags "")))]
    (flags-composition (str (get-date date)))))
(defmethod formatter "e" [date _]
  (format date "%_d"))
(defmethod formatter "j" [date _]
  (let [january-first (js/Date. (get-year date) 0 1)
        one-day (* 1000 60 60 24)]
    ((any-padded "0") (str (quot (- (js/Number date) (js/Number january-first)) one-day)) :count 3)))
;; Time
(defmethod formatter "H" [date _]
  ((any-padded "0") (str (get-hours date))))
(defmethod formatter "k" [date _]
  ((any-padded " ") (str (get-hours date))))
(defmethod formatter "I" [date _]
  (let [h (get-hours date)
        h (cond
            (= h 24) 12
            (= h 0) 1
            :else (mod h 12))]
    ((any-padded "0") (str h))))
(defmethod formatter "l" [date _]
  (let [h (get-hours date)
        h (cond
            (= h 24) 12
            (= h 0) 1
            :else (mod h 12))]
    ((any-padded "_") (str h))))
(defmethod formatter "P" [date [flags _ _ _]]
  (let [flags-composition (apply comp (map flags-map (.split flags "")))]
    (case (quot (get-hours date) 12)
      0 (flags-composition (translate :time :am))
      1 (flags-composition (translate :time :pm)))))
(defmethod formatter "p" [date _]
  (formatter date ["^" nil nil "P"]))
(defmethod formatter "M" [date _]
  ((any-padded "0") (str (get-minutes date))))
(defmethod formatter "S" [date _]
  ((any-padded "0") (str (get-seconds date))))
(defmethod formatter "L" [date _]
  ((any-padded "0") (str (get-milliseconds date))))
;; TimeZone
(defmethod formatter "z" [date [flags _ _ _]]
  (let [flags (filter #(= % ":") flags)
        tz (apply str (get-timezone-offset date))]
    (if ((comp not empty?) flags)
      (let [flags-composition (apply comp (map flags-map flags))]
        (flags-composition tz))
      tz)))
(defmethod formatter "Z" [date _]
  (apply str (butlast (rest (last (.split (.toString date) " "))))))
;; Weekday
(defmethod formatter "A" [date _]
  (let [weekday-names (translate :date :day_names)]
    (get weekday-names (get-weekday date))))
(defmethod formatter "a" [date _]
  (let [short-weekday-names (translate :date :abbr_day_names)]
    (get short-weekday-names (get-weekday date))))
(defmethod formatter "w" [date _]
  (str (get-weekday date)))
(defmethod formatter "u" [date _]
  (let [weekday (get-weekday date)]
    (if (= weekday 0)
      "7"
      (str weekday))))
;; ISO 8601 week-based year and week number

;; Week number

;; Seconds since the Unix Epoch
(defmethod formatter "s" [date _]
  (str (js/Number date)))
(defmethod formatter "Q" [date _]
  (str (* 1000 (js/Number date))))
;; Literal strings
(defmethod formatter "n" [_ _] "\n")
(defmethod formatter "t" [_ _] "\t")
(defmethod formatter "%" [_ _] "%")
;; Combinations
(defmethod formatter "c" [date _] (format date "%a %b %e %T %Y"))
(defmethod formatter "D" [date _] (format date "%m/%d/%y"))
(defmethod formatter "F" [date _] (format date "%Y-%m-%d"))
(defmethod formatter "v" [date _] (format date "%e-%b-%Y"))
(defmethod formatter "x" [date _] (format date "%D"))
(defmethod formatter "X" [date _] (format date "%D"))
(defmethod formatter "r" [date _] (format date "%I:%M:%S %p"))
(defmethod formatter "R" [date _] (format date "%H:%M"))
(defmethod formatter "T" [date _] (format date "%H:%M:%S"))
(defmethod formatter "+" [date _] (format date "%a %b %e %H:%M:%S %Z %Y"))
;; Default
(defmethod formatter :default [_ _] "Not yet implemented!")

(defn interpolate [translation value]
  (.replace translation (js/RegExp. "%\\{(value|count)\\}") value))

(defn plural [translation count]
  (let [key (pluralize count)]
    (interpolate (key translation) count)))

(defn- parse-formatter [formatter]
  (let [pattern #"%([-_0^#:]*)([123456789]*)([EO]?)([YCymBbhdejHklIPpMSLzZaAwuGgVUWsQntcDFvxXrRT+%]?)"]
    (re-seq pattern formatter)))

(def ^:private memoized-parse-formatter (memoize parse-formatter))

(defn format [date format]
  (loop [formatters (memoized-parse-formatter format) res format]
    (if (= (count formatters) 0)
      res
      (let [pattern (first (first formatters))
            params (rest (first formatters))]
        (recur (rest formatters) (.replace res pattern (formatter date params)))))))

(defn -main []
  (js/console.log "!"))
