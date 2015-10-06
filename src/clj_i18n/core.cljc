(ns clj-i18n.core
  #?(:cljs (:require-macros [clj-i18n.macros :refer [translate]])
     :clj (:require [clj-i18n.macros :refer [translate]])))

(declare t)

(defn blank-padded
  ([value]
   (blank-padded value 2))
  ([value count]
   (apply str (take-last count (apply str (concat (repeat count " ") value))))))

(defn zero-padded
  ([value]
   (zero-padded value 2))
  ([value count]
   (apply str (take-last count (apply str (concat (repeat count "0") value))))))

;; Date

(declare %-m)
(declare %e)

(defn %Y [date]
  (.getFullYear date))

(defn %C [date]
  (let [year (%Y date)]
    (rem year 100)))

(defn %y [date]
  (let [year (%Y date)]
    (mod year 100)))

(defn %m [date]
  (zero-padded (%-m date)))

(defn %_m [date]
  (blank-padded (%-m date)))

(defn %-m [date]
  (inc (.getMonth date)))

(defn %B [date]
  (let [month-names (translate :date :month_names)]
    (get month-names (%-m date))))

(defn %b [date]
  (let [short-month-names (translate :date :abbr_month_names)]
    (get short-month-names (%-m date))))

(defn %h [date]
  (%b date))

(defn %d [date]
  (zero-padded (%-d date)))

(defn %_d [date]
  (%e date))

(defn %e [date]
  (blank-padded (%-d date)))

(defn %-d [date]
  (.getDate date))

(defn %j [date]
  (let [january-first (js/Date. (%Y date) 0 1)
        one-day (* 1000 60 60 24)]
    (zero-padded (rem (js/Number date) one-day) 3)))

;; Time

(defn %H [date]
  (zero-padded (.getHours date)))

(defn %k [date]
  (blank-padded (.getHours date)))

(defn hours [date]
  (let [h (.getHours date)]
    (cond
      (= h 24) 12
      (= h 0) 1
      :else (mod h 12))))

(defn %I [date]
  (zero-padded (hours date)))

(defn %l [date]
  (blank-padded (hours date)))

(defn %P [date]
  (let [h (.getHours date)]
    (case (quot h 12)
      0 (translate :time :am)
      1 (translate :time :pm))))

(defn %p [date]
  (.toUpperCase (%P date)))

(defn %M [date]
  (zero-padded (.getMinutes date)))

(defn %S [date]
  (zero-padded (.getSeconds date)))

(defn %L [date]
  (zero-padded (.getMilliseconds date) 3))

;; TimeZone

(defn get-timezone [date]
  (let [tz (.getTimezoneOffset date)
        tz-hours (let [th (quot tz 60)]
                   (if (> th 0)
                     (zero-padded (str th))
                     (str "-" (zero-padded (str th)))))
        tz-minutes (zero-padded (str (mod tz 60)))]
    [tz-hours tz-minutes]))

(defn %z [date]
  (apply str (get-timezone date)))

(defn %:z [date]
  (let [[tz-h tz-m] (get-timezone date)]
    tz-h))

(defn %::z [date]
  (let [[tz-h tz-m] (get-timezone date)]
    (str tz-h ":" tz-m)))

(defn %:::z [date]
  (str (%::z date) ":00"))

(defn %Z [date]
  (let [tz-str (take-last 1 (.split (str date) " "))]
    (apply str (rest (butlast tz-str)))))

;; Weekday

(declare %w)

(defn %A [date]
  (let [day_names (translate :date :day_names)]
    (get day_names (%w date))))

(defn %a [date]
  (let [days_short (translate :date :abbr_day_names)]
    (get days_short (%w date))))

(defn %w [date]
  (.getDay date))

(defn %u [date]
  (let [x (%w date)]
    (if (= x 0)
      7
      x)))

;; ISO 8601 week-based year and week number

(defn %G [date])

(defn %g [date])

(defn %V [date])

;; Week number
(defn %U [date])

(defn %W [date])

;; Seconds since the Unix Epoch

(defn %s [date]
  (str (js/Number date)))

(defn %Q [date]
  (str (* (js/Number date) 1000)))

;; Literal string

(defn %n [date]
  "\n")

(defn %t [date]
  "\t")

;; Combination

(defn %c [date]
  (format-date date "%a %b %e %T %Y"))

(defn %D [date]
  (format-date date "%m/%d/%y"))

(defn %F [date]
  (format-date date "%Y-%m-%d"))

(defn %v [date]
  (format-date date "%e-%b-%Y"))

(defn %x [date]
  (%D date))

(defn %X [date]
  (%D date))

(defn %r [date]
  (format-date date "%I:%M:%S %p"))

(defn %R [date]
  (format-date date "%H:%M"))

(defn %T [date]
  (format-date date "%H:%M:%S"))

(defn %+ [date]
  (format-date date "%a %b %e %H:%M:%S %Z %Y"))

(def format-map {"%a" #(t [:datepicker :days_short (.getDay %)])
                 "%A" #(t [:datepicker :days (.getDay %)])
                 "%b" #(t [:datepicker :months_short (.getMonth %)])
                 "%B" #(t [:datepicker :months (.getMonth %)])
                 "%d" #(apply str (take-last 2 (str "0" (.getDate %))))
                 "%m" #(apply str (take-last 2 (str "0" (inc (.getMonth %)))))
                 "%y" #(apply str (take-last 2 (str (.getFullYear %))))
                 "%Y" #(.getFullYear %)
                 "%-e" #(.getDate %)})

(defn interpolate [translation value]
  (.replace translation (js/RegExp. "%\\{(value|count)\\}") value))

(defn plural [translation count]
  (let [key (pluralize count)]
    (interpolate (key translation) count)))

(defn t [translation-keys & [{:keys [value count]}]]
  (let [translation (translate translation-keys)]
    (cond
      value
      (interpolate translation value)
      count
      (plural translation count)
      :else
      translation)))

(defn- parse-char [date ch acc-t acc-r]
  (if (= (count acc-t) 0)
    (if (= ch "%")
      [[ch] acc-r]
      [acc-t (concat acc-r [ch])])
    (if-let [fmt-fn (get format-map (apply str acc-t))]
      (if (= ch "%")
        [[ch] (concat acc-r [(fmt-fn date)])]
        (if (nil? ch)
          [[] (concat acc-r [(fmt-fn date)])]
          [acc-t (concat acc-r [(fmt-fn date) ch])]))
      (if (= ch "%")
        [[ch] (concat acc-r acc-t)]
        (if (nil? ch)
          [[] (vec (concat acc-r acc-t))]
          [(conj acc-t ch) acc-r])))))

(defn format-date [date format]
  (if date
    (loop [fmt-v (seq format) acc-t [] acc-r []]
      (if (and (= (count acc-t) 0) (= (count fmt-v) 0))
        (apply str acc-r)
        (let [ch (first fmt-v)
              rest-ch (rest fmt-v)
              [n-acc-t n-acc-r] (parse-char date ch acc-t acc-r)]
          (recur rest-ch n-acc-t n-acc-r))))
    ""))

