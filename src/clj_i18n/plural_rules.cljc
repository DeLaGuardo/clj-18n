(ns clj-i18n.plural-rules)

(defn zero-other [count]
  (cond
    (= count 0) :zero
    :else :other))

(defn zero-one-other [count]
  (cond
    (= count 1) :one
    :else (zero-other count)))

(defn east-slavic [count]
  (let [mod10 (mod count 10)
        mod100 (mod count 100)]
    (cond
      (and (contains? #{2 3 4} mod10) (not (contains? #{12 13 14} mod100))) :few
      (or (contains? #{0 1 5 6 7 8 9} mod10) (contains? #{12 13 14} mod100)) :many
      :else (zero-one-other count))))

(defn zero-one-two-other [count]
  (cond
    (= count 2) :two
    :else (zero-one-other count)))

(defn one-upto-two-other [count]
  (cond
    (and (<= 0 count) (> 2 count)) :one
    :else :other))

(defn one-with-zero-other [count]
  (cond
    (or (= 0 count) (= 1 count)) :one
    :else :other))

(defn romanian [count]
  (cond
    (and (= 0 count) (contains? (into #{} (range 1 20)) (mod count 100))) :few
    :else (zero-one-other count)))

(defn west-slavic [count]
  (cond
    (contains? #{2 3 4} count) :few
    :else (zero-one-other count)))

