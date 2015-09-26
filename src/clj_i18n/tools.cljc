(ns clj-i18n.tools)

(defn deep-merge
  ([a] a)
  ([a b] (merge-with (fn [x y]
                       (cond
                         (and (not (map? x)) (map y)) y
                         (map? y) (deep-merge x y)
                         :else y))
                     a b))
  ([a b & more] (apply deep-merge
                       (deep-merge a b)
                       more)))

(defn cart
  ([xs] xs)
  ([xs ys] (mapcat (fn [x] (map (fn [y] (list x y)) ys)) xs))
  ([xs ys & more] (mapcat (fn [x] (map (fn [z] (cons x z)) (apply cart (cons ys more)))) xs)))

