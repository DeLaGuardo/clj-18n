(ns clj-i18n.dev
  (:require-macros [clj-i18n.macros :refer [generate-translations!]])
  (:require [clj-i18n.core :as i18n]))

(defn init []
  (generate-translations! "th")
  (js/console.log (i18n/format (js/Date.) "%a")))

(defn refresh []
  (let [location (.-href (.-location js/window))]
    (set! (.-href (.-location js/window))
          location)))
