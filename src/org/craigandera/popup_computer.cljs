(ns org.craigandera.popup-computer)

(defn hello [] (.debug js/console "ohai there"))

(defn reciprocal
  [heading]
  (if (< heading 180)
    (+ heading 180)
    (- heading 180)))

(defn ^:export compute
  []
  (let [attack-heading (-> js/document
                         (.getElementById "attack-heading")
                         .-value
                         js/Number)
        output (-> js/document
                 (.getElementById "output" ""))]
    (set! (.-innerHTML output) (reciprocal attack-heading))))
