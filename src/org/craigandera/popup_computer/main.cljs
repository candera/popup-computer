(ns org.craigandera.popup-computer.main
  (:require [org.craigandera.popup-computer :as pc]))

(defn hello [] (.debug js/console "ohai there"))

(defn by-id
  [id]
  (.getElementById js/document id))

(defn set-inner-html
  [e html]
  (set! (.-innerHTML e) html))

(defn by-class
  [class]
  (array-seq (.getElementsByClassName js/document class)))

(defn set-inner-text
  [id text]
  (set! (.-innerText (by-id id)) text))

(defn set-value
  [id val]
  (set! (.-value (by-id id)) val))

(defn get-numeric-value
  [id]
  (-> js/document
    (.getElementById id)
    .-value
    js/Number))

(defn round
  [val precision]
  (as-> val ? (* ? (/ 1 precision)) (Math/round ?) (* ? precision)))

(def ^:dynamic *indent* "")

(defmulti stringify (fn [x]
                     (cond (map? x) :map
;;                           (vector? x) :vector
                           :else :default)))

(defmethod stringify :map
  [x]
  (str "\n" *indent* "{"
       (binding [*indent* (str "  " *indent*)]
        (->> x
          (map (fn [[k v]]
                 (str (stringify k) " " (stringify v))))
          (interpose (str "\n" *indent*))
          (apply str)))
       "}"))

(defmethod stringify :default
  [x]
  (pr-str x))

(defn get-input
  []
  (let [params (->> [:speed :turn-g :attack-heading
                     :target-altitude
                     :dive-angle :release-altitude :tracking-time
                     :climb-angle
                     :ingress-altitude
                     :vrp-to-pup-distance]
                 (map (fn [x] [x (get-numeric-value (name x))]))
                 (into (sorted-map)))]
    (assoc params
           :pop-up-turn-amount
           (* (get-numeric-value "pop-up-turn-amount")
              (if (-> (by-id "pop-up-turn-direction")
                    .-value
                    (= "right"))
                1 -1))
           :pull-down-turn-amount
           (* (get-numeric-value "pull-down-turn-amount")
              (if (-> (by-id "pull-down-turn-direction")
                    .-value
                    (= "right"))
                1 -1)))))

(defn ^:export compute
  [e]
  (.debug js/console "compute")
  (let [input (get-input)
        target-steerpoint (get-numeric-value "target-steerpoint")
        output (pc/compute input)
        ;;solution (pc/find-solution input 100 500)
        ]
    (set-inner-html (by-id "mach")
                    (-> output :mach (.toFixed 2)))
    (set-inner-html (by-id "turn-radius")
                    (-> output :turn-radius (round 2)))

    ;; TGT-TO-VRP
    (let [{:keys [r theta z]} (pc/->vec :cyl (:vrp output))]
      (set-inner-html (by-id "tgt-to-vrp-tgt") target-steerpoint)
      (set-inner-html (by-id "tgt-to-vrp-brg")
                      (.toFixed (mod theta 360) 1))
      (set-inner-html (by-id "tgt-to-vrp-rng")
                      (round r 1))
      (set-inner-html (by-id "tgt-to-vrp-elev") (:ingress-altitude input)))

    ;; TGT-TO-PUP
    (let [{:keys [r theta z]} (pc/->vec :cyl (:pup output))]
      (set-inner-html (by-id "tgt-to-pup-tgt") target-steerpoint)
      (set-inner-html (by-id "tgt-to-pup-brg")
                      (.toFixed (mod theta 360) 1))
      (set-inner-html (by-id "tgt-to-pup-rng")
                      (round r 1))
      (set-inner-html (by-id "tgt-to-pup-elev") (:ingress-altitude input)))

    ;; Offset aimpoints
    (let [{:keys [theta r z]} (pc/->vec :cyl (:pull-down output))
          brg (.toFixed (mod theta 360) 1)
          rng (round r 1)]
      (set-inner-html (by-id "oa1-brg") brg)
      (set-inner-html (by-id "oa2-brg") brg)
      (set-inner-html (by-id "oa1-rng") rng)
      (set-inner-html (by-id "oa2-rng") rng)
      (set-inner-html (by-id "oa1-elev") (-> input
                                           :target-altitude
                                           (+ z)
                                           (round 2)))
      (set-inner-html (by-id "oa2-elev") 0))

    ;; Fill in profile details
    (doseq [[class content]
            {"ip-steerpoint"        (dec target-steerpoint)
             "target-steerpoint"    target-steerpoint
             "speed"                (:speed input)
             "turn-g"               (:turn-g input)
             "level-turn-direction" (.-value (by-id "pop-up-turn-direction"))
             "pup-heading"          (:heading-to-pull-down output)
             "climb-angle"          (:climb-angle input)
             "dive-angle"           (:dive-angle input)
             "release-altitude"     (:release-altitude input)}
            elem (by-class class)]
      (set-inner-html elem content))

    (set-inner-text "debug" (stringify {:inputs input
                                        :output output}))))

(set! (.-onload js/window)
      compute)
