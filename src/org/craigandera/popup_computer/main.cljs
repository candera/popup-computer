(ns ^:figwheel-always org.craigandera.popup-computer.main
    (:require[om.core :as om :include-macros true]
             [om.dom :as dom :include-macros true]
             [org.craigandera.popup-computer :as pc]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

(defn round
  [val precision]
  (as-> val ? (* ? (/ 1 precision)) (Math/round ?) (* ? precision)))

(def fields
  [{:id           :target-steerpoint
    :label        "Target steerpoint"
    :locked?      true
    :type         :number
    :help         "The steerpoint where the target is located."
    :validate     #(<= 1 % 99)
    :arrow-amount 1}
   {:id           :target-altitude
    :label        "Target altitude:"
    :locked?      true
    :type         :number
    :units        :feet-msl
    :help         "The altitude of the target above sea level."
    :validate     #(<= 0 % 20000)
    :arrow-amount 50}
   {:id           :ingress-altitude
    :label        "Ingress altitude:"
    :locked?      true
    :type         :number
    :units        :feet-agl
    :help         "The altitude at which the run in to the target will be made. Assumes that the ground at ingress is the same altitude as the target."
    :validate     #(<= 250 % 700)
    :arrow-amount 50}
   {:id           :speed
    :label        "Speed:"
    :locked?      true
    :type         :number
    :units        :kts
    :help         "The speed at which the attack profile will be run."
    :validate     #(<= 250 % 700)
    :arrow-amount 50}
   {:id     :mach
    :label  "Mach:"
    :locked? false
    :type   :number
    :help   "The mach at which the attack profile will be run. Computed from the speed and altitude."
    :format #(.toFixed % 2)}
   {:id           :turn-g
    :label        "Turn G:"
    :locked?      true
    :type         :number
    :help         "The turn G at which all turns in the profile will be made."
    :validate     #(<= 2.0 % 9.0)
    :arrow-amount 0.5}
   {:id     :turn-radius
    :label  "Turn radius:"
    :locked? false
    :type   :number
    :units  :feet
    :help   "The turn radius of all turns. Computed from altitude and mach."
    :format #(round % 2)}
   {:id           :attack-heading
    :label        "Attack heading:"
    :locked?      true
    :type         :number
    :units        :degrees
    :help         "The heading at which the aircraft will roll out before release."
    :validate     #(<= 0 % 360)
    :convert      #(mod % 360)
    :arrow-amount 5}
   {:id           :climb-angle
    :label        "Climb angle:"
    :locked?      true
    :type         :number
    :units        :degrees
    :help         "The desired angle at which to climb to the pull-down point."
    :validate     #(<= 10 % 60)
    :arrow-amount 5}
   {:id           :dive-angle
    :label        "Dive angle:"
    :locked?      true
    :type         :number
    :units        :degrees
    :help         "The angle at which the diving attack will be made. Currently assumes that the FPM will be pointed directly at the target - subtract a few degrees from this to get a bit of aim-off distance."
    :validate     #(<= 10 % 60)
    :arrow-amount 5}
   {:id           :vrp-to-pup-distance
    :label        "VRP to PUP distance:"
    :locked?      true
    :type         :number
    :units        :nm
    :help         "Distance between the VRP and the PUP."
    :validate     #(<= 0.25 % 20)
    :arrow-amount 0.25}
   {:id           :pop-up-turn-amount
    :label        "Heading change at pop-up point:"
    :locked?      true
    :type         :number
    :units        :degrees-change
    :help         "Desired turn amount and direction at PUP."
    :validate     #(<= -90 % 90)
    :arrow-amount 5}
   {:id           :pull-down-turn-amount
    :label        "Heading change during pull-down:"
    :locked?      true
    :type         :number
    :units        :degrees-change
    :help         "Desired turn amount and direction at pull-down."
    :validate     #(<= -90 % 90)
    :arrow-amount 5}
   {:id           :roll-out-altitude
    :label        "Roll-out altitude:"
    :locked?      false
    :type         :number
    :units        :feet-agl
    :help         "The altitude at which you roll out on the attack heading."
    :validate     #(<= 1500 % 20000)
    :arrow-amount 100
    :lock-group   :release}
   {:id           :release-altitude
    :label        "Release altitude:"
    :locked?      true
    :type         :number
    :units        :feet-agl
    :help         "The altitude by which weapons release will happen."
    :validate     #(<= 1500 % 20000)
    :arrow-amount 100
    :lock-group   :release}
   {:id           :tracking-time
    :label        "Tracking time:"
    :locked?      true
    :type         :number
    :units        :seconds
    :help         "How long to spend aligning after rolling out from the pull-down."
    :validate     #(<= 0.5 % 5.0)
    :arrow-amount 0.1
    :lock-group   :release}])

(def unit-label
  {:seconds        "seconds"
   :feet-agl       "feet AGL"
   :feet-msl       "feet MSL"
   :kts            "kts"
   :feet           "feet"
   :degrees        "degrees"
   :degrees-change "degrees"
   :nm             "nm"})

(def initial-inputs
  {:target-steerpoint     3
   :target-altitude       0
   :ingress-altitude      200
   :speed                 500
   :turn-g                4.0
   :attack-heading        270
   :climb-angle           25
   :dive-angle            20
   :vrp-to-pup-distance   1.0
   :pop-up-turn-amount    45
   :pull-down-turn-amount -90
   :roll-out-altitude     5000
   :release-altitude      2000
   :tracking-time         1.5})

(defonce app-state (atom {:input initial-inputs
                          :output (pc/compute initial-inputs)}))

(defmulti render-field :type)

(defn handle-input
  [e id]
  (swap! app-state (fn [state]
                     (let [input (:input state)
                           new-input (assoc input id (-> e .-target .-value js/Number))
                           new-output (pc/compute new-input)]
                       {:input new-input
                        :output new-output}))))

(defmethod render-field :number
  [{:keys [id label locked? units help format validate arrow-amount lock-group]
    :or {format identity}}]
  (dom/tr nil
          (dom/td nil
                  (dom/img #js {:src "images/help.png"
                                :title help}))
          (dom/td nil
                  (when lock-group
                    (dom/img #js {:src (if locked?
                                         "images/locked.png"
                                         "images/unlocked.png")})))
          (dom/td nil
                  (dom/label nil label))
          (dom/td nil
                  (if locked?
                    (dom/input #js {:onChange #(handle-input % id)
                                    :type "text"
                                    :value (get-in @app-state [:input id])})
                    (dom/label nil
                               (format (get-in @app-state [:output id])))))
          (dom/td nil
                  (unit-label units))))


(defn by-id
  [id]
  (.getElementById js/document id))

(om/root
 (fn [data owner]
   (reify om/IRender
     (render [_]
             (apply dom/table
                    nil
                    (for [field fields]
                      (render-field field))))))
 app-state
 {:target (by-id "inputs")})


;; (defn hello [] (.debug js/console "ohai there"))


;; (defn set-inner-html
;;   [e html]
;;   (set! (.-innerHTML e) html))

;; (defn by-class
;;   [class]
;;   (array-seq (.getElementsByClassName js/document class)))

;; (defn set-inner-text
;;   [id text]
;;   (set! (.-innerText (by-id id)) text))

;; (defn set-value
;;   [id val]
;;   (set! (.-value (by-id id)) val))

;; (defn get-numeric-value
;;   [id]
;;   (-> js/document
;;     (.getElementById id)
;;     .-value
;;     js/Number))


;; (def ^:dynamic *indent* "")

;; (defmulti stringify (fn [x]
;;                      (cond (map? x) :map
;; ;;                           (vector? x) :vector
;;                            :else :default)))

;; (defmethod stringify :map
;;   [x]
;;   (str "\n" *indent* "{"
;;        (binding [*indent* (str "  " *indent*)]
;;         (->> x
;;           (map (fn [[k v]]
;;                  (str (stringify k) " " (stringify v))))
;;           (interpose (str "\n" *indent*))
;;           (apply str)))
;;        "}"))

;; (defmethod stringify :default
;;   [x]
;;   (pr-str x))

;; (defn get-input
;;   []
;;   (let [params (->> [:speed :turn-g :attack-heading
;;                      :target-altitude
;;                      :dive-angle :release-altitude :tracking-time
;;                      :climb-angle
;;                      :ingress-altitude
;;                      :vrp-to-pup-distance]
;;                  (map (fn [x] [x (get-numeric-value (name x))]))
;;                  (into (sorted-map)))]
;;     (assoc params
;;            :pop-up-turn-amount
;;            (* (get-numeric-value "pop-up-turn-amount")
;;               (if (-> (by-id "pop-up-turn-direction")
;;                     .-value
;;                     (= "right"))
;;                 1 -1))
;;            :pull-down-turn-amount
;;            (* (get-numeric-value "pull-down-turn-amount")
;;               (if (-> (by-id "pull-down-turn-direction")
;;                     .-value
;;                     (= "right"))
;;                 1 -1)))))

;; (defn rmove-to
;;   [x y]
;;   (str "m " x "," y))

;; (defn rline-to
;;   [x y]
;;   (str "l " x "," y))

;; (defn move-to
;;   [p]
;;   (str "M " (pc/x p) "," (pc/y p)))

;; (defn line-to
;;   [p]
;;   (str "L " (pc/x p) "," (pc/y p)))

;; (defn close
;;   []
;;   "z")

;; (defn path-string
;;   [& commands]
;;   (->> commands
;;     (interpose " ")
;;     (apply str)))

;; (defn circle
;;   [p radius attrs]
;;   [:circle (merge {:cx (pc/x p)
;;                    :cy (pc/y p)
;;                    :r radius}
;;                   attrs)])

;; (defn diamond
;;   [p size attrs]
;;   (let [offset (/ size 2)]
;;     [:path (merge {:d (path-string (move-to p)
;;                                    (rmove-to 0 offset)
;;                                    (rline-to offset (- offset))
;;                                    (rline-to (- offset) (- offset))
;;                                    (rline-to (- offset) offset)
;;                                    (close))}
;;                   attrs)]))

;; (defn square
;;   [p size attrs]
;;   [:path (merge {:d (path-string (move-to p)
;;                                  (rmove-to (/ size 2) (/ size 2))
;;                                  (rline-to 0 (- size))
;;                                  (rline-to (- size) 0)
;;                                  (rline-to 0 size)
;;                                  (close))}
;;                 attrs)])

;; (defn triangle
;;   [p width height attrs]
;;   [:path (merge {:d (path-string (move-to p)
;;                                  (rmove-to 0 (/ height 2))
;;                                  (rline-to (/ width 2) (- height))
;;                                  (rline-to (- width) 0)
;;                                  (close))}
;;                 attrs)])

;; (defn text
;;   [p t attrs]
;;   [:text (merge {:x (pc/x p)
;;                  :y (pc/y p)}
;;                 attrs)
;;    t])

;; (let [counter (atom 0)]
;;   (defn unique-id
;;     [prefix]
;;     (str prefix (swap! counter inc))))

;; (defn curve-to
;;   [p11 p12 p21 p22]
;;   (let [mag  (* (pc/distance p12 p21) 0.5)
;;         c1 (->> p11
;;              (pc/scale -1)
;;              (pc/vector-add p12)
;;              pc/normalize
;;              (pc/scale mag)
;;              (pc/vector-add p12))
;;         c2 (->> p22
;;              (pc/scale -1)
;;              (pc/vector-add p21)
;;              pc/normalize
;;              (pc/scale mag)
;;              (pc/vector-add p21))]
;;     (str "C" (pc/x c1) "," (pc/y c1) "," (pc/x c2) "," (pc/y c2) "," (pc/x p21) "," (pc/y p21))))

;; (defn ^:export compute
;;   [e]
;;   (.debug js/console "compute")
;;   (let [input (get-input)
;;         target-steerpoint (get-numeric-value "target-steerpoint")
;;         output (pc/compute input)
;;         ;;solution (pc/find-solution input 100 500)
;;         ]
;;     (set-inner-html (by-id "mach")
;;                     (-> output :mach (.toFixed 2)))
;;     (set-inner-html (by-id "turn-radius")
;;                     (-> output :turn-radius (round 2)))

;;     ;; TGT-TO-VRP
;;     (let [{:keys [r theta z]} (pc/->vec :cyl (:vrp output))]
;;       (set-inner-html (by-id "tgt-to-vrp-tgt") target-steerpoint)
;;       (set-inner-html (by-id "tgt-to-vrp-brg")
;;                       (.toFixed (mod theta 360) 1))
;;       (set-inner-html (by-id "tgt-to-vrp-rng")
;;                       (round r 1))
;;       (set-inner-html (by-id "tgt-to-vrp-elev") (:ingress-altitude input)))

;;     ;; TGT-TO-PUP
;;     (let [{:keys [r theta z]} (pc/->vec :cyl (:pup output))]
;;       (set-inner-html (by-id "tgt-to-pup-tgt") target-steerpoint)
;;       (set-inner-html (by-id "tgt-to-pup-brg")
;;                       (.toFixed (mod theta 360) 1))
;;       (set-inner-html (by-id "tgt-to-pup-rng")
;;                       (round r 1))
;;       (set-inner-html (by-id "tgt-to-pup-elev") (:ingress-altitude input)))

;;     ;; Offset aimpoints
;;     (let [{:keys [theta r z]} (pc/->vec :cyl (:pull-down output))
;;           brg (.toFixed (mod theta 360) 1)
;;           rng (round r 1)]
;;       (set-inner-html (by-id "oa1-stpt") target-steerpoint)
;;       (set-inner-html (by-id "oa2-stpt") target-steerpoint)
;;       (set-inner-html (by-id "oa1-brg") brg)
;;       (set-inner-html (by-id "oa2-brg") brg)
;;       (set-inner-html (by-id "oa1-rng") rng)
;;       (set-inner-html (by-id "oa2-rng") rng)
;;       (set-inner-html (by-id "oa1-elev") (-> input
;;                                            :target-altitude
;;                                            (+ z)
;;                                            (round 2)))
;;       (set-inner-html (by-id "oa2-elev") 0))

;;     ;; Fill in profile details
;;     (doseq [[class content]
;;             {"ip-steerpoint"        (dec target-steerpoint)
;;              "target-steerpoint"    target-steerpoint
;;              "speed"                (:speed input)
;;              "turn-g"               (:turn-g input)
;;              "level-turn-direction" (.-value (by-id "pop-up-turn-direction"))
;;              "pup-heading"          (:heading-to-pull-down output)
;;              "climb-angle"          (:climb-angle input)
;;              "dive-angle"           (:dive-angle input)
;;              "release-altitude"     (:release-altitude input)}
;;             elem (by-class class)]
;;       (set-inner-html elem content))

;;     ;; Draw plan path
;;     ;; VRP, PUP, climb, pull-down, roll-out, release, target
;;     (let [plan                  (by-id "profile-plan")
;;           coordinates           [:vrp :pup :pull-up :climb :pull-down :roll-out :release :target]
;;           xs                    (mapv #(-> output % pc/x) coordinates)
;;           ys                    (mapv #(-> output % pc/y) coordinates)
;;           max-x                 (apply max xs)
;;           min-x                 (apply min xs)
;;           max-y                 (apply max ys)
;;           min-y                 (apply min ys)
;;           width                 (- max-x min-x)
;;           height                (- max-y min-y)
;;           padding               (* 0.05 (max width height))
;;           max-x                 (+ max-x padding)
;;           min-x                 (- min-x padding)
;;           max-y                 (+ max-y padding)
;;           min-y                 (- min-y padding)
;;           width                 (- max-x min-x)
;;           height                (- max-y min-y)
;;           aspect                (/ width height)
;;           svg-width             (-> plan .getBoundingClientRect .-width)
;;           svg-height            (-> plan .getBoundingClientRect .-height)
;;           svg-aspect            (/ svg-width svg-height)
;;           px-per-ft             (if (< svg-aspect aspect)
;;                                   (/ svg-width (* width 1.2))
;;                                   (/ svg-height (* height 1.2)))
;;           [cen-x cen-y]         [(/ (+ max-x min-x) 2.0)
;;                                  (/ (+ max-y min-y) 2.0)]
;;           [svg-cen-x svg-cen-y] [(/ (/ svg-width 2.0) px-per-ft)
;;                                  (/ (/ svg-height 2.0) (- px-per-ft))]
;;           [offset-x offset-y]   [(- svg-cen-x cen-x)
;;                                  (- svg-cen-y cen-y)]]
;;       (let [{:keys [vrp pup pull-up climb pull-down
;;                     roll-out release target turn-radius]}
;;             output
;;             transform (str "scale(" px-per-ft "," (- px-per-ft) ")"
;;                            " translate(" offset-x "," offset-y ")")
;;             stroke-width (/ 1.5  px-per-ft)
;;             marker-attrs {:stroke-width stroke-width
;;                           :style "stroke:#00bb00; fill:white; fill-opacity:0"}
;;             circle-attrs {:stroke-width stroke-width
;;                           :style "stroke:black; fill:red"}]
;;         (set-inner-html plan
;;                         (html [:g {:transform transform}
;;                                [:path {:style "stroke:#660000; fill:none;"
;;                                        :stroke-width stroke-width
;;                                        :d (path-string (move-to vrp)
;;                                                        (line-to pup)
;;                                                        (curve-to vrp pup pull-up climb)
;;                                                        (line-to climb)
;;                                                        (line-to pull-down)
;;                                                        (curve-to climb pull-down roll-out release)
;;                                                        (line-to release)
;;                                                        (line-to target))}]
;;                                [:g [:title "VRP"]
;;                                 (diamond vrp 800 marker-attrs)]
;;                                [:g [:title "PUP - start level turn here"]
;;                                 (circle pup 400 marker-attrs)]
;;                                [:g [:title "Pull-up - Start pull-up here"]
;;                                 (circle pull-up 200 circle-attrs)]
;;                                [:g [:title "Climb - Orient on pull-down (OA1) marker"]
;;                                 (circle climb 200 circle-attrs)]
;;                                [:g [:title "Pull-down point (OA1) - Roll and pull to target"]
;;                                 (triangle pull-down 500 1000 marker-attrs)]
;;                                [:g [:title "Roll-out wings level on attack heading"]
;;                                 (circle roll-out 200 circle-attrs)]
;;                                [:g [:title "Weapons release"]
;;                                 (circle release 200 circle-attrs)]
;;                                [:g [:title "Target"]
;;                                 (square target 600 marker-attrs)]]))))

;;     (set-inner-text "debug" (stringify {:inputs input
;;                                         :output output}))))

;; (def keycode
;;   {37 :left
;;    38 :up
;;    39 :right
;;    40 :down})

;; (defn handle-arrowable
;;   [e]
;;   (let [code (-> e (or (.-event js/window)) .-keyCode keycode)
;;         amount (-> e .-target (.getAttribute "data-arrow-amount") js/Number)
;;         current (-> e .-target .-value js/Number)
;;         mod-value (some-> e .-target (.getAttribute "data-mod-value") js/Number)]
;;     (when (#{:up :down} code)
;;       (set! (-> e .-target .-value)
;;             (let [new-val (+ current (if (= :up code)
;;                                        amount
;;                                        (- amount)))]
;;               (if mod-value
;;                 (mod new-val mod-value)
;;                 new-val))))))

;; (defn validate
;;   [e]
;;   (let [min-valid (some-> e (.getAttribute "data-min-valid") js/Number)
;;         max-valid (some-> e (.getAttribute "data-max-valid") js/Number)
;;         current (-> e .-value js/Number)]
;;     (when (and min-valid max-valid)
;;       (classes/enable e "invalid" (not (<= min-valid current max-valid))))))

;; (defn lock-toggle
;;   [e]
;;   (if (classes/has e "locked")
;;     (do
;;       (classes/remove e "locked")
;;       (doseq [elem (by-class "lock-target")]
;;         (when-not (= elem e)
;;           (classes/add e "locked"))))
;;     (do
;;       (classes/add e "locked")
;;       (classes/remove (->> (by-class "lock-target")
;;                            (remove #(= % e))
;;                            first)
;;                       "locked"))))

(comment
  (set! (.-onload js/window)
        (fn [e]
          (doseq [elem (by-class "recompute")]
            (set! (.-oninput elem)
                  (fn [e]
                    (validate (.-target e))
                    (compute (.-target e)))))
          (doseq [elem (by-class "arrowable")]
            (set! (.-onkeydown elem)
                  (fn [e]
                    (handle-arrowable e)
                    (validate (.-target e))
                    (compute (.-target e)))))
          (doseq [elem (by-class "lock-trigger")]
            (set! (.-onclick elem)
                  (fn [e]
                    (lock-toggle (.-target e)))))
          (compute e))))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
