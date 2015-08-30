(ns org.craigandera.popup-computer)

(def PI
  #? (:cljs Math/PI :clj Math/PI))

(defn deg->rad
  "Convert degrees to radians."
  [deg]
  (* PI (/ deg 180.0)))

(defn rad->deg
  "Convert radians to degrees."
  [rad]
  (* 180.0 (/ rad PI)))

(defn tan
  "Tangent of an angle in degrees"
  [angle]
  #?(:cljs (.tan js/Math (deg->rad angle))
           :clj (Math/tan (deg->rad angle))))

(defn cos
  "Cosine of an angle in degrees"
  [angle]
  #?(:cljs (.cos js/Math (deg->rad angle))
     :clj (Math/cos (deg->rad angle))))

(defn sin
  "Cosine of an angle in degrees"
  [angle]
  #?(:cljs (Math/sin (deg->rad angle))
           :clj (Math/sin (deg->rad angle))))

(defn atan
  "Arctan of a value. Result in degrees."
  [v]
  (rad->deg (Math/atan v)))

(defn acos
  "Arccos of a value. Result in degrees."
  [v]
  (rad->deg (Math/acos v)))

(defn sqrt
  "Square root"
  [x]
  (Math/sqrt x))

(defn reciprocal
  [heading]
  (if (< heading 180)
    (+ heading 180)
    (- heading 180)))

(defn mach
  [{:keys [altitude speed]}]
  ;; Source: http://www.tscm.com/mach-as.pdf
  (/ speed  (* 29.06 (sqrt (- 518.7 (* 3.57 (/ altitude 1000.0)))))))

(def turn-performance
  ;; G -> mach -> radius
  ;; TODO: This is all at sea level. Add data for different altitudes.
  {2 {0.30 2000
      0.36 3000
      0.42 4000
      0.52 6000
      0.60 8000
      0.66 10000
      0.94 20000}
   3 {0.38 2000
      0.46 3000
      0.54 4000
      0.66 6000
      0.76 8000
      0.86 10000}
   4 {0.45 2000
      0.55 3000
      0.63 4000
      0.77 6000
      0.90 8000
      1.0 10000}
   5 {0.50 2000
      0.62 4000
      0.71 6000
      1.00 8000
      1.12 10000}
   6 {0.55 2000
      0.66 3000
      0.78 4000
      0.96 6000
      1.10 8000}
   7 {0.60 2000
      0.73 3000
      0.84 4000
      1.40 6000}
   8 {0.64 2000
      0.78 3000
      0.90 4000
      1.10 6000}
   9 {0.68 2000
      0.83 3000
      0.96 4000
      1.18 6000}})

(defn bracket
  "Returns the map entries with the keys that most closely surround
  k."
  [m k]
  (loop [highest-low nil
         lowest-high nil
         [[this-k this-v :as entry] & more] (seq m)]
    (if-not entry
      [[highest-low (get m highest-low)]
       [lowest-high (get m lowest-high)]]
      (recur (if (<= (or highest-low this-k) this-k k)
               this-k
               highest-low)
             (if (<= k this-k (or lowest-high this-k))
               this-k
               lowest-high)
             more))))

(defn proportion
  [x xl xh yl yh]
  (if (= xl xh)
    yl
    (let [x-proportion  (/ (- x xl) (- xh xl))]
      (+ yl (* (- yh yl) x-proportion)))))

(defn turn-radius
  [{:keys [mach turn-g]}]
  (let [[[low-g low-perf]
         [high-g high-perf]] (bracket turn-performance turn-g)
        [[low-low-mach low-low-radius]
         [low-high-mach low-high-radius]] (bracket low-perf mach)
        [[high-low-mach high-low-radius]
         [high-high-mach high-high-radius]] (bracket high-perf mach)
        low-mach (proportion turn-g high-g low-g
                             low-low-mach high-low-mach)
        low-radius (proportion turn-g high-g low-g
                               low-low-radius high-low-radius)
        high-mach (proportion turn-g high-g low-g
                              low-high-mach high-high-mach)
        high-radius (proportion turn-g high-g low-g
                                low-high-radius high-high-radius)]
    (proportion mach low-mach high-mach
                low-radius high-radius)))

(defn kts->fts
  "Convert knots to feet/sec"
  [v]
  (/ (* v 6000.0) 3600.0))

(defn fts->kts
  "Convert feet/sec to knots"
  [v]
  (* 3600.0 (/ v 6000.0)))

(defn nm->ft
  "Convert nautical miles to feet"
  [nm]
  (* nm 6000))

(defn vector-type
  [v]
  (condp = (set (keys v))
    #{:x :y :z} :xyz
    #{:r :theta :phi} :sph
    #{:r :theta :z} :cyl))

(defmulti ->vec (fn [t v] [(vector-type v) t]))

(defmethod ->vec [:sph :sph]
  [_ v]
  v)

(defmethod ->vec [:cyl :cyl]
  [_ v]
  v)

(defmethod ->vec [:xyz :xyz]
  [_ v]
  v)

(defmethod ->vec [:sph :xyz]
  [_ {:keys [theta phi r]}]
  {:x (* r (sin theta) (cos phi))
   :y (* r (cos theta) (cos phi))
   :z (* r (sin phi))})

(defmethod ->vec [:sph :cyl]
  [_ {:keys [theta phi r]}]
  {:theta theta
   :r (* r (cos phi))
   :z (* r (sin phi))})

(defmethod ->vec [:xyz :sph]
  [_ {:keys [x y z]}]
  (let [rxy (sqrt (+ (* x x) (* y y)))]
    {:theta (if (zero? y)
              (if (pos? x) 90 -90)
              (+ (atan (/ x y))
                 (if (neg? y)
                   180
                   0)))
     :phi   (if (zero? rxy)
              (if (pos? z) 90 -90)
              (atan (/ z rxy)))
     :r     (sqrt (+ (* x x) (* y y) (* z z)))}))

(defmethod ->vec [:cyl :xyz]
  [_ {:keys [r theta z]}]
  {:x (* r (sin theta))
   :y (* r (cos theta))
   :z z})

(defmethod ->vec [:xyz :cyl]
  [_ v]
  (->vec :cyl (->vec :sph v)))

(defmethod ->vec [:cyl :sph]
  [_ v]
  (->> v (->vec :xyz) (->vec :sph)))

(defn x
  "Return the x component of a vector"
  [v]
  (or (:x v)
      (->> v (->vec :xyz) :x)))

(defn y
  "Return the y component of a vector"
  [v]
  (or (:y v)
      (->> v (->vec :xyz) :y)))

(defn z
  "Return the z component of a vector"
  [v]
  (or (:z v)
      (->> v (->vec :xyz) :z)))

(defn vector-add
  ([v] v)
  ([v1 v2]
   (let [v1* (->vec :xyz v1)
         v2* (->vec :xyz v2)]
     {:x (+ (:x v1*) (:x v2*))
      :y (+ (:y v1*) (:y v2*))
      :z (+ (:z v1*) (:z v2*))}))
  ([v1 v2 & more]
   (reduce vector-add (vector-add v1 v2) more)))

(defn dot
  "Returns the dot product of two vectors"
  [a b]
  (let [a* (->vec :xyz a)
        b* (->vec :xyz b)]
   (+ (* (:x a*) (:x b*))
      (* (:y a*) (:y b*))
      (* (:z a*) (:z b*)))))

(defn magnitude
  "Return the magnitude of a vector"
  [v]
  (let [{:keys [x y z]} (->vec :xyz v)]
   (Math/sqrt (+ (* x x) (* y y) (* z z)))))

(defn normalize
  "Normalizes a vector to length one"
  [v]
  (let [{:keys [x y z]} (->vec :xyz v)
        l (magnitude v)]
    {:x (/ x l)
     :y (/ y l)
     :z (/ z l)}))

(defn scale
  "Scales vector `v` by a scalar `s`."
  [s v]
  (let [v* (->vec :sph v)]
    (update-in v* [:r] #(* s %))))

(defn angle-between
  "Returns the angle in degrees between two polar vectors."
  [a b]
  (acos (dot (normalize a) (normalize b))))

(defn turn
  "Compute the end location of a turn starting at the given starting
  location and direction, ending in the specified direction."
  [start-loc start-vec end-vec turn-radius]
  (let [xd (* turn-radius
              (tan (/ (angle-between start-vec end-vec)
                      2.0)))
        n1 (normalize start-vec)
        n2 (normalize end-vec)]
    (vector-add start-loc
                (scale xd n1)
                (scale xd n2))))

;; Target is origin
(defn compute
  [{:keys [attack-heading dive-angle release-altitude
           tracking-time speed target-altitude
           climb-angle ingress-altitude
           vrp-to-pup-distance pop-up-turn-amount
           pull-down-turn-amount turn-g]
    :as inputs}]
  (let [mach                 (mach {:speed    speed
                                    :altitude (+ target-altitude
                                                 ingress-altitude)})
        turn-radius          (turn-radius {:mach   mach
                                           :turn-g turn-g})
        attack-recip         (reciprocal attack-heading)
        ;; Release point
        release              {:theta attack-recip
                              :phi dive-angle
                              :r (/ release-altitude (tan dive-angle))}
        tracking-dist        (* tracking-time (kts->fts speed))
        ;; Roll-out point - attack dive starts here
        roll-out             (vector-add release
                                         {:theta attack-recip
                                          :phi dive-angle
                                          :r (/ tracking-dist
                                                (cos dive-angle))})
        heading-to-pull-down (reciprocal (- attack-recip pull-down-turn-amount))
        ;; Where we start the pull toward the attack heading
        pull-down            (turn roll-out
                                   {:theta attack-recip
                                    :phi dive-angle
                                    :r 1.0}
                                   {:theta (reciprocal heading-to-pull-down)
                                    :phi (- climb-angle)
                                    :r 1.0}
                                   turn-radius)
        alt-gain-during-pup  (* turn-radius (- 1 (cos climb-angle)))
        climb                (vector-add pull-down
                                         {:theta (reciprocal heading-to-pull-down)
                                          :phi (- climb-angle)
                                          :r (/ alt-gain-during-pup
                                                (sin climb-angle))})
        pull-up              (turn climb
                                   {:theta (reciprocal heading-to-pull-down)
                                    :phi (- climb-angle)
                                    :r 1.0}
                                   {:theta (reciprocal heading-to-pull-down)
                                    :phi 0
                                    :r 1.0}
                                   turn-radius)
        pup-heading-recip    (reciprocal (- heading-to-pull-down pop-up-turn-amount))
        pup                  (turn pull-up
                                   {:theta (reciprocal heading-to-pull-down)
                                    :phi 0
                                    :r 1.0}
                                   {:theta pup-heading-recip
                                    :phi 0
                                    :r 1.0}
                                   turn-radius)
        vrp                  (vector-add pup
                                         {:theta pup-heading-recip
                                          :phi   0
                                          :r     (nm->ft vrp-to-pup-distance)})]
    {:turn-radius   turn-radius
     :mach          mach
     :attack-recip  attack-recip
     :release       release
     :tracking-dist tracking-dist
     :roll-out      roll-out
     :pull-down     pull-down
     :climb         climb
     :heading-to-pull-down (mod heading-to-pull-down 360)
     :pull-up       pull-up
     :pup-heading   (mod (reciprocal pup-heading-recip) 360)
     :pup           pup
     :vrp           vrp
     :target        {:x 0 :y 0 :z 0}}))

(defn distance
  "Distance between two vectors"
  [v1 v2]
  (-> v1
    (vector-add (scale -1 v2))
    magnitude))

(defn improve
  "Return a set of inputs that are better than init at minimizing the
  error between pull-down and pull-down2."
  [factor init]
  (let [heading-delta (* 10 factor)
        distance-delta (* 2 factor)]
    (->> (for [tgt-to-vrp-heading-delta [(- heading-delta) 0 heading-delta]
               tgt-to-vrp-distance-delta [(- distance-delta) 0 distance-delta]]
           (let [input  (-> init
                          (update-in [:tgt-to-vrp-heading]
                                     + tgt-to-vrp-heading-delta)
                          (update-in [:tgt-to-vrp-heading]
                                  #(mod % 360))
                          (update-in [:tgt-to-vrp-distance]
                                     + tgt-to-vrp-distance-delta)
                          (update-in [:tgt-to-vrp-distance]
                                     #(max % 0)))
                 output (compute input)]
             {:input input
              :output output
              :error (distance (:pull-down output)
                               (:pull-down2 output))}))
      (sort-by :error)
      first)))

(defn find-solution
  [init max-iters acceptable-error]
  (loop [factor      1.0
         iters       0
         data         {:input  init
                       :output (compute init)
                       :error  (* acceptable-error 2.0)}]
    (let [{:keys [input output error]} data]
      (cond
        (< max-iters iters) nil
        (< error acceptable-error) (assoc data :iters iters)
        :else
        (let [improved (improve factor input)]
          (recur (if (= (:input improved) input)
                   (* factor 0.5)
                   factor)
                 (inc iters)
                 improved))))))

