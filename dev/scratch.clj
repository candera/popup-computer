(->> (pc/compute {:attack-heading 270
                  :dive-angle 20
                  :release-altitude 2000
                  :tracking-time 2.0
                  :speed 500
                  :turn-g 3.0
                  :climb-angle 25
                  :heading-to-pull-down 360
                  :ingress-altitude 200
                  :tgt-to-vrp-heading 180
                  :tgt-to-vrp-distance 3.0
                  :vrp-to-pup-distance 1.0})
  :pull-up
  (pc/->vec :cyl)
  pprint)

(pc/compute {:attack-heading 270
             :dive-angle 20
             :release-altitude 2000
             :tracking-time 2.0
             :speed 500
             :turn-g 3.0
             :climb-angle 25
             :heading-to-pull-down 360
             :ingress-altitude 200
             :tgt-to-vrp-heading 180
             :tgt-to-vrp-distance 3.0
             :vrp-to-pup-distance 1.0})

(let [error (fn [{:keys [pull-up pull-up2]}]
              (->> pull-up2
                (pc/scale -1)
                (pc/vector-add pull-up)
                pc/magnitude))
      new-climb (fn [{:keys [pull-up2 pull-down]}]
                  (->> pull-up2
                    (pc/scale -1)
                    (pc/vector-add pull-down)))]
  (-> (pc/compute {:attack-heading 270
                   :dive-angle 20
                   :release-altitude 2000
                   :tracking-time 2.0
                   :speed 500
                   :turn-g 3.0
                   :climb-angle 20
                   :heading-to-pull-down 45
                   :ingress-altitude 200
                   :tgt-to-vrp-heading 180
                   :tgt-to-vrp-distance 10.0
                   :vrp-to-pup-distance 1.0})
    ;;:roll-out
    ;;(pc/->vec :cyl)
;;new-climb
    ;;(pc/->vec :sph)
    (update-in [:roll-out] #(pc/->vec :cyl %))
    (update-in [:pull-up] #(pc/->vec :cyl %))
    (update-in [:pull-up2] #(pc/->vec :cyl %))
    (update-in [:release] #(pc/->vec :cyl %))
    (update-in [:pull-down] #(pc/->vec :cyl %))
    pprint
    ))

(-> (pc/compute {:attack-heading 0
                 :dive-angle 0
                 :release-altitude 200
                 :tracking-time 2.0
                 :speed 500
                 :turn-g 3.0
                 :climb-angle 0
                 :heading-to-pull-down 0
                 :ingress-altitude 200})
  pprint)

(pc/tan 20)

(in-ns 'boot.user)

(pc/fts->kts (pc/kts->fts 500))

(pc/vector-add {:theta 90 :phi 0 :r 2}
               {:theta -90 :phi 0 :r 2})

(pc/polar->xyz {:theta 90 :phi 0 :r 2})

(pc/xyz->polar {:x 1 :y 1 :z 1.414})

(pc/atan 1)

(pc/angle-between {:theta 0 :phi 0 :r 1}
                  {:theta 270 :phi 0 :r 10})

(pc/normalize (pc/polar->xyz {:theta 0 :phi 45 :r 10}))

(pc/polar->cyl {:r 1 :theta 0 :phi 90})

(pc/dot )

(pc/turn {:x 0 :y 0 :z 0} ; Start loc
         {:x 0 :y 1 :z 0} ; start vec
         {:x 1 :y 0 :z 0} ; end vec
         1)                             ; turn radius

(pc/angle-between {:x 0 :y 1 :z 0}      ; start vec
                  {:x 1 :y 0 :z 0}      ; end vec
                  )

(let [turn-radius 2.0
      start-pos   {:x 0 :y 0 :z 0}
      start-vec   {:x 0 :y 1 :z 0}      ; start vec
      end-vec     {:x 1 :y 1 :z 0}      ; end vec
      xd (/ turn-radius
            (pc/tan (/ (pc/angle-between start-vec end-vec)
                       2.0)))
      n1 (pc/normalize start-vec)
      n2 (pc/normalize end-vec)
      n1* (pc/scale xd n1)
      n2* (pc/scale xd n2)]
  {:xd    xd
   :n1    n1
   :n2    n2
   :n1*   (pc/->vec :xyz n1*)
   :n2*   (pc/->vec :xyz n2*)
   :start start-vec
   :final (pc/vector-add start-pos
                         n1*
                         n2*)}

  )

(pc/vector-add {:x 1 :y 0 :z 0}
               {:x 1 :y 1 :z 0}
               {:x 1 :y 1 :z 0})

(pc/turn {:x 0 :y 0 :z 0} ; Start loc
         {:x 0 :y 1 :z 0} ; start vec
         {:x 1 :y 1 :z 0} ; end vec
         1)               ; turn radius

(pc/tan (/ (pc/angle-between {:x 0 :y 1 :z 0} ; start vec
                            {:x 1 :y 1 :z 0} ; end vec
                            )
           2.0)) ; 0.41421356237309515

(pc/scale 0.41421356237309515
 (pc/normalize          {:x 1 :y 0 :z 0} ; start vec

                        ))

(pc/turn {:x 0 :y 0 :z 0}
         {:x 1 :y 1 :z 1}
         {:x 1 :y 0 :z 0}
         1)



(require '[incanter.core :as incanter])
(require '[incanter.charts :as charts])

(let [data (-> (pc/compute {:attack-heading 270
                            :dive-angle 20
                            :release-altitude 2000
                            :tracking-time 2.0
                            :speed 500
                            :turn-g 3.0
                            :climb-angle 20
                            :heading-to-pull-down 45
                            :ingress-altitude 200
                            :tgt-to-vrp-heading 180
                            :tgt-to-vrp-distance 10.0
                            :vrp-to-pup-distance 1.0}))]
  (doto (charts/scatter-plot)
    (charts/add-lines [0
                       (-> data :release pc/x)
                       (-> data :roll-out pc/x)
                       (-> data :pull-down pc/x)]
                      [0
                       (-> data :release pc/y)
                       (-> data :roll-out pc/y)
                       (-> data :pull-down pc/y)]
                      :points true)
    (charts/add-lines [(-> data :vrp pc/x)
                       (-> data :pup pc/x)
                       (-> data :pull-up pc/x)
                       (-> data :pull-down2 pc/x)]
                      [(-> data :vrp pc/y)
                       (-> data :pup pc/y)
                       (-> data :pull-up pc/y)
                       (-> data :pull-down2 pc/y)]
                      :points true)
    (incanter/view)))

(->> (pc/compute {:attack-heading 270
                  :dive-angle 20
                  :release-altitude 2000
                  :tracking-time 2.0
                  :speed 500
                  :turn-g 3.0
                  :climb-angle 20
                  :heading-to-pull-down 45
                  :ingress-altitude 200
                  :tgt-to-vrp-heading 180
                  :tgt-to-vrp-distance 10.0
                  :vrp-to-pup-distance 1.0})
  :pup
  pc/y)


(-> (pc/compute {:attack-heading 270
                 :dive-angle 20
                 :release-altitude 2000
                 :tracking-time 2.0
                 :speed 500
                 :turn-g 3.0
                 :climb-angle 20
                 :pop-up-turn-amount 45
                 :ingress-altitude 200
                 :tgt-to-vrp-heading 180
                 :tgt-to-vrp-distance 10.0
                 :vrp-to-pup-distance 1.0})
  user/plot)


(let [init {:attack-heading 270
            :dive-angle 20
            :release-altitude 2000
            :tracking-time 2.0
            :speed 500
            :turn-g 3.0
            :climb-angle 20
            :pop-up-turn-amount 45
            :ingress-altitude 200
            :tgt-to-vrp-heading 180
            :tgt-to-vrp-distance 10.0
            :vrp-to-pup-distance 1.0}]
  (->> (for [tgt-to-vrp-heading-delta [5 0 -5]
             tgt-to-vrp-distance-delta [1 0 -1]]
         (let [input (-> init
                       (update-in [:tgt-to-vrp-heading]
                                  + tgt-to-vrp-heading-delta)
                       (update-in [:tgt-to-vrp-distance]
                                  + tgt-to-vrp-distance-delta))
               output (pc/compute input)]
           {:input input
            :output output
            :error (pc/distance (:pull-down output)
                                (:pull-down2 output))}))
    (sort-by :error)
    first
    :error))

(let [output (pc/compute {:attack-heading 270
                          :dive-angle 20
                          :release-altitude 2000
                          :tracking-time 2.0
                          :speed 500
                          :turn-g 3.0
                          :climb-angle 20
                          :pop-up-turn-amount 45
                          :ingress-altitude 200
                          :tgt-to-vrp-heading 180
                          :tgt-to-vrp-distance 10.0
                          :vrp-to-pup-distance 1.0})]
  (pc/distance (:pull-down output)
               (:pull-down2 output)))

(->> (iterate pc/improve {:attack-heading 270
                          :dive-angle 20
                          :release-altitude 2000
                          :tracking-time 2.0
                          :speed 500
                          :turn-g 3.0
                          :climb-angle 20
                          :pop-up-turn-amount 45
                          :ingress-altitude 200
                          :tgt-to-vrp-heading 180
                          :tgt-to-vrp-distance 10.0
                          :vrp-to-pup-distance 1.0})
  (take 5)
  (map pc/compute)
  (map #(pc/distance (:pull-down %)
                     (:pull-down2 %))))


(org.craigandera.popup-computer/find-solution
 {:attack-heading 270
  :dive-angle 20
  :release-altitude 2000
  :tracking-time 2.0
  :speed 500
  :turn-g 3.0
  :climb-angle 20
  :pop-up-turn-amount 45
  :ingress-altitude 200
  :tgt-to-vrp-heading 180
  :tgt-to-vrp-distance 10.0
  :vrp-to-pup-distance 1.0}
 10
 1000)

(->> (pc/improve 1.0
                 {:attack-heading 270
                  :dive-angle 20
                  :release-altitude 2000
                  :tracking-time 2.0
                  :speed 500
                  :turn-g 3.0
                  :climb-angle 20
                  :pop-up-turn-amount 45
                  :ingress-altitude 200
                  :tgt-to-vrp-heading 180
                  :tgt-to-vrp-distance 10.0
                  :vrp-to-pup-distance 1.0})
  :input
  (pc/improve 1.0)
  :input
  (pc/improve 1.0)
  :input
  (pc/improve 1.0)
  :input
  (pc/improve 1.0)
  :input
  (pc/improve 1.0)
  :input
  (pc/improve 1.0)
  :input
  (pc/improve 0.5)
  :input
  (pc/improve 0.25)
  :input
  (pc/improve 0.125)
  :error)


(org.craigandera.popup-computer/find-solution
 {:attack-heading 270
  :dive-angle 20
  :release-altitude 2000
  :tracking-time 2.0
  :speed 500
  :turn-g 3.0
  :climb-angle 20
  :pop-up-turn-amount 45
  :ingress-altitude 200
  :tgt-to-vrp-heading 0
  :tgt-to-vrp-distance 5.0
  :vrp-to-pup-distance 1.0}
 10
 1000)

(->> {:input  {:attack-heading 270
               :dive-angle 20
               :release-altitude 2000
               :tracking-time 2.0
               :speed 500
               :turn-g 3.0
               :climb-angle 20
               :pop-up-turn-amount 45
               :ingress-altitude 200
               :tgt-to-vrp-heading 0
               :tgt-to-vrp-distance 5.0
               :vrp-to-pup-distance 1.0}}
  (iterate #(pc/improve (+ 0.25 (rand 1.75)) (:input %)))
  (take 3)
  (map (juxt :error
             (comp :tgt-to-vrp-distance :input)
             (comp :tgt-to-vrp-heading :input)))
  pprint)


(pc/compute {:attack-heading 270
             :dive-angle 20
             :release-altitude 2000
             :tracking-time 2.0
             :speed 500
             :turn-g 3.0
             :climb-angle 20
             :pop-up-turn-amount 45
             :pull-down-turn-amount -90
             :ingress-altitude 200
             :vrp-to-pup-distance 1.0})

(defn curve-to
  [radius p11 p12 p21 p22]
  (let [c1 (->> p11 (pc/scale -1) (pc/vector-add p12) pc/normalize (pc/scale radius))
        c2 (->> p22 (pc/scale -1) (pc/vector-add p21) pc/normalize (pc/scale radius))]
   (str "C" (pc/x c1) "," (pc/y c1) "," (pc/x c2) "," (pc/y c2) "," (pc/x p12) "," (pc/y p12))))
