(ns Projects.own.advent-of-code-2019.day12)

(def moons
  {:io       {:pos {:x -7  :y -8  :z 9}  :vel {:x 0 :y 0 :z 0}}
   :europa   {:pos {:x -12 :y -3  :z -4} :vel {:x 0 :y 0 :z 0}}
   :ganymede {:pos {:x 6   :y -17 :z -9} :vel {:x 0 :y 0 :z 0}}
   :callisto {:pos {:x 4   :y -10 :z -6} :vel {:x 0 :y 0 :z 0}}})

(defn moon-pairs [moons]
  (set
   (for [moon1 (keys moons)
         moon2 (keys moons)
         :when (not= moon1 moon2)]
     [moon1 moon2])))

(comment
  (moon-pairs moons)
  )

(defn calc-velocity [vel m1-pos m2-pos]
  (cond
    (< m1-pos m2-pos) (inc vel)
    (> m1-pos m2-pos) (dec vel)
    :else vel))

(defn update-velocity [m1 m2 coord]
  (let [m1-pos (-> m1 :pos coord)
        m2-pos (-> m2 :pos coord)]
    (update-in m1 [:vel coord] calc-velocity m1-pos m2-pos)))

(comment
  (update-velocity (:io moons) (:europa moons) :x)
  )

(defn update-velocities [m1 m2]
  (-> m1
      (update-velocity m2 :x)
      (update-velocity m2 :y)
      (update-velocity m2 :z)))

(defn update-all-velocities [moons]
  (reduce
   (fn [moons pair]
     (let [[m1-name m2-name] (seq pair)
           m1 (get moons m1-name)
           m2 (get moons m2-name)]
       (update moons m1-name update-velocities m2)))
   moons
   (moon-pairs moons)))

(comment
  (update-all-velocities moons)
  )

(defn sum-pos [p1 p2]
  {:x (+ (:x p1) (:x p2))
   :y (+ (:y p1) (:y p2))
   :z (+ (:z p1) (:z p2))})

(defn apply-velocity [m]
  (update m :pos sum-pos (:vel m)))

(defn simulate-one [moons]
  (into {}
        (map (fn [[moon-name moon]]
               [moon-name (apply-velocity moon)]))
        (update-all-velocities moons)))

(defn abs-sum [coord]
  (->> (vals coord)
       (map #(Math/abs %))
       (apply +)))

(defn pot [moon]
  (abs-sum (:pos moon)))

(defn kin [moon]
  (abs-sum (:vel moon)))

(defn total-energy [moon]
  (* (pot moon) (kin moon)))

(defn total-energy-all [moons]
  (apply +
         (map (fn [[_ moon]]
                (total-energy moon)) moons)))

(defn simulate [n moons]
  (last (take (inc n) (iterate simulate-one moons))))

(defn hash [moons coord]
  (map (fn [[k v]] [(-> v :pos coord) (-> v :vel coord)]) moons))

(defn repeated? [initial candidate coord]
  (= (hash initial coord) (hash candidate coord)))

(defn simulate-until-repeat [moons]
  (let [simulation-seq (iterate simulate-one moons)
        initial (first simulation-seq)]
    (loop [simulation-seq (rest simulation-seq)
           i 1
           repeats {}]
      (let [candidate (first simulation-seq)
            x-rep (or (:x repeats) (when (repeated? initial candidate :x) i))
            y-rep (or (:y repeats) (when (repeated? initial candidate :y) i))
            z-rep (or (:z repeats) (when (repeated? initial candidate :z) i))
            repeats {:x x-rep
                     :y y-rep
                     :z z-rep}]
        (if (and x-rep y-rep z-rep)
          repeats
          (recur (rest simulation-seq) (inc i) repeats))))))

(defn gcd
  ([a b]
   (if (zero? b)
     a
     (recur b (mod a b))))
  ([a b c] (gcd a (gcd b c))))

(defn lcm
  ([a b]
   (/ (* a b) (gcd a b)))
  ([a b c]
   (lcm a (lcm b c))))

(comment
  (def example-moons
    {:m1 {:pos {:x -1 :y 0,  :z 2}  :vel {:x 0 :y 0 :z 0}}
     :m2 {:pos {:x 2  :y -10 :z -7} :vel {:x 0 :y 0 :z 0}}
     :m3 {:pos {:x 4  :y -8  :z 8}  :vel {:x 0 :y 0 :z 0}}
     :m4 {:pos {:x 3  :y 5   :z -1} :vel {:x 0 :y 0 :z 0}}})

  (simulate-one example-moons)

  (vals example-moons)

  (map (fn [[k v]] [(-> v :pos :x) (-> v :vel :x)]) example-moons)

  (time
   (doall
    (simulate 2772 example-moons)))

  (time
   (lcm ((juxt :x :y :z) (simulate-until-repeat example-moons))))

  (time
   (total-energy-all
    (simulate 100000 example-moons)))

  (total-energy-all
   (simulate 1000 moons))

  (time
   (simulate-until-match moons))


  (time
   (lcm ((juxt :x :y :z) (simulate-until-repeat moons))))

  (apply lcm [186028 28482 231614])



  )
