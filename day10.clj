(ns Projects.own.advent-of-code-2019.day10
  (:require [clojure.string :as str]))

(def puzzle-input
  "#..#.#.#.######..#.#...##
  ##.#..#.#..##.#..######.#
  .#.##.#..##..#.#.####.#..
  .#..##.#.#..#.#...#...#.#
  #...###.##.##..##...#..#.
  ##..#.#.#.###...#.##..#.#
  ###.###.#.##.##....#####.
  .#####.#.#...#..#####..#.
  .#.##...#.#...#####.##...
  ######.#..##.#..#.#.#....
  ###.##.#######....##.#..#
  .####.##..#.##.#.#.##...#
  ##...##.######..##..#.###
  ...###...#..#...#.###..#.
  .#####...##..#..#####.###
  .#####..#.#######.###.##.
  #...###.####.##.##.#.##.#
  .#.#.#.#.#.##.#..#.#..###
  ##.#.####.###....###..##.
  #..##.#....#..#..#.#..#.#
  ##..#..#...#..##..####..#
  ....#.....##..#.##.#...##
  .##..#.#..##..##.#..##..#
  .##..#####....#####.#.#.#
  #..#..#..##...#..#.#.#.##")

(defn analyze-map [map-input]
  (let [m (->> map-input
               str/split-lines
               (map str/trim)
               (map vec))
        h (count m)
        w (count (first m))
        coord-map (apply concat
                         (map-indexed (fn [y row]
                                        (map-indexed
                                         (fn [x item]
                                           [[x y] item]) row)) m))
        asteroids (->> coord-map
                       (filter (fn [[coord item]] (= item \#)))
                       (map first)
                       set)]
    {:height h
     :width w
     :asteroids asteroids}))

(defn quadrant [[x y]]
  (cond
    (and (pos? x) (zero? y)) 2
    (and (pos? x) (pos? y)) 2
    (and (zero? x) (pos? y)) 3
    (and (neg? x) (pos? y)) 3
    (and (neg? x) (zero? y)) 4
    (and (neg? x) (neg? y)) 4
    (and (zero? x) (neg? y)) 1
    (and (pos? x) (neg? y)) 1))

(defn angle [[x y]]
  (if (zero? x)
    0
    (/ (Math/abs y)
       (Math/abs x))))

(defn distance [[x y]]
  (+ (Math/abs x)
     (Math/abs y)))

(defn analyze-asteroid [center asteroid]
  (let [[center-x center-y] center
        [asteroid-x asteroid-y] asteroid
        rel-pos [(- asteroid-x center-x)
                 (- asteroid-y center-y)]
        q (quadrant rel-pos)
        a (angle rel-pos)
        d (distance rel-pos)]
    {:quadrant q
     :angle (float a)
     :distance d
     :asteroid asteroid
     :rel-pos rel-pos}))

(defn group-by-angle [asteroids]
  (->> asteroids
       (group-by (juxt :quadrant :angle))))

(defn visible-asteroids [asteroids]
  (->> asteroids
       group-by-angle
       vals
       (map #(first (sort-by :distance %)))))

(defn visible-count [asteroids center]
  (let [v-count (->> (disj asteroids center)
                     (map #(analyze-asteroid center %))
                     visible-asteroids
                     count)]
    [center v-count]))

(defn optimal-position [asteroids]
  (->> asteroids
       (map (partial visible-count asteroids))
       (sort-by second)
       last))

(defn queue [& items]
  (if (seq items)
    (apply conj clojure.lang.PersistentQueue/EMPTY items)
    clojure.lang.PersistentQueue/EMPTY))

(defn queue-by-angle [center asteroids]
  (->> (disj asteroids center)
       (map #(analyze-asteroid center %))
       group-by-angle
       (sort-by first)
       vals
       (apply conj clojure.lang.PersistentQueue/EMPTY)))

(defn vaporize [q]
  (loop [q q
         vaporized []]
    (let [angle (peek q)
          q (pop q)
          [fst & rst] angle
          q (if (seq rst)
              (conj q rst)
              q)]
      (if (seq q)
        (recur q (conj vaporized fst))
        vaporized))))

(comment
  (let [{:keys [asteroids]} (analyze-map puzzle-input)]
    (optimal-position asteroids))

  (let [{:keys [asteroids]} (analyze-map puzzle-input)
        center [11 19]
        queue (queue-by-angle center asteroids)
        vaporized (vaporize queue)]
    (nth vaporized (dec 200)))
  )
