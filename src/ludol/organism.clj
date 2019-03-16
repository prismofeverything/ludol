(ns ludol.organism)

;; primus
;; secundus
;; tertius
;; quartus

(defn map-cat
  [f s]
  (reduce into [] (map f s)))

(def ring-spaces
  [[:a 1]
   [:b 6]
   [:c 12]
   [:d 18]])

(def spaces-per-ring
  (into {} ring-spaces))

(defn make-rings
  [spaces]
  (map-cat
   (fn [[ring numeral]]
     (map
      (fn [index]
        [ring index])
      (range numeral)))
   spaces))

(defn spaces-for
  [ring]
  (map
    (fn [space]
      [ring space])
    (range (spaces-per-ring ring))))

(defn same-ring-adjacencies
  [ring n]
  (let [total (spaces-per-ring ring)
        clockwise (mod (inc n) total)
        counterclockwise (mod (dec n) total)]
    [[ring clockwise] [ring counterclockwise]]))

(defn a-space-adjacencies
  [n]
  (spaces-for :b))

(defn b-space-adjacencies
  [n]
  (let [a [[:a 0]]

        b (same-ring-adjacencies :b n)

        up (* 2 n)
        c-total (spaces-per-ring :c)
        c (mapv
           (fn [m]
             [:c (mod (dec (+ up m)) c-total)])
           (range 3))]

    (reduce into [] [a b c])))

(defn c-space-adjacencies
  [n]
  (let [b (if (even? n)
            [[:b (quot n 2)]]
            (mapv (fn [m] [:b (+ (quot n 2) m)]) (range 2)))

        c (same-ring-adjacencies :c n)

        up (* (quot n 2) 3)
        d-total (spaces-per-ring :d)
        d (if (even? n)
            (mapv
             (fn [m]
               [:d (mod (dec (+ up m)) d-total)])
             (range 3))
            (mapv
             (fn [m]
               [:d (mod (inc (+ up m)) d-total)])
             (range 2)))]

    (reduce into [] [b c d])))

(defn d-space-adjacencies
  [n]
  (let [down (* 2 (quot n 3))
        c-total (spaces-per-ring :c)
        c (cond
            (zero? (mod n 3))
            [[:c down]]

            (= 1 (mod n 3))
            (mapv
             (fn [m]
               [:c (mod (+ down m) c-total)])
             (range 2))

            (= 2 (mod n 3))
            (mapv
             (fn [m]
               [:c (mod (inc (+ down m)) c-total)])
             (range 2)))

        d (same-ring-adjacencies :d n)]
    (reduce into [] [c d])))

(def space-adjacencies
  {:a a-space-adjacencies
   :b b-space-adjacencies
   :c c-space-adjacencies
   :d d-space-adjacencies})

(defn ring-adjacencies
  [ring]
  (map
   (fn [space]
     (let [n (last space)]
       [[ring n]
        ((space-adjacencies ring) n)]))
   (spaces-for ring)))

(defn generate-adjacencies
  [rings]
  (mapv ring-adjacencies rings))

(defn initial-state
  []
  {:spaces (make-rings ring-spaces)
   :adjacencies (generate-adjacencies (map first ring-spaces))})
