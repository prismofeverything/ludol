(ns ludol.organism-test
  (:require
   [clojure.test :refer :all]
   [ludol.rings :as rings]
   [ludol.organism :refer :all]))

(def dead-organism
  {[:b 5] {:role :eat :food 3}})

(def minimal-organism
  {[:d 8] {:role :eat :food 1}
   [:d 9] {:role :move :food 1}
   [:d 10] {:role :grow :food 1}})

(def unconnected-organism
  {[:d 8] {:role :eat :food 1}
   [:d 9] {:role :move :food 1}
   [:b 4] {:role :grow :food 2}})

(deftest alive-test
  (is (alive? minimal-organism))
  (is (not (alive? dead-organism))))

(deftest adjacent-groups-test
  (let [rings (rings/generate-adjacencies [:a :b :c :d])
        spaces (spaces-for minimal-organism)
        groups (map vector spaces)
        adjacencies (adjacent-groups rings groups)]
    (println adjacencies)))

(deftest connected-test
  (let [rings (rings/generate-adjacencies [:a :b :c :d])
        spaces (spaces-for minimal-organism)
        groups (connected-groups rings spaces)]
    (println groups)
    (is (= 1 (count groups)))))

(deftest unconnected-test
  (let [rings (rings/generate-adjacencies [:a :b :c :d])
        spaces (spaces-for unconnected-organism)
        groups (connected-groups rings spaces)]
    (println groups)
    (is (= 2 (count groups)))))
