(ns ludol.organism-test
  (:require
   [clojure.test :refer :all]
   [ludol.rings :as rings]
   [ludol.organism :refer :all]))

(def dead-organism
  {:eat {[:b 5] {:food 3}}
   :move {}
   :grow {}})

(def minimal-organism
  {:eat {[:d 8] {:food 1}}
   :move {[:d 9] {:food 1}}
   :grow {[:d 10] {:food 1}}})

(def unconnected-organism
  {:eat {[:d 8] {:food 1}}
   :move {[:d 9] {:food 1}}
   :grow {[:b 4] {:food 2}}})

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
