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

(def nearby-organism
  {[:c 7] {:role :eat :food 1}
   [:b 4] {:role :move :food 1}
   [:b 5] {:role :grow :food 1}})

(def unconnected-organism
  {[:d 8] {:role :eat :food 1}
   [:d 9] {:role :move :food 1}
   [:b 4] {:role :grow :food 2}})

(def orbius
  {:organisms
   [minimal-organism]})

(def plotonos
  {:organisms
   [nearby-organism]})

(deftest alive-test
  (is (alive? minimal-organism))
  (is (not (alive? dead-organism))))

(deftest adjacent-groups-test
  (let [rings (rings/generate-adjacencies [:a :b :c :d])
        spaces (spaces-for minimal-organism)
        groups (map vector spaces)
        adjacencies (rings/adjacent-groups rings groups)]
    (println adjacencies)))

(deftest connected-test
  (let [rings (rings/generate-adjacencies [:a :b :c :d])
        spaces (spaces-for minimal-organism)
        groups (rings/connected-groups rings spaces)]
    (println groups)
    (is (= 1 (count groups)))))

(deftest unconnected-test
  (let [rings (rings/generate-adjacencies [:a :b :c :d])
        spaces (spaces-for unconnected-organism)
        groups (rings/connected-groups rings spaces)]
    (println groups)
    (is (= 2 (count groups)))))

(deftest tag-test
  (let [tagged (tag-players [orbius plotonos])
        nodes (apply merge tagged)]
    (println nodes)
    (is (= 6 (count nodes)))))

(deftest conflict-test
  (let [nodes (apply merge (tag-players [orbius plotonos]))]
    (is (node-conflict? (get nodes [:d 10]) (get nodes [:c 7])))
    (is (not (node-conflict? (get nodes [:c 7]) (get nodes [:d 10]))))))

(deftest find-conflicts-test
  (let [rings (rings/generate-adjacencies [:a :b :c :d])
        removals (find-conflicts rings [orbius plotonos])]
    (println "rings" rings)
    (println "removals" removals)
    (is (= 1 (count removals)))
    (is (= [:d 10] (first removals)))))

(deftest find-conflicts-test
  (let [rings (rings/generate-adjacencies [:a :b :c :d])
        after (resolve-conflicts rings [orbius plotonos])]
    (println "rings" rings)
    (println "after" after)
    (is (= 2 (count (first (:organisms (first after))))))))
