(ns ludol.rings-test
  (:require
   [clojure.test :refer :all]
   [ludol.rings :refer :all]))

(deftest a-ring-test
  (let [adjacencies (a-space-adjacencies 1)]
    (is (= 6 (count adjacencies)))))

(deftest b-ring-test
  (let [adjacencies (b-space-adjacencies 2)
        rings (frequencies (map first adjacencies))]
    (println adjacencies)
    (is (= 6 (count adjacencies)))
    (is (= 3 (count rings)))
    (is (= 3 (get rings :c)))))

(deftest c-ring-test
  (let [adjacencies (c-space-adjacencies 3)
        rings (frequencies (map first adjacencies))]
    (println adjacencies)
    (is (= 6 (count adjacencies)))
    (is (= 3 (count rings)))
    (is (= 2 (get rings :b)))))

(deftest d-ring-test
  (let [adjacencies (d-space-adjacencies 4)
        rings (frequencies (map first adjacencies))]
    (println adjacencies)
    (is (= 4 (count adjacencies)))
    (is (= 2 (count rings)))
    (is (= 2 (get rings :d)))))

(deftest adjacencies-test
  (let [adjacencies (generate-adjacencies (map first ring-spaces))]
    (print adjacencies)
    (is (= 6 (count (get adjacencies [:b 5]))))))
