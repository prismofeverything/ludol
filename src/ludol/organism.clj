(ns ludol.organism
  (:require
   [clojure.set :as set]
   [ludol.rings :as rings]))

(def disk-types
  [:eat :move :grow])

(defn spaces-for
  [organism]
  (rings/map-cat keys (vals organism)))

(defn alive?
  [organism]
  (not
   (some
    (partial = 0)
    (map
     (comp count last)
     organism))))

(defn group-adjacencies
  [adjacent group]
  (apply set (map adjacent group)))

(defn adjacent-groups
  [adjacent groups]
  (map
   (juxt
    identity
    (partial group-adjacencies adjacent))
   groups))

(defn connected-groups
  [adjacent spaces]
  (let [groups (map vector spaces)
        adjacencies (adjacent-groups adjacent groups)
        merged-groups
        (reduce
         (fn [merged-groups [group adjacent-to]]
           (let [new-groups
                 (mapv
                  (fn [[new-group new-adjacent-to]]
                    (if (empty? (set/intersection adjacent-to (set new-group)))
                      [new-group new-adjacent-to]
                      [(concat group new-group)
                       (set/union adjacent-to new-adjacent-to)]))
                  merged-groups)]
             (if (= new-groups merged-groups)
               (conj merged-groups [group adjacent-to])
               new-groups)))
         [] adjacencies)]
    (keys merged-groups)))
