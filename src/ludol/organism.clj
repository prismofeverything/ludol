(ns ludol.organism
  (:require
   [clojure.set :as set]
   [ludol.rings :as rings]))

(def disk-power
  {:eat :grow
   :move :eat
   :grow :move})

(defn devours?
  [aggressor target]
  (= target (disk-power aggressor)))

(defn spaces-for
  [organism]
  (keys organism))

(defn alive?
  [organism]
  (= (count (set (map :role (vals organism))))
     3))

(defn all-nodes
  [player]
  (apply merge (:organisms player)))

(defn tag-nodes
  [key tag player]
  (reduce-kv
   (fn [m node info]
     (assoc m node (assoc info key tag)))
   {} (all-nodes player)))

(defn tag-players
  [players]
  (map
   (partial tag-nodes :player)
   (range)
   players))

(defn node-conflict?
  [node other]
  (and
   (not=
    (:player other)
    (:player node))
   (devours?
    (:role other)
    (:role node))))

(defn find-conflicts
  [adjacent players]
  (let [nodes (apply merge (tag-players players))]
    (mapv
     first
     (filter
      (fn [[space node]]
        (let [opposing
              (filter
               (partial node-conflict? node)
               (map nodes (adjacent space)))]
          (> (count opposing) 0)))
      nodes))))

(defn repartition-organisms
  [adjacent removals organisms]
  (let [merged (apply merge organisms)
        seared (apply dissoc merged removals)
        groups (rings/connected-groups adjacent (keys seared))]
    (map (partial select-keys seared) groups)))

(defn resolve-conflicts
  [adjacent players]
  (let [removals (find-conflicts adjacent players)
        repartition (partial repartition-organisms adjacent removals)]
    (map
     (fn [player]
       (update player :organisms repartition))
     players)))


;; The ribosome retains interpretable molecular records of a world of primordial molecules from around 4 billion years ago.
