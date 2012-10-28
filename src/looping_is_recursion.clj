(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n] 
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
    (if (zero? exp)
      1
      (helper base (dec exp)))))

(defn last-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (recur (rest a-seq)))))

(defn seq= [seq1 seq2]
  (loop [in-seq1 seq1
         in-seq2 seq2]
    (cond
     (and (empty? in-seq1) (empty? in-seq2)) true
     (or  (empty? in-seq1) (empty? in-seq2)) false
     (not (== (first in-seq1) (first in-seq2))) false
     :else (recur (rest in-seq1) (rest in-seq2)))))

(defn find-first-index [pred a-seq]
  (loop [in-seq a-seq
         index 0]
    (cond
     (empty? in-seq) nil
     (pred (first in-seq)) index
     :else (recur (rest in-seq) (inc index)))))

(defn avg [a-seq]
  (loop [sum 0
         amnt 0
         in-seq a-seq]
    (cond
     (and (empty? in-seq) (zero? amnt)) nil
     (empty? in-seq) (/ sum amnt)
     :else (recur (+ sum (first in-seq)) (inc amnt) (rest in-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [parities #{}
         in-seq a-seq]
    (if (empty? in-seq)
      parities
      (recur (toggle parities (first in-seq)) (rest in-seq)))))

(defn fast-fibo [n]
  (loop [acc 0
         n-1 1
         i n]
    (if (zero? i) 
      acc
      (recur (+ acc n-1) acc (dec i)))))

(defn cut-at-repetition [a-seq]
  (loop [seen []
         in-seq a-seq]
    (cond
     (empty? in-seq) seen
     (some #{(first in-seq)} seen) seen
     :else (recur (conj seen (first in-seq)) (rest in-seq)))))