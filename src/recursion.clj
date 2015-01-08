(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (empty? (rest coll)) (not (empty? coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (let [the-rest (rest coll)]
    (if (empty? the-rest)
      (first coll)
      (my-last the-rest)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [max-rest (max-element (rest a-seq))
          top      (first a-seq)]
      (if (nil? max-rest)
        top
        (max top max-rest)))))

(defn seq-max [seq-1 seq-2]
  (let [len1 (count seq-1)
        len2 (count seq-2)]
  (if (> len1 len2)
    seq-1
    seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (let [longest-rest (longest-sequence (rest a-seq))
          top          (first a-seq)]
      (if (nil? longest-rest)
        top
        (seq-max top longest-rest)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    []
    (let [top           (first a-seq)
          filtered-rest (my-filter pred? (rest a-seq))]
      (if (pred? top)
        (cons top filtered-rest)
        filtered-rest))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    []
    (let [top (first a-seq)]
    (if (pred? top)
      (cons top (my-take-while pred? (rest a-seq)))
      []))))

(defn my-drop-while [pred? a-seq]
    (if (empty? a-seq)
      []
      (if (pred? (first a-seq))
        (my-drop-while pred? (rest a-seq))
        a-seq)))

(defn seq= [a-seq b-seq]
  (let [af (first a-seq)
        bf (first b-seq)
        ar (rest a-seq)
        br (rest b-seq)]
  (if (= af bf)
    (if (empty? ar)
      (empty? br)
      (and (not (empty? br)) (seq= ar br)))
    false)))

(defn my-map [f a-seq b-seq]
  (if (or (empty? a-seq) (empty? b-seq))
    []
    (let [af (first a-seq)
          bf (first b-seq)
          ar (rest a-seq)
          br (rest b-seq)]
    (cons (f af bf) (my-map f ar br)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    []
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    []
    (let [one-less (- up-to 1)]
    (cons one-less (my-range one-less)))))


(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (my-map concat (rest (reverse (tails a-seq))) (rest (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [current-elem (first a-seq)
          current-freq (if (contains? freqs current-elem)
                          (get freqs current-elem)
                          0)]
      (my-frequencies-helper (assoc freqs current-elem (+ current-freq 1)) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (let [[sym freq] (first a-map)]
      (concat (my-repeat freq sym) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll ) (= n 0))
    []
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll ) (= n 0))
    coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
 (let [size      (count a-seq)
       half-size (quot size 2)
       fst       (my-take half-size a-seq)
       snd       (my-drop half-size a-seq)]
  [fst snd]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (let [a-head (first a-seq)
                b-head (first b-seq)]
          (if (< a-head b-head)
            (cons a-head (seq-merge (rest a-seq) b-seq))
            (cons b-head (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [[fst snd] (halve a-seq)]
    (seq-merge (merge-sort fst) (merge-sort snd)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

