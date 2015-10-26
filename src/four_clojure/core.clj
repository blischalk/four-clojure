(ns four-clojure.core)

;; Naive implementation.  Turn to string and reverse / compare
(defn palendromic-first [start]
  (let [num-as-str (str start)
        reversed-num-as-str (clojure.string/reverse (str start))]
    (if (= num-as-str reversed-num-as-str) (lazy-seq (cons start (palendromic-first (inc start)))) (lazy-seq (palendromic-first (inc start))))))

;; Port C implementation using recursion and math
(defn palendromic-second [start]
  (letfn [(reverse-num [number]
            (loop [input number rev 0]
              (if (= input 0) rev
                (recur (quot input 10) (+ (mod input 10) (* rev 10))))))]
      (if (= start (reverse-num start)) (lazy-seq (cons start (palendromic-second (inc start)))) (lazy-seq (palendromic-second (inc start))))))

(defn reverse-number [number]
  (loop [input number rev 0]
    (if (= input 0) rev
        (recur (quot input 10) (+ (mod input 10) (* rev 10))))))

;; No reversal, compare first and last and move toward center
(defn palendromic [start]
  (letfn [(is-palendromic [number]
            (let [parsed (str number)]
              (loop [x (first parsed)
                     y (last parsed)
                     left (drop 1 (drop-last 1 parsed))]

                (cond (not= x y) false
                      (nil? x) true
                      :else (recur (first left) (last left) (drop 1 (drop-last 1 left)))))))]
    (if (is-palendromic start) (lazy-seq (cons start (palendromic (inc start)))) (lazy-seq (palendromic (inc start))))))

;; if number is single digit it is palendromic
;; if number id 2 digits and they are the same it is palendromic
;; if number is 3 digits and the first and last are the same it is palendromic
;; if number is 4 or 5 digits and the last 2 are the inverse of the first 2 it is palendromic
;; if number is 6 or 7 digits and the last 3 are the inverse of the first 3 it is palendromic
;; if number is 8 or 9 digits and the last 4 are the inverse of the first 4 it is palendromic
;; if number is 10 or 11 digits and the last 5 are the inverse of the first 5 it is palendromic


(= true 
   (apply < (take 6666 (palendromic 9999999))))

(palendromic 0)

(= (str 0) (reverse (str 0)))
(str 0)
(str (clojure.string/reverse (str 123)))

(take 20 (palendromic 0))

(time) (+ 1 2 3)

(reverse (str 123))

;; Trick Taking Problem 141
;; Which cards won trick?
;; Accept a trump suit and return a function winner
;; Winner will be called on a sequence of cards
;; and should return the one which wins the trick
;; cards represented as a hash-map of :suit and numeric :rank
;; cards with larger rank are stronger
(defn trick [trump]
  (fn [cards]
    (let [suit (if (nil? trump) (:suit (first cards)) trump)]
      (last (sort-by :rank (filter #(= (:suit %) suit) cards))))))


(defn trick-pro [trump]
  (fn [cards]
    (let [suit (if (nil? trump) (:suit (first cards)) trump)]
      (last (sort-by :rank (filter #(= (:suit %) suit) cards))))))

(let [notrump (trick-pro nil)]
  (and (= {:suit :club :rank 9} (notrump [{:suit :club :rank 4}
                                          {:suit :club :rank 9}]))
       (= {:suit :spade :rank 2} (notrump [{:suit :spade :rank 2}
                                           {:suit :club :rank 10}]))))

(= {:suit :club :rank 10} ((trick-pro :club) [{:suit :spade :rank 2}
                                       {:suit :club :rank 10}]))

(= {:suit :heart :rank 8}
   ((trick-pro :heart) [{:suit :heart :rank 6} {:suit :heart :rank 8}
                 {:suit :diamond :rank 10} {:suit :heart :rank 4}]))


(= {:suit :heart :rank 14}
   ((trick-pro :heart) [{:suit :heart :rank "A"} {:suit :heart :rank 8}
                 {:suit :diamond :rank 10} {:suit :heart :rank 4}]))

(map #(update-in % [:rank] (fn [item] (cond (= item "A") 14))) [{:rank "A"} {:rank "K"}])



; 63 - Group a Sequence
(fn [f coll]
  (reduce (fn [hm item]
            (let [key (f item)]
              (if-let [val (get hm key)
                       key (f item)]
                (assoc hm key (conj val item))
                (assoc hm key [item])))) {} coll))

;; Better Solution
(comment (fn [f coll]
            (apply merge-with
                   concat
                   (map #(assoc {} (f %) [%]) coll))))

;; Another Better Solution
(comment ((fn [f coll]
            (map #(vector (f (first %)) (vec %))
                 (partition-by f (sort coll)))) #(> 5 %) [1 2 3 4 5 6 7 8]))


;; 96
; Let us define a binary tree as "symmetric" if the left half
; of the tree is the mirror image of the right half of the tree.
; Write a predicate to determine whether or not a given binary
; tree is symmetric. (see To Tree, or not to Tree for a reminder
; on the tree representation we're using).

; flatten 1 side of tree.
; recurse other side of tree.
; invert left and right node at each level
; could possibly be optimized to pass the flattened right side
; for comparison to bail instead of completely recurring

(fn [[_ left right]]
  (= (vec (flatten ((fn inverter [items]
                      (let [n (first items)
                            l (second items)
                            r (last items)]
                        (cond (= nil l r) items
                              (= nil l) [n (inverter r) nil]
                              :othwewise [n (inverter r) (inverter l)])))
                    left)))
     (vec (flatten right))))


;; 122
; Convert a binary number, provided in the
; form of a string, to its numerical value.
; Count the amount of places in the input string
; and calculate 2^n for each place starting with 0
; Reverse and split apart the input number so that
; the number places correspond to the calculated max binary value
; Keep a running total and every time a 1 is encountered add that
; value to the running total.

(defn bi-str-to-int [input]
  (letfn [(binary-values [times]
            (loop [iter 0
                   count times
                   coll []]
              (let [val (reduce * (repeat iter 2))]
                (if (zero? count) coll
                    (recur (inc iter) (dec count) (conj coll val))))))]
    (loop [i (reverse (rest (clojure.string/split input #"")))
           vals (take (count i) (binary-values (count input)))
           total 0]
      (if (zero? (count i)) total
          (recur (rest i) (rest vals) (if (= "0" (first i)) total
                                        (+ total (first vals))))))))

(= 65535 (bi-str-to-int "1111111111111111"))
(= 9     (bi-str-to-int "1001"))


; 88
; Write a function which returns the symmetric difference of two sets.
; The symmetric difference is the set of items belonging to
; one but not both of the two sets.


(fn [a b] (clojure.set/union
           (clojure.set/difference a b)
           (clojure.set/difference b a)))

(= (my-funk #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})


;; 143

((fn [col1 col2] (reduce + #(map col1 col2))) [1 1 1] [1 1 1])

(fn [col1 col2] (apply +  (#(map * %1 %2) col1 col2)))


(= 3 ((fn [col1 col2] (apply +  (#(map * %1 %2) col1 col2))) [1 1 1] [1 1 1]))



;; 94 John Conway - Game of Life

(defn life [world]
  (vec (map-indexed (fn [row-index row]
                      (apply str (map-indexed (fn [ele-index ele]
                                                (let [row-vec (vec world)
                                                      above (get (get row-vec
                                                                      (dec row-index)) ele-index)
                                                      d-t-l (get (get row-vec
                                                                      (dec row-index)) (dec ele-index))
                                                      d-t-r (get (get row-vec
                                                                      (dec row-index)) (inc ele-index))
                                                      below (get (get row-vec
                                                                      (inc row-index)) ele-index)
                                                      d-b-l (get (get row-vec
                                                                      (inc row-index)) (dec ele-index))
                                                      d-b-r (get (get row-vec
                                                                      (inc row-index)) (inc ele-index))
                                                      left  (get (get row-vec row-index)
                                                                 (dec ele-index))
                                                      right (get (get row-vec row-index)
                                                                 (inc ele-index))]
                                                  (let [live-neighbors (get
                                                                        (frequencies
                                                                         [below above
                                                                          left right
                                                                          d-t-l d-t-r
                                                                          d-b-l d-b-r]) \#)]
                                                    (if (= ele \space)
                                                      (if (= live-neighbors 3) "#" " ")
                                                      (if (or (= live-neighbors 3)
                                                              (= live-neighbors 2)) "#" " "))))
                                                ) row))) world)))


;; 120 Sum of square of digits

(fn [coll]
    (count (filter #(< %1 (apply +
                                 (map (fn [x] (let [num (Integer. x)]
                                                (* num num)))
                                      (rest (clojure.string/split (str %1) #"")))))
                   coll)))

;; 97 Pascal's Triangle

(fn [row-idx]
  (letfn [(build-row [ptri]
            (if (= (count ptri) 1) (conj ptri [1 1])
                (conj ptri (flatten (vector 1 (vec (map #(apply + %1) (partition 2 1 (last ptri)))) 1)))))]
    (loop [ptri [[1]] curr-idx 0]
      (if (= curr-idx row-idx) (get ptri (- curr-idx 1))
          (recur (build-row ptri) (inc curr-idx))))))

;; 157 Indexing Sequences
;; Transform a sequence into a sequence of pairs containing the
;; original elements along with their index.

(fn [x] (map-indexed #(vector %2 %1) x))

;; 118 Re-implement Map
;; Map is one of the core elements of a functional programming language.
;; Given a function f and an input sequence s,
;; return a lazy sequence of (f x) for each element x in s.
(= [3 4 5 6 7]
   (__ inc [2 3 4 5 6]))


;; 144 Oscilrate
;; Construct a lazy-seq
;; Add the first argument into the seq on each iteration
;; Construct the next arg collection where the new derived
;; value is the head element in the colleciton
;; and the tail is the rest of the arg list with the
;; recently applied function moved to the back of the line
(defn funky [& args]
  (let [[ele & funks] args
        acting-funk (first funks)
        application (acting-funk ele)
        next-iteration (flatten [application (rest funks) acting-funk])]
    (cons ele (lazy-seq (apply funky next-iteration)))))

(= (take 3 (funky 3.14 int double)) [3.14 3 3.0])

(= (take 5 (funky 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7])

(= (take 12 (funky 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3])


(defn euler [x]
  (if (<= x 1) 1
      (letfn [(gcd [a b]
                (let [mx (max a b)
                      mn (min a b)
                      r (rem mx mn)]
                  (if (= 0 r) mn
                      (gcd mn r))))]
        (loop [curr x acc 0]
          (if (= curr 1) acc
              (recur (dec curr) (if (= 1 (gcd x (dec curr))) (inc acc)
                                    acc)))))))

(euler 1)

(= (euler 1) 1)

(euler 10)

(= (euler 10) (count '(1 3 7 9)) 4)


;; Problem 100
;; LCM

(defn lcm-eles [& eles]
  (letfn [(gcd [a b]
            (if (= 0 b) a
                (recur b
                       (mod a b))))
          (lcm [a b]
            (/ (* a b)
               (gcd a b)))]
         (reduce lcm eles)))



;; Problem 195
;; Parentheses... Again

(= [#{""} #{"()"} #{"()()" "(())"}] (map (fn [n] (__ n)) [0 1 2]))

;; Problem 147
;; Pascal's Trapezoid

(fn my-fnk [input]
  (let [values (partition 2 1 input)
        f (first input)
        l (last input)
        next (if (seq values)
               (conj (apply vector
                            f
                            (map #(apply +' %) values))
                     l)
               [f l])]
    (lazy-seq (cons input (my-fnk next)))))

;; Problem 146
;; Trees into tables

(fn [input]
  (into {} (for [[key1 mp] input
                 [key2 v] mp]
             (hash-map [key1 key2] v))))

;; Problem 153
;; Pairwise Disjoint Sets
#(apply distinct? (mapcat seq %))

;; Problem 56
;; Find Distinct Items
(defn my-distinct [items]
  (loop [itms (reverse items)
         out []]
    (let [f (first itms)
          r (vec (rest itms))]
      (if (nil? f)
        (reverse out)
        (if (some #(= f %) r)
          (recur r out)
          (recur r (conj out f)))))))

;; Problem 58
;; Function Composition
(defn my-comp [& fnks]
  (fn [& args]
    (apply (reduce (fn [fn1 fn2]
                     (fn [& as]
                       (fn1 (apply fn2 as))))
             fnks)
     args)))

;; Problem 54
;; Partition a Sequence
(= (__ 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
(= (__ 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
(= (__ 3 (range 8)) '((0 1 2) (3 4 5)))

(defn party [cnt data]
  (if (or (empty? data) (< (count data) cnt))
    (empty data)
    (cons (take cnt data) (party cnt (drop cnt data)))))

;; Problem 59
;; Juxtaposition

(fn [& fns]
  (fn [& eles]
    (vec (for [f fns]
           (apply f eles)))))

(= [21 6 1] ((my-juxt + max min) 2 3 5 1 6 4))

(= ["HELLO" 5] ((my-juxt #(.toUpperCase %) count) "hello"))

(= [2 6 4] ((my-juxt :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))

;; Problem 74
;; Filter Perfect Squares

(fn [num-str]
  (let [nums (clojure.string/split num-str #",")
        nums-as-ints (map read-string nums)
        perfect-sqrs (set (map #(* % %) (range 100)))
        filtered (filter perfect-sqrs nums-as-ints)]
    (apply str (interpose "," filtered))))

(= (fps "4,5,6,7,8,9") "4,9")

(= (fps "15,16,25,36,37") "16,25,36")

;; Problem 76
;; Intro to Trampoline

(= [1 3 5 7 9 11]
   (letfn
     [(foo [x y] #(bar (conj x y) y))
      (bar [x y] (if (> (last x) 10)
                   x
                   #(foo x (+ 2 y))))]
     (trampoline foo [] 1)))

;; Problem 65
;; Black Box Testing
(defn black-box [arg]
  ({{} :map #{} :set} (empty arg) (if (reversible? arg) :vector :list)))

(= :map (black-box {:a 1, :b 2}))
(= :list (black-box (range (rand-int 20))))
(= :vector (black-box [1 2 3 4 5 6]))
(= :set (black-box #{10 (rand-int 5)}))
(= [:map :set :vector :list] (map black-box [{} #{} [] ()]))

;; Problem 69
;; Merge with a function Write a function which takes a function f and
;; a variable number of maps. Your function should return a map that
;; consists of the rest of the maps conj-ed onto the first. If a key
;; occurs in more than one map, the mapping(s) from the latter
;; (left-to-right) should be combined with the mapping in the result
;; by calling (f val-in-result val-in-latter)
(some identity [{:first "item"} {:second "value"}])
(reduce)

(defn merge-with' [f & maps]
  (when (some identity maps)
    (let [merge-entry (fn [m e]
                        (let [k (key e) v (val e)]
                          (if (contains? m k)
                            (assoc m k (f (get m k) v))
                            (assoc m k v))))
          merge2 (fn [m1 m2]
                   (reduce merge-entry (or m1 {}) (seq m2)))]
      (reduce merge2 maps))))

(= (__ * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
   {:a 4, :b 6, :c 20})

(= (__ - {1 10, 2 20} {1 3, 2 10, 3 15})
   {1 7, 2 10, 3 15})

(= (__ concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
   {:a [3 4 5], :b [6 7], :c [8 9]})


(defmacro solves [f & body]
  (let [replaced (clojure.walk/postwalk-replace {'__ f} body)]
    `(and ~@replaced)))

(solves
 (fn [foo] foo)
 (= (__ 6) true)
 (= (__ 496) true)
 (= (__ 500) false))



;; Problem 80
;; Perfect Numbers
;; A number is "perfect" if the sum of its divisors equal the number
;; itself. 6 is a perfect number because 1+2+3=6. Write a function
;; which returns true for perfect numbers and false otherwise.

(defn perfect? [n]
  (= n (apply +
              (filter #(= 0 (mod n %))
                      (map inc (range (/ n 2)))))))


;; Problem 77
;; Anagram Finder Write a function which finds all the anagrams in a
;; vector of words. A word x is an anagram of word y if all the
;; letters in x can be rearranged in a different order to form y. Your
;; function should return a set of sets, where each sub-set is a group
;; of words which are anagrams of each other. Each sub-set should have
;; at least two words. Words without any anagrams should not be
;; included in the result.
(defn anagram-finder [xs]
  (let [sets (map set xs)
        freqs (frequencies sets)
        anagram-sets (filter #(>= (last %) 2) freqs)
        winning-sets (map first anagram-sets)]
    (set (for [w winning-sets]
           (set (filter #(= w (set %)) xs))))))

(solves
 anagram-finder
 (= (__ ["meat" "mat" "team" "mate" "eat"])
    #{#{"meat" "team" "mate"}}))

;; Problem 60
;; Sequence Reductions
;; Write a function which behaves like reduce, but
;; returns each intermediate value of the reduction. Your function
;; must accept either two or three arguments, and the return sequence
;; must be lazy.

(defn sequence-reduction
  ([f coll]
   (sequence-reduction f (first coll) (rest coll)))
  ([f start coll]
   (let [fst (first coll)
         result (f start fst)
         rst (rest coll)]
     (cons start (lazy-seq (if (empty? rst)
                             [result]
                             (sequence-reduction f result rst)))))))


(solves
 sequence-reduction
 (= (take 5 (__ + (range))) [0 1 3 6 10])
 (= (__ conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])

 (= (last (__ * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120))


;; Problem 102
;; intoCamelCase When working with java, you often need to create an
;; object with fieldsLikeThis, but you'd rather work with a hashmap
;; that has :keys-like-this until it's time to convert. Write a
;; function which takes lower-case hyphen-separated strings and
;; converts them to camel-case strings.

(defn camel [word]
  (let [pieces (clojure.string/split word #"-")
        f (first pieces)
        r (rest pieces)]
    (str f (if r (apply str (map clojure.string/capitalize r))))))

(camel "this-camel-case")


(solves
 camel
 (= (__ "something") "something")
 (= (__ "multi-word-key") "multiWordKey")
 (= (__ "leaveMeAlone") "leaveMeAlone"))


;; Problem 86
;; Happy Numbers Happy numbers are positive integers that follow a
;; particular formula: take each individual digit, square it, and then
;; sum the squares to get a new number. Repeat with the new number and
;; eventually, you might get to a number whose squared sum is 1. This
;; is a happy number. An unhappy number (or sad number) is one that
;; loops endlessly. Write a function that determines if a number is
;; happy or not.

(defn happy? [number]
  (let [orig-input number]
    (loop [number number counter 0]
      (let [p-num (rest (clojure.string/split (str number) #""))]
        (let [result
              (reduce + (map #(* % %) (map read-string p-num)))]
          (cond (= 1 result) true
                (> counter orig-input) false
                :otherwise (recur result (inc counter))))))))

(solves
 happy?
 (= (__ 7) true)
 (= (__ 986543210) true)
 (= (__ 2) false)
 (= (__ 3) false))


;; Problem 78
;; Reimplement Trampoline
;; Reimplement the function described in "Intro to Trampoline".

;; The trampoline function takes a function f and a variable number of
;; parameters. Trampoline calls f with any parameters that were
;; supplied. If f returns a function, trampoline calls that function
;; with no arguments. This is repeated, until the return value is not
;; a function, and then trampoline returns that non-function
;; value. This is useful for implementing mutually recursive
;; algorithms in a way that won't consume the stack.

(defn trampoline' [f & args]
  (loop [result (apply f args)]
    (if (not (fn? result))
      result
      (recur (result)))))

(solves
 trampoline'
 (= (letfn [(triple [x] #(sub-two (* 3 x)))
            (sub-two [x] #(stop?(- x 2)))
            (stop? [x] (if (> x 50) x #(triple x)))]
      (__ triple 2))
    82)
 (= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
            (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
      (map (partial __ my-even?) (range 6)))
    [true false true false true false]))

;; Problem 85
;; Power Set Write a function which generates the power set of a given
;; set. The power set of a set x is the set of all subsets of x,
;; including the empty set and x itself.
(defn power-set [xs]
  (set (map set
            (loop [[f & r] (seq xs) result '(())]
                  (if f (recur r (concat result (map #(cons f %) result)))
                      result)))))

(solves
 power-set
 (= (__ #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})
 (= (__ #{}) #{#{}})
 (= (__ #{1 2 3})
    #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})
 (= (count (__ (into #{} (range 10)))) 1024))

;; Probelm 115
;; A balanced number is one whose component digits have
;; the same sum on the left and right halves of the number. Write a
;; function which accepts an integer n, and returns true if n is
;; balanced.
(defn balanced-number [number]
  {:pre  [(integer? number)]}
  (let [as-str (str number)
        cnt (count as-str)
        items (quot cnt 2)
        l (take items as-str)
        r (take items (reverse as-str))
        sum (fn [s] (reduce + (map #(Integer. (str %)) s)))]
    (= (sum l) (sum r))))

(solves balanced-number
        (= true (__ 11))
        (= true (__ 121))
        (= false (__ 123))
        (= true (__ 0))
        (= false (__ 88099))
        (= true (__ 89098))
        (= true (__ 89089))
        (= (take 20 (filter __ (range)))
           [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101]))

;; Problem 116
;; A balanced prime is a prime number which is also the mean of the primes
;; directly before and after it in the sequence of valid primes.
;; Create a function which takes an integer n, and returns true if it is a balanced prime.


(defn is-prime? [n]
  (if (< n 2) false
      (let [max (/ n 2)
            possible (range 2 (inc max))]
        (empty? (filter #(= (mod n %) 0) possible)))))


(defn balanced-prime [n]
  (if (is-prime? n)
    (let [next (first (take 1 (filter is-prime? (iterate inc (inc n)))))
          prev (first (take 1 (filter is-prime? (iterate dec (dec n)))))
          avg (/ (+ next prev) 2)]
      (= avg n))
    false))

(balanced-prime 563)

(= false (balanced-prime 4))

(= true (balanced-prime 563)) ;; 557    563    569

(= 1103 (nth (filter balanced-prime (range 2)) 15))

(nth (filter balanced-prime (range 2 100)) 15)



















;; Color permutations of a specific length of pattern
(def colors [:red :green])

(defn allColors [n]
  ;; Colors is the full list of colors
  ;; at each level of recursion!
  (if (= n 0) '(())
      (mapcat (fn [c]
                (map #(cons c %)
                     (allColors (- n 1))))
              colors)))


(defn allColors' [n]
  (if (= n 0)
    '(())
    ;; Colors is the full list of colors
    ;; at each level of recursion!
    (for [c colors items (allColors' (dec n))]
      (cons c items))))


(allColors 1)

(allColors' 2)

((:red :red :red) (:red :red :green) (:red :green :red) (:red :green :green) (:green :red :red) (:green :red :green) (:green :green :red) (:green :green :green))

(((:red (:red (:red)) (:red (:green))) (:red (:green (:red)) (:green (:green)))) ((:green (:red (:red)) (:red (:green))) (:green (:green (:red)) (:green (:green)))))


(let [colors [:red :purple]]
  (map (fn [i] (map #(cons i %) '((:red) (:purple)))) colors))

(((:red :red) (:red :purple)) ((:purple :red) (:purple :purple)))
((:red :red) (:red :purple) (:purple :red) (:purple :purple))

(map list [:red :purple])

{4 #{2 -2}}

#_(defn my-funk [f d]
  (reduce (fn [coll i]
            ;; {}
            (if (get coll i)
              (update coll (f i) conj i)
              ())) {} d))

(= (__ #(* % %) #{-2 -1 0 1 2})
   #{#{0} #{1 -1} #{2 -2}})


;; Problem 105
;; Identify keys and values

(defn keys-and-values [input]
  (let [iter-fn (fn [col in key]
                  (if (empty? in) col
                      (let [item (first in)
                            updated (cond (keyword? item) (assoc col item [])
                                          (keyword? key) (update-in col [key] conj item))
                            next-key (if (keyword? item) item key)]
                        (recur updated (rest in) next-key))))]
    (iter-fn {} input nil)))


(defn keys-and-values-2 [input]
  (letfn [(kv-iter [coll in]
            (if (empty? in) coll
                (let [key (first in)
                      not-kw (complement keyword?)
                      rst-in (rest in)
                      values (take-while not-kw rst-in)
                      rst (drop-while not-kw rst-in)]
                  (recur (assoc coll key values) rst))))]
    (kv-iter {} input)))

;; Problem 115

(defn foobar [input]
  (let [explode (fn explode [n]
                  (if (= n 0)
                    []
                    (let [d (rem n 10)
                          r (quot n 10)]
                      (conj (explode r) d))))
        nums (explode input)
        midpoint (/ (count nums) 2)]
    (apply = (map (fn [ns]
                    (apply + (take midpoint ns)))
                  [nums (reverse nums)]))))


(fn [number]
  {:pre  [(integer? number)]}
  (let [as-str (str number)
        cnt (count as-str)
        items (quot cnt 2)
        l (take items as-str)
        r (take items (reverse as-str))
        sum (fn [s] (reduce + (map #(Integer. (str %)) s)))]
    (= (sum l) (sum r))))


;; Problem 110
;; Sequence of pronunciations


(= [[1 1] [2 1] [1 2 1 1]] (take 3 (__ [1])))
(= [3 1 2 4] (first (__ [1 1 1 4 4])))
(= [1 1 1 3 2 1 3 2 1 1] (nth (__ [1]) 6))

(fn [seed]
  (rest (iterate
         #(->> %
               (partition-by identity)
               (mapcat (juxt count first)))
         seed)))


;; Problem 158
;; Decurry

(= 10 ((__ (fn [a]
             (fn [b]
               (fn [c]
                 (fn [d]
                   (+ a b c d))))))
       1 2 3 4))

(fn decurry [input]
  (fn [& args]
    (let [f (first args)
          result (input f)
          rst (rest args)
          fnk-returned (fn? result)]
      (if fnk-returned
        (apply (decurry result) rst)
        result))))


;; Problem 93
;; Partially Flatten a Sequence

(defn almost-flatten [input-col]
  (filter (fn [item]
            (or (and (sequential? item)
                     (not (some sequential? item)))
                ((complement sequential?) item)))
          (tree-seq (fn [input]
                      (and (sequential? input)
                           (some sequential? input)))
                    seq
                    input-col)))

(solves
 almost-flatten
 (= (__ [["Do"] ["Nothing"]])
    [["Do"] ["Nothing"]])
 (= (__ [[[[:a :b]]] [[:c :d]] [:e :f]])
    [[:a :b] [:c :d] [:e :f]])
 (= (__ '((1 2)((3 4)((((5 6)))))))
    '((1 2)(3 4)(5 6))))

;; Problem 114
;; Global take-while
;; take-while is great for filtering sequences, but it limited: you can only examine a single item of the sequence at a time. What if you need to keep track of some state as you go over the sequence?
;; Write a function which accepts an integer n, a predicate p, and a sequence. It should return a lazy sequence of items in the list up to, but not including, the nth item that satisfies the predicate.

(defn global-take-while [i p s])

(solves
 global-take-while
 (= [2 3 5 7 11 13]
    (__ 4 #(= 2 (mod % 3))
        [2 3 5 7 11 13 17 19 23]))

 (= ["this" "is" "a" "sentence"]
    (__ 3 #(some #{\i} %)
        ["this" "is" "a" "sentence" "i" "wrote"]))

 (= ["this" "is"]
    (__ 1 #{"a"}
        ["this" "is" "a" "sentence" "i" "wrote"])))
