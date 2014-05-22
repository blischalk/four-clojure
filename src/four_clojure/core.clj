(ns four-clojure.core)

(defn happy-numbers [number]
  (let [orig-input number]
    (loop [number number counter 0]
      (let [p-num (rest (clojure.string/split (str number) #""))]
        (let [result 
              (reduce + (map #(* % %) (map read-string p-num)))]
          (cond (= 1 result) true
                (> counter orig-input) false
                :otherwise (recur result (inc counter))))))))

(happy-numbers 8)

