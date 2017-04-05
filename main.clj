(ns square.v3
  (:gen-class)
  (require [clojure.string :as str]))

(defn load-heights [filename]
  (map read-string
    (str/split-lines (slurp filename))))

(defn mean-int [nums]
  (int (Math/floor (/ (reduce + nums) (count nums)))))

(defn max-width [ridx heights]
  (apply max
    (map count
      (partition-by
       #(>= % (inc ridx))
       heights))))

(def max-ridx 
  (memoize 
    (fn [heights]
      (dec (apply max heights)))))

(defn adjacent-max-widths [ridx heights]
  { :below (when (> ridx 0) #(max-width (dec ridx) heights)),
    :above (when (< ridx (max-ridx heights)) #(max-width (inc ridx) heights)) })

(defn below? [{:keys [below]} height]
  (and (some? below) (< (below) height)))

(defn above? [{:keys [above]} height]
  (and (some? above) (> (above) height)))
  
(defn bracket [ridx-low ridx-high heights]
  (let [ridx (mean-int [ridx-low ridx-high]) widths (adjacent-max-widths ridx heights) height (inc ridx)]
    (cond
      (below? widths height) (recur ridx-low ridx heights)
      (above? widths height) (recur ridx ridx-high heights)
      :else height)))

(defn max-square-size [heights]
  (bracket 0 (max-ridx heights) heights))

(defn -main [& args]
  (println ; 201
    (time  ; lein test 73 ms, lein run 95 ms
      (max-square-size
        (load-heights "resources/input.txt")))))
