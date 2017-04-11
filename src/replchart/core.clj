(ns replchart.core
  (:require
    [clojure.string :as str]
    [clansi :refer [style ansi] :as clansi]))

(defn- str-buf
  ([^StringBuilder sb]
     (fn
       ([] (.toString sb))
       ([& args]
          (doseq [arg args]
            (.append sb arg)))))
  ([] (str-buf (StringBuilder.))))

(defn- digits-of
  [n]
  (map #(Integer/parseInt (str %)) (str n)))

(defn- linear-equation
  [m b]
  (fn [x] (+ (* m x) b)))

(defn- linear-scale-fn
  [[domain-start domain-end] [range-start range-end]]
  (let [domain-length (- domain-end domain-start)
        range-length (- range-end range-start)
        m (/ range-length domain-length)]
    (linear-equation m (- range-start (* m domain-start)))))

(def BAR-CHARS "▁▂▃▄▅▆▇█")

(defn mini-bar-chart
  [vs & {:keys [width separator]
         :or {width 80
              separator " "}}]
  (let [min-val (apply min vs)
        max-val (apply max vs)
        domain [min-val max-val]
        range [-0.49 (+ (count BAR-CHARS) -1 0.49)]
        linscale (linear-scale-fn domain range)
        scaled (map #(Math/round (linscale %)) vs)
        bars (interpose separator (map #(nth BAR-CHARS %) scaled))]
    (style (apply str (interpose separator (map #(nth BAR-CHARS %) scaled))) :white)))

(defn render-buffer
  "Renders a map containing [x,y] points as keys and characters
  to print at those locations as values.  {[3, 5] /*, ...}"
  [data width height]
  (let [buffer (str-buf)]
    (loop [row height
           col 0]
      (buffer (get data [col row] " "))
      (when (= col width) (buffer "\n"))
      (cond
        (and (= row 0) (= col width)) (buffer)
        (= col width) (recur (dec row) 0)
        :default (recur row (inc col))))))

(defn backing-buffer
  [bg-char width height]
  (into {}
        (map (fn [[x y]]
               [[x y] bg-char])
             (for [y (range height)
                   x (range width)]
               [x y]))))

(def UNICODES
  {:crosshairs "⌖"
   :bullet "•"
   :circle (style "●" :red :bg-white)
   :square (style "■" :red :bg-white)
   :horizontal (style "─" :black :bg-white)
   :horizontal-dashed (style "┄" :black :bg-white)
   :vertical (style "│" :black :bg-white)
   :vertical-dashed (style "┊" :black :bg-white)
   :left-tick (style "┤" :black :bg-white)
   :right-tick (style "├" :black :bg-white)
   :up-tick (style "┴" :black :bg-white)
   :down-tick (style "┬" :black :bg-white)
   :ll-corner (style "└" :black :bg-white)
   })

; Some drawing unicodes that might be helpful
; "╭ ╮ ╯ ╰ ╱ ╲ "
; "─ │ ┌ ┐ └ ┘ ├ ┤ ┬ ┴ ┼"
; "━ ┃ ┏ ┓ ┗ ┛ ┣ ┫ ┳ ┻ ╋"
(defn round10
  [v]
  (Math/pow 10
            (Math/round (+ 0.5
                           (Math/log10 v)
                           (- (Math/log10 5.5))))))

(defn- major-axis-for
  [v-range]
  (let [axis (round10 v-range)
        n-ticks (/ v-range axis)]
    (cond
      (> n-ticks 12.0) (* 10 axis)
      (< n-ticks 3.0) (* 0.1 axis)
      :default axis)))


(defn prepare-data
  [vs {:keys [width height
              x-domain y-domain x-range y-range]
       :as spec}]
  (let [xs (map first vs)
        ys (map second vs)
        lower-margin 1
        y-label-width (apply max (map (comp count digits-of) ys))
        ;_ (println "label width: " y-label-width)
        y-label-format (str "%" y-label-width
                            (if (every? integer? ys)
                              "d"
                              "f"))
        left-margin (+ 1 y-label-width)
        min-x (apply min xs)
        max-x (apply max xs)
        min-y (apply min ys)
        max-y (apply max ys)
        x-domain (or x-domain [min-x max-x])
        y-domain (or y-domain [min-y max-y])
        x-range (or x-range [0 width])
        y-range (or y-range [0 height])
        full-width (+ left-margin width 2)
        full-height (+ lower-margin height 2)
        x-scale (linear-scale-fn x-domain x-range)
        y-scale (linear-scale-fn y-domain y-range)
        x-scaled (map #(Math/round (float (x-scale %))) xs)
        y-scaled (map #(Math/round (float (y-scale %))) ys)]
    (merge spec
           {:xs x-scaled
            :ys y-scaled
            :x-domain x-domain
            :y-domain y-domain
            :x-range x-range
            :y-range y-range
            :left-margin left-margin
            :lower-margin lower-margin
            :y-label-format y-label-format
            :full-width full-width
            :full-height full-height})))

(defn y-axis
  [{:keys [y-domain y-range y-label-format left-margin lower-margin full-height]}]
  (let [[y0 y1] y-range
        [min-y max-y] y-domain
        y0 y0
        y1 (+ 2 y1)
        line (into {}
                   (for [row (range y0 y1)]
                     [[left-margin (+ lower-margin row)] (:vertical UNICODES)]))
        line (assoc line
                    [left-margin lower-margin] (:ll-corner UNICODES) ; bottom left corner
                    [0 lower-margin] (style (format y-label-format min-y) :black :bg-white)
                    [0 (dec full-height)] (style (format y-label-format max-y) :black :bg-white)
                    )]
    line))

(defn x-axis
  [{:keys [x-domain x-range left-margin lower-margin full-width]}]
  (let [[x0 x1] x-range
        [min-x max-x] x-domain
        x-label-width (count (digits-of max-x))
        line (into {}
                   (for [col (range x0 (+ 2 x1))]
                     [[(+ left-margin col) lower-margin] (:horizontal UNICODES)]))
        line (assoc line
                    [left-margin 0] (style min-x :black :bg-white)
                    [(- full-width x-label-width) 0] (style max-x :black :bg-white)
                    )]
    line))



(defn xy-points
  [{:keys [xs ys left-margin lower-margin point] :as spec}]
  (into {} (map #(vec [[(+ left-margin 1 %1) (+ lower-margin 1 %2)] point]) xs ys)))

(defn xy-chart
  "Plot a seq of [[x,y] ...] pairs."
  [vs & {:keys [width height x-domain y-domain x-range y-range point background]
         :or {width 24
              height 8
              point (:circle UNICODES)
              background (style " " :bg-white)}
         :as spec}]
  (let [spec (assoc spec :width width :height height
                    :point point :background background)
        {:keys [full-width full-height] :as spec} (prepare-data vs spec)
        xys (xy-points spec)
        background (backing-buffer background full-width full-height)
        y-axis (y-axis spec)
        x-axis (x-axis spec)
        buf (merge background x-axis y-axis xys)]
    (render-buffer buf full-width full-height)))

(defn bar
  [ch left-margin lower-margin x y]
  (into {}
        (for [v (range y)]
          [[(+ x 1 left-margin) (+ v 1 lower-margin)] ch])))

(defn bars
  [{:keys [xs ys left-margin lower-margin point] :as spec}]
  (reduce merge (map (partial bar point left-margin lower-margin)
                     (map #(+ 1 (* 2 %)) (range (count xs))) ys)))


(defn bar-chart
  "Plot a seq of [[x,y] ...] pairs, where the X's will be used as labels for the Y bars."
  [vs & {:keys [width height x-domain y-domain x-range y-range point background]
         :or {width 24
              height 8
              point (style " " :bg-blue)
              background (style " " :bg-white)}
         :as spec}]
  (let [spec (assoc spec :width width :height height
                    :point point :background background
                    :y-range [2 height])
        {:keys [full-width full-height] :as spec} (prepare-data vs spec)
        background (backing-buffer background full-width full-height)
        xys (bars spec)
        y-axis (y-axis spec)
        x-axis (x-axis spec)
        buf (merge background x-axis y-axis xys)]
    (render-buffer buf full-width full-height)))

(defn h-bar-chart
  [values & {:keys [max-width char]
             :or {max-width 60
                  char "*"}}]
  (let [min-val (apply min values)
        max-val (apply max values)
        length (- max-val min-val)
        margin (* 0.05 length)
        chart-min (- min-val margin)
        chart-max (+ max-val margin)
        bin-size (/ (- chart-max chart-min) max-width)
        label-width (apply max (map (comp count digits-of) values))
        ints? (every? #(= % (int %)) values)
        labels (for [v values]
                 (if ints?
                   (format (str "%" (inc label-width) "d") v)
                   (format (str "%" (inc label-width) "0.2f") (float v))))
        bars (for [v values]
               (apply str (repeat (Math/round (float v)) "*")))]
    (doseq [[bar label] (map vector bars labels)]
      (println (str (style (format "%s |" label) :bg-white) (style bar :blue :bg-white))))))

; TODO: figure out how to build a line chart by interpolating between points
; correctly, and then drawing those points with the xy-chart.
#_(defn line-chart
  [vs & {:keys [width height x-domain y-domain x-range y-range point]
         :or {width 30
              height 10
              point "*"}}]
  (let [vs (sort vs)]
    nil))

(defn animation
  []
  (dotimes [i 20]
    (println (apply str (repeat 200 "\n")))
    (println (mini-bar-chart (repeatedly 10 rand)))
    (Thread/sleep 150)))

; Examples
; (bar-chart [2 4 8 18 23 16 12 5 2 1])
; (mini-bar-chart (repeatedly 10 rand))

;(vijual/draw-graph [[:a [:b] [:c]]])
;(vijual/draw-tree [[:a [:b] [:c]]])

;(println (xy-chart [[3 4] [5 3] [4 3] [2 5] [6 2]] :width 60 :height 10))
