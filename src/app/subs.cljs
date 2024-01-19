(ns app.subs
  (:require [re-frame.core :as rf]
            [emmy.env :as emmy :refer [*]]
            [emmy.matrix :as matrix]))

(def directions
  [[1 0] [0 1] [-1 0] [0 -1]])

(defn dfs [image components count root]
  (let [child-fn (fn [root]
                   (filter (and (fn [c] (every? #(<= 0 % 4) c))
                                (fn [c] (zero? (get-in @components c)))
                                (fn [c] (= (get-in image c) 1)))
                           (map (partial map + root) directions)))
        branch-fn (fn [c] (and (zero? (get-in @components c))
                               (= (get-in image c) 1)))
        walk (fn walk [c]
               (when (branch-fn c)
                 (swap! components assoc-in c count)
                 (mapcat walk (child-fn c))))]
    (walk root)))

(defn scan [image]
  (let [dimension (matrix/dimension image)
        components (atom (matrix/generate dimension dimension (constantly 0)))
        count (atom 0)]
    (doseq [i (range dimension)]
      (doseq [j (range dimension)]
        (when (and (= (get-in image [i j]) 1)
                   (zero? (get-in @components [i j])))
          (dfs image components (swap! count inc) [i j]))))
    @components))

(comment
  (let [image (rf/subscribe [:app/image])]
    (scan @image)))

(rf/reg-sub :app/todos
            (fn [db _]
              (:todos db)))

(rf/reg-sub :app/image
            (fn [db _]
              (apply matrix/by-rows (:image db))))

(rf/reg-sub :app/image-dimension
            :<- [:app/image]
            (fn [image _]
              (matrix/dimension image)))

(rf/reg-sub :app/negated-image
            :<- [:app/image]
            (fn [image _]
              (emmy/- image)))

(rf/reg-sub :app/components
            :<- [:app/image]
            (fn [image _]
              (scan image)))

(rf/reg-sub :app/component-of-pixel
            :<- [:app/components]
            (fn [components [_ row column]]
              (get-in components [row column])))

