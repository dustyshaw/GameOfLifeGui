(ns graphics
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.test :refer :all])
  )



;; DUSTYS CODE

(def LivingCellsList #{[2 3] [3 3] [4 3]})

(defn neighbors-of [[x y]]
  #{[(- x 1) (- y 1)] [x (- y 1)] [(+ x 1) (- y 1)]
    [(- x 1) y]                   [(+ x 1) y]
    [(- x 1) (+ y 1)] [x (+ y 1)] [(+ x 1) (+ y 1)]})

(defn living-neighbors [[x y] livingCells]
  (def n (neighbors-of [x y]))
  (def livingNeighbCells #{})

  (doseq [item livingCells]
    (doseq [cell n]
      (if (= cell item)
        (def livingNeighbCells (conj livingNeighbCells cell)))))
  livingNeighbCells)

(defn countLivingNeighbors [[x y] LivingCellsList]
  (count (living-neighbors [x y] LivingCellsList))
  )





(defn living-cells
  ([cell] (fn [cell] (living-neighbors [2 3] LivingCellsList)))
  ([cell livingCells] (fn [cell livingCells]
                        ((def n (neighbors-of cell))
                         (def livingNeighbCells #{})

                         (doseq [item livingCells]
                           (doseq [cell n]
                             (if (= cell item)
                               (def livingNeighbCells (conj livingNeighbCells cell)))))
                         (count livingNeighbCells)))))

(defn will-live [[x y] livingCells]
  (if (not (= (livingCells [x y]) nil))
    (cond
      (< (countLivingNeighbors [x y] livingCells) 2) false
      (> (countLivingNeighbors [x y] livingCells) 3) false
      :else true)
    (cond
      (< (countLivingNeighbors [x y] livingCells) 3) false
      (> (countLivingNeighbors [x y] livingCells) 3) false
      :else true)))

(will-live [3 3] LivingCellsList)

(defn next-generation [livingCells]
  (def allNeighbors #{})
  (def next-gen-cells #{})
  (doseq [item livingCells]
    (def allNeighbors (conj allNeighbors (neighbors-of item))))
  (doseq [nCell allNeighbors]
    (doseq [kCell nCell]
      (if (= (will-live kCell livingCells) true)
        (def next-gen-cells (conj next-gen-cells kCell))
        (def next-gen-cells (disj next-gen-cells kCell)))))
  next-gen-cells)
(next-generation #{[2 3] [3  3] [4 3]})


;; END OF DUSTYS CODE

;; --------------- TESTS -----------------
(with-test (defn neighbors-of-test [[x y]] (neighbors-of [x y])) (is (= #{[1 1] [1 0] [1 -1] [0 1] [0 -1] [-1 1] [-1 0] [-1 -1]} (neighbors-of-test [0 0]))))
(with-test (defn get-living-neighbors-test [[x y] livingCells] (living-neighbors [x y] livingCells)) (is (= #{[2 3] [4 3]} (get-living-neighbors-test [3 3] LivingCellsList))))
(with-test (defn will-live-test [[x y] livingCells] (will-live [x y] livingCells)) (is (= true (will-live [3 3] living-cells))))
(with-test (defn next-generation-test [livingCells] (next-generation livingCells)) (is (= #{[3 2] [3 3] [3 4]} (next-generation LivingCellsList))))

(defn tests [opts]
  (run-all-tests))

;; END OF TESTS

(def min-r 10)
(def square-size 10)


(defn setup []
  ; initial state
  (q/frame-rate 10)
  #{[2 3] [3 3] [4 3] [5 6] [6 6] [7 6]}
  )

(defn update [state]
  ; increase radius of the circle by 1 on each frame
  ;; (update-in state [:r] inc)
)

(defn draw [state]
  (q/background 255)
  (doseq [cell state]
    (let [xCoor (* square-size (first cell))
          yCoor (* square-size (second cell))]
      (q/rect xCoor yCoor square-size square-size)
      )
    )
  )

; decrease radius by 1 but keeping it not less than min-r
(defn shrink [r]
  (max min-r (dec r)))

;; (defn mouse-moved [state event]
;;   (-> state
;;       ; set circle position to mouse position
;;       (assoc :x (:x event) :y (:y event))
;;       ; decrease radius
;;       (update-in [:r] shrink)))


(defn draw-stuff [args]
  
  (q/defsketch example
    :size [800 800]
    :setup setup
    :draw draw
    :update next-generation
    ;; :mouse-moved mouse-moved
    :middleware [m/fun-mode])
  )