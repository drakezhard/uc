(ns uc.app
  (:refer-clojure :exclude [atom])
  (:require [freactive.core :refer [atom cursor]]
            [freactive.dom :as dom])
  (:require-macros [freactive.macros :refer [rx]]))

(enable-console-print!)

(def tau (* 2 Math/PI))
(def h (min (.-innerWidth js/window) (.-innerHeight js/window)))
(def c (/ h 2))
(def r (* c 0.78))
(def r_2 (/ r 2))
(def s3_2 (* r (/ (Math/sqrt 3) 2)))
(def s2_2 (* r (/ (Math/sqrt 2) 2)))

(defonce mouse-x (atom nil))
(defonce mouse-y (atom nil))

(defonce listeners
  (dom/listen! js/window "mousemove"
               (fn [e]
                 (reset! mouse-x (.-clientX e))
                 (reset! mouse-y (.-clientY e)))))

(defn trunc-100 [n]
  (/ (Math/trunc (* 100 n)) 100))

(defn l [stroke x y]
  [:svg/line {:x1 c :y1 c :x2 (+ c x) :y2 (+ c y) :stroke stroke}])

(def line (partial l "black"))

(defn point [x y]
  [:svg/circle {:cx (+ c x)
                :cy (+ c y)
                :r 5
                :fill "red"
                :stroke "cyan"}])

(defn e [s z stroke src x y]
  (let [m (/ (/ y r) (/ x r))
        f (fn [x] (+ (* m x) (* m (- c)) c))]
    [:svg/svg 
     [:svg/circle {:cx (+ c (* x s))
                   :cy (f (+ c (* x s)))
                   :r (* r z)
                   :fill "white" :stroke stroke}]
     [:svg/image {:href src
                  :x (- (+ c (* x s)) (* (* r z) 0.9))
                  :y (- (f (+ c (* x s))) (* (* r z) 0.9))
                  :height "5%" :width "5%"}]]))

(def eq (partial e 0.5 0.08 "white"))
(def eq-tau_8 (partial e 0.65 0.08 "white"))
(def eq-exp (partial e 0.92 0.08 "red"))

(defn coord [src x y]
  (let [m (/ (/ y r) (/ x r))
        f (fn [x] (+ (* m x) (* m (- c)) c))
        s 1.10
        z 0.09]
    [:svg/svg 
     [:svg/image {:href src
                  :x (- (+ c (* x s)) (* (* r z) 0.9))
                  :y (- (f (+ c (* x s))) (* (* r z) 0.9))
                  :height "7%" :width "7%"}]]))

(defn arc [pr theta x y]
  (let [radius (* r pr)
        path #([:svg/path {:fill "none" :stroke "red"
                           :d (apply str (interpose " " %))}])
        angles (let [qu (quot theta (/ tau 4))]
                 (if (= qu 0)
                   [theta]
                   (-> (reduce (fn [acc a] (conj acc (+ (last acc) a)))
                              [] (repeat qu (/ tau 4)))
                       (conj theta))))
        arc (fn [angle] ["A" radius radius 0 0 0
                         (+ c (* radius (Math/cos angle)))
                         (+ c (- (* radius (Math/sin angle))))])]
    [:svg/path {:stroke "green"
                :stroke-width "2px"
                :fill "none"
                :d (-> ["M" (+ c radius) c]
                       (into (map arc angles))
                       (flatten)
                       (->> (interpose " ")
                            (apply str)))}]))

(defn svg []
  [:svg/svg {:width h :height h}
   [:svg/circle {:cx c :cy c :r r :fill "white" :stroke "black"}]
   ;tau
   (line r 0)
   (eq "img/tau.svg"  r 0)
   (eq-exp "img/etau.svg" r 0)
   (coord "img/tau-coord.svg" r 0)
   ;tau/2
   (line (- r) 0)
   (eq "img/tau_2.svg" (- r) 0)
   (coord "img/tau_2-coord.svg" (- r) 0)
   (eq-exp "img/etau_2.svg" (- r) 0)
   ;3tau/4
   (line 0 r)
   (eq "img/3tau_4.svg"  0.0000001  r)
   (coord "img/3tau_4-coord.svg" 0.0000001  r)
   (eq-exp "img/e3tau_4.svg" 0.0000001 r)
   ;tau/4
   (line 0 (- r))
   (eq "img/tau_4.svg" 0.0000001 (- r))
   (coord "img/tau_4-coord.svg" 0.0000001 (- r))
   (eq-exp "img/etau_4.svg" 0.0000001 (- r))
   ;tau/8
   (line s2_2 (- s2_2))
   (eq-tau_8 "img/tau_8.svg" s2_2 (- s2_2))
   (coord "img/tau_8-coord.svg" s2_2 (- s2_2))
   ;tau/12
   (line s3_2 (- r_2))
   (eq "img/tau_12.svg" s3_2 (- r_2))
   (coord "img/tau_12-coord.svg" s3_2  (- r_2))
   ;tau/6
   (line r_2 (- s3_2))
   (eq "img/tau_6.svg" r_2 (- s3_2))
   (coord "img/tau_6-coord.svg" r_2  (- s3_2))
   ;tau/3
   (line (- r_2) (- s3_2))
   (eq "img/tau_3.svg" (- r_2) (- s3_2))
   (coord "img/tau_6-coord.svg" (- r_2) (- s3_2))
   ;widget
   (rx (let [x (if (> @mouse-x c)
                 (- @mouse-x c)
                 (- (- c @mouse-x)))
             y (- @mouse-y c)
             theta (cond (< x 0) (+ (/ tau 2)(Math/atan (/ (- y) x)))
                         (and (> x 0) (< (- y) 0)) (+ tau (Math/atan (/ (- y) x)))
                         :else (Math/atan (/ (- y) x)))
             cos-theta (* r (Math/cos theta))
             sin-theta (- (* r (Math/sin theta)))]
         [:svg/svg
          [:svg/text {:x (+ c (* 0.07 h))
                      :y (+ c (- (* 0.009 h)))
                      :font-family  "Verdana"
                      :font-size "5%"
                      :stroke "darkgreen"}
           (str "θ = "(trunc-100 theta))]
          [:svg/line {:x1 c
                      :y1 c
                      :x2 (+ c cos-theta)
                      :y2 (+ c sin-theta)
                      :stroke "darkgreen"
                      :stroke-width "2px"}]
          [:svg/line {:x1 (+ c cos-theta)
                      :y1 c
                      :x2 (+ c cos-theta)
                      :y2 (+ c sin-theta)
                      :stroke "blue"
                      :stroke-width "2px"}]
          [:svg/text {:x (+ c cos-theta)
                      :y (+ c (/ (- (+ c sin-theta) c)
                                 3.14))
                      :font-family  "Verdana" :font-size "5%" :stroke "darkblue"}
           (str (trunc-100 (/ (- sin-theta) r)))]
          [:svg/line {:x1 c :y1 c
                      :x2 (+ c cos-theta) :y2 c 
                      :stroke "red"
                      :stroke-width "2px"}]
          [:svg/text {:x (+ c (/ (- (+ c cos-theta) c)
                                 2.7))
                      :y (+ c (* 0.015 h))
                      :font-family  "Verdana"
                      :font-size "5%"
                      :stroke "darkred"}
           (str (trunc-100 (/ cos-theta r)))]
          (arc 0.360 theta (+ c cos-theta) (+ c sin-theta))]))
   ;key
   [:svg/svg {:x (- c  s2_2 (* 0.02 h)) :y (+ c (/ s2_2 2) (* 0.009 h))}
    [:svg/image {:href "img/key1.svg"
                 :x (* 0.01 h)
                 :y (- (* 0.01 h))
                 :height "7%" :width "24%"}]
    [:svg/image {:href "img/key2.svg"
                 :x (* 0.0175 h)
                 :y (* 0.03 h)
                 :height "7%" :width "24%"}]
    [:svg/rect {:x 1 :y 0 :height (* 0.10 h) :width (* 0.27 h) :fill-opacity 0 :stroke "red"}]]])

(defonce root (dom/append-child! (.-body js/document) [:div#root]))

(defn init [] (dom/mount! root (svg)))
