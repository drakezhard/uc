(ns uc.app
  (:refer-clojure :exclude [atom])
  (:require [freactive.core :refer [atom cursor]]
            [freactive.dom :as dom])
  (:require-macros [freactive.macros :refer [rx]]))

(enable-console-print!)

(def tau (* 2 Math/PI))

(defn trunc-100 [n]
  (/ (Math/trunc (* 100 n))
     100))

(def h (min (.-innerWidth js/window)
            (.-innerHeight js/window)))
(def c (/ h 2))
(def r (* c 0.78))
(def r_2 (/ r 2))
(def s3_2 (* r (/ (Math/sqrt 3) 2)))
(def s2_2 (* r (/ (Math/sqrt 2) 2)))

(defonce mouse-x (atom nil))
(defonce mouse-y (atom nil))

(defonce listeners (do (dom/listen! js/window "mousemove"
                                    (fn [e]
                                      (reset! mouse-x (.-clientX e))
                                      (reset! mouse-y (.-clientY e))))))

(defn l [stroke x y]
  [:svg/line {:x1 c :y1 c :x2 (+ c x) :y2 (+ c y) :stroke stroke}])

(def line (partial l "black"))
(def green-line (partial l "green"))

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

(defn arc [theta x y]
  (let [path #([:svg/path {:fill "none" :stroke "red"
                           :d (apply str (interpose " " %))}])
        angles (let [qu (quot theta (/ tau 4))
                     re (rem theta (/ tau 4))]
                 )
        flag #(if (<= % Math/PI)
               "0"
               "1")]
    ["M"
     (+ c r)
     c
     "A"
     r
     r
     0
     flag
     0
     x
     y]))

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
   ;key 
   [:svg/svg {:x (- c  s2_2 25) :y (+ c (/ s2_2 2) -32)}
    [:svg/image {:href "img/key1.svg" :x 5 :y -5  :height "7%" :width "24%"}]
    [:svg/image {:href "img/key2.svg" :x 10 :y 27  :height "7%" :width "24%"}]
    [:svg/rect {:x 1 :y 0 :height 66 :width 181 :fill-opacity 0 :stroke "red"}]]
   ;widget
   (rx (let [y (- @mouse-y c)
             x (if (> @mouse-x c)
                 (- @mouse-x c)
                 (- (- c @mouse-x)))
             theta (if (> x 0)
                     (Math/atan (/ y x))
                     (+ Math/PI (Math/atan (/ y x))))
             cos-theta (* r (Math/cos theta))
             sin-theta (* r (Math/sin theta))]
         [:svg/svg
          (green-line cos-theta sin-theta)
          [:svg/line {:x1 (+ c cos-theta)
                      :y1 c
                      :x2 (+ c cos-theta)
                      :y2 (+ c sin-theta)
                      :stroke "blue"}]
          [:svg/text {:x (/ (+ (+ c cos-theta)(+ c cos-theta))
                            2)
                      :y (/ (+ c (+ c sin-theta))
                            2)
                      :font-family  "Verdana"
                      :font-size "5%"
                      :stroke "darkblue"}
           (str (trunc-100 (/ (- sin-theta) r)))]
          [:svg/line {:x1 c :y1 c
                      :x2 (+ c cos-theta) :y2 c 
                      :stroke "red"}]
          [:svg/text {:x (/ (+ c (+ c cos-theta))
                            2)
                      :y (/ (+ c c)
                            2)
                      :font-family  "Verdana"
                      :font-size "5%"
                      :stroke "darkred"}
           (str (trunc-100 (/ cos-theta r)))]
          (point cos-theta sin-theta)]))])

(defonce root (dom/append-child! (.-body js/document) [:div#root]))

(defn init [] (dom/mount! root (svg)))
