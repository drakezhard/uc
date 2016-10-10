(ns uc.app
  (:require [sablono.core :as html :refer-macros [html]]
   [sablono.server :refer [render-static]]))

(enable-console-print!)

(def h (min (.-innerWidth js/window)
            (.-innerHeight js/window)))
(def c (/ h 2))
(def r (* c 0.78))
(def r_2 (/ r 2))
(def s3_2 (* r (/ (Math/sqrt 3) 2)))
(def s2_2 (* r (/ (Math/sqrt 2) 2)))

(defn line [x y]
  [:line {:x1 c :y1 c :x2 (+ c x) :y2 (+ c y) :stroke "black"}])

(defn neg [n]
  (* -1 n))

(defn e [s stroke src x y]
  [:svg
   [:circle {:cx (+ c x 16) :cy (+ c y 16) :r (* r s) :fill "white" :stroke stroke}]
   [:image {:href src :x (+ c x)  :y (+ c y) :height "5%" :width "5%"}]])

(def exp-eq (partial e 0.08 "red"))

(defn e [s z src x y]
  (let [m (/ (/ y r) (/ x r))
        f (fn [x] (+ (* m x) (* m (neg c)) c))]
    [:svg 
     [:circle {:cx (+ c (* x s))
               :cy (f (+ c (* x s)))
               :r (* r z)
               :fill "white" :stroke "white"}]
     [:image {:href src
              :x (- (+ c (* x s)) (* r z))
              :y (- (f (+ c (* x s))) (* r z))
              :height "5%" :width "5%"}]]))

(def eq (partial e 0.5 0.08))
(def eq-tau_8 (partial e 0.65 0.08))

(defn coord [src x y]
  [:image {:href src :x (+ c x)  :y (+ c y) :height "5%" :width "10%"}])

(def svg 
  [:svg {:width h :height h}
   [:circle {:cx c :cy c :r r :fill "white" :stroke "black"}]
   (line r 0)
   (eq "img/tau.svg"  r 0)
   (exp-eq "img/etau.svg"  (* r 0.855) -16)
   (coord "img/tau-coord.svg" r -16)
   (line (neg r) 0)
   (eq "img/tau_2.svg" (neg r) 0)
   (coord "img/tau_2-coord.svg" (- (neg r) 69) -16)
   (exp-eq "img/etau_2.svg" (- (neg (* r 0.855)) 32) -16)
   (line 0 r)
   (eq "img/3tau_4.svg"  0.0000001  r)
   (coord "img/3tau_4-coord.svg" -35  r)
   (exp-eq "img/e3tau_4.svg" -16 (* r 0.855) )
   (line 0 (neg r))
   (eq "img/tau_4.svg" 0.0000001 (neg r))
   (coord "img/tau_4-coord.svg" -35  (- (neg r) 32))
   (exp-eq "img/etau_4.svg" -16 (- (neg (* r 0.855)) 32))
   (line s2_2 (neg s2_2))
   (eq-tau_8 "img/tau_8.svg" s2_2 (neg s2_2))
   (coord "img/tau_8-coord.svg" s2_2  (- (neg s2_2) 31))
   (exp-eq "img/etau_8.svg" -16 (- (neg (* r 0.855)) 32))
   (line s3_2 (neg r_2))
   (eq "img/tau_12.svg" s3_2 (neg r_2))
   (coord "img/tau_12-coord.svg" (- s3_2 3)  (- (neg r_2) 31))
   (line r_2 (neg  s3_2))
   (eq "img/tau_6.svg" r_2 (neg s3_2))
   (coord "img/tau_6-coord.svg" (- r_2 2)  (- (neg s3_2) 31))
   (line (neg r_2) (neg s3_2))
   (eq "img/tau_3.svg" (neg r_2) (neg s3_2))
   (coord "img/tau_6-coord.svg" (- (neg r_2) 62)  (- (neg s3_2) 31))

   [:svg {:x (- c  s2_2 5) :y (+ c (/ s2_2 2))}
    [:image {:href "img/key1.svg" :x 5 :y -5  :height "7%" :width "24%"}]
    [:image {:href "img/key2.svg" :x 10 :y 27  :height "7%" :width "24%"}]
    [:rect {:x 1 :y 0 :height 66 :width 181 :fill-opacity 0 :stroke "red"}]]])

(def unit-circle (render-static (html svg)))

(defn init []
  (let [c (.. js/document (createElement "DIV"))]
    (aset c "innerHTML" unit-circle)
    (.. js/document (getElementById "container") (appendChild c))))
