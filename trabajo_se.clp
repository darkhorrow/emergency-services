
; auxiliar functions
(deffunction ceil(?value)
  (bind ?x (mod ?value 1))
  (if (> ?x 0) then
      (+ (integer ?value) 1)
    else
      (integer ?value)
  )
)

; templates
(deftemplate Service
  (slot name (allowed-values Sanitary Firemen Policemen))
  (multislot location (type FLOAT))
  (slot n_members (type INTEGER))
  (slot movement_speed (type FLOAT))
  (slot prep_time (type FLOAT))
)

(deftemplate Emergency
  (slot type (allowed-values natural_desaster thief homicide pandemic car_crash))
  (multislot location (type FLOAT))
  (slot n_affected_people (type INTEGER))
)

(deffacts services_facts

  (Service
    (name Sanitary)
    (location  2.0 10.0)
    (n_members 100)
    (movement_speed 2.0)
    (prep_time 5.0)
  )

  (Service
    (name Sanitary)
    (location  4.0 6.0)
    (n_members 100)
    (movement_speed 2.0)
    (prep_time 5.0)
  )

  (Service
    (name Policemen)
    (location 8.0 1.0)
    (n_members 100)
    (movement_speed 2.0)
    (prep_time 5.0)
  )

  (Service
    (name Policemen)
    (location 10.0 10.0)
    (n_members 100)
    (movement_speed 2.0)
    (prep_time 5.0)
  )

  (Service
    (name Firemen)
    (location 4.0 2.0)
    (n_members 100)
    (movement_speed 2.0)
    (prep_time 5.0)
  )

  (Service
    (name Firemen)
    (location 10.0 4.0)
    (n_members 100)
    (movement_speed 2.0)
    (prep_time 5.0)
  )

  ; Emergecies

  (Emergency
    (type thief)
    (location 10.0 2.0)
    (n_affected_people 100)
  )
)

(defrule notifyExistenceService
  (Service (name ?n) (location ?loc_X ?loc_Y))
  =>
  (printout t "Service " ?n " situated at (" ?loc_X " " ?loc_Y ") ready!" crlf)
)

(defrule emergencySpotted
  ?e <- (Emergency (type ?t) (location ?loc_X ?loc_Y) (n_affected_people ?n))
  =>
  (printout t "A emergency appeared!" crlf)
  (assert
    (choose-service ?t ?n ?loc_X ?loc_Y)
  )
)

; Emergency type handler

(defrule is-thief
  (choose-service ?t ?n ?x ?y)
  (test (eq ?t thief))
  =>
  (printout t "Is a thief emergency" crlf)
  ; calculate required staff: 1 member/10 people
  (bind ?staff (ceil (/ ?n 10)))
  (assert
    (call-policemen ?n ?x ?y ?staff)
  )
)

(defrule is-natural_desaster
  (choose-service ?t ?n ?x ?y)
  (test (eq ?t natural_desaster))
  =>
  (printout t "Is a natural desaster emergency" crlf)
  ; calculate required staff: 1 member/10 people
  (bind ?staff (ceil (/ ?n 10)))
  (assert
    (call-policemen ?n ?x ?y ?staff)
  )
  (assert
    (call-sanitary ?n ?x ?y ?staff)
  )
  (assert
    (call-firemen ?n ?x ?y ?staff)
  )
)

(defrule is-homicide
  (choose-service ?t ?n ?x ?y)
  (test (eq ?t homicide))
  =>
  (printout t "Is a homicide emergency" crlf)
  ; calculate required staff: 1 member/10 people
  (bind ?staff (ceil (/ ?n 10)))
  (assert
    (call-policemen ?n ?x ?y ?staff)
  )
  (assert
    (call-sanitary ?n ?x ?y ?staff)
  )
)

(defrule is-pandemic
  (choose-service ?t ?n ?x ?y)
  (test (eq ?t pandemic))
  =>
  (printout t "Is a pandemic emergency" crlf)
  ; calculate required staff: 1 member/10 people
  (bind ?staff (ceil (/ ?n 10)))
  (assert
    (call-sanitary ?n ?x ?y ?staff)
  )
)

(defrule is-car-crash
  (choose-service ?t ?n ?x ?y)
  (test (eq ?t car_crash))
  =>
  (printout t "Is a car crash emergency" crlf)
  ; calculate required staff: 1 member/10 people
  (bind ?staff (ceil (/ ?n 10)))
  (assert
    (call-policemen ?n ?x ?y ?staff)
  )
  (assert
    (call-firemen ?n ?x ?y ?staff)
  )
)

; Service calls

(defrule policemen-station-service
  (call-policemen ?n ?x ?y ?staff)
  =>

  ; calculate nearest station
  (bind ?dist -1)
  (bind ?minx -1)
  (bind ?miny -1)
  (do-for-all-facts ((?service Service)) TRUE
    (bind ?locx (nth$ 1 (fact-slot-value ?service location)))
    (bind ?locy (nth$ 2 (fact-slot-value ?service location)))
    (bind ?new_dist (sqrt (+ (* (- ?x ?locx) (- ?x ?locx)) (* (- ?y ?locy) (- ?y ?locy)))) )
    (if (or (< ?dist 0) (< ?new_dist ?dist)) then
      (bind ?dist ?new_dist)
      (bind ?minx ?locx)
      (bind ?miny ?locy)
    )
  )
  (printout t ?dist " x = " ?minx " y = " ?miny crlf)
)
