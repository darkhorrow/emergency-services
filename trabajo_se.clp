
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
    (location  10.0 10.0)
    (n_members 100)
    (movement_speed 2.0)
    (prep_time 5.0)
  )

  (Service
    (name Firemen)
    (location 2.0 5.0)
    (n_members 100)
    (movement_speed 2.0)
    (prep_time 5.0)
  )

  (Service
    (name Policemen)
    (location 1.0 4.0)
    (n_members 100)
    (movement_speed 2.0)
    (prep_time 5.0)
  )

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
  (assert
    (call-policemen ?n ?x ?y)
  )
)

(defrule is-natural_desaster
  (choose-service ?t)
  (test (eq ?t natural_desaster))
  =>
  (printout t "Is a natural desaster emergency" crlf)
  (assert
    (call-policemen ?n ?x ?y)
  )
  (assert
    (call-sanitary ?n ?x ?y)
  )
  (assert
    (call-firemen ?n ?x ?y)
  )
)

(defrule is-homicide
  (choose-service ?t ?n ?x ?y)
  (test (eq ?t homicide))
  =>
  (printout t "Is a homicide emergency" crlf)
  (assert
    (call-policemen ?n ?x ?y)
  )
  (assert
    (call-sanitary ?n ?x ?y)
  )
)

(defrule is-pandemic
  (choose-service ?t ?n ?x ?y)
  (test (eq ?t pandemic))
  =>
  (printout t "Is a pandemic emergency" crlf)
)

(defrule is-car-crash
  (choose-service ?t ?n ?x ?y)
  (test (eq ?t car_crash))
  =>
  (printout t "Is a car crash emergency" crlf)
  (assert
    (call-policemen ?n ?x ?y)
  )
  (assert
    (call-firemen ?n ?x ?y)
  )
)

; Service calls
