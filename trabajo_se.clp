
; auxiliar functions
(deffunction ceil(?value)
  (bind ?x (mod ?value 1))
  (if (> ?x 0) then
      (+ (integer ?value) 1)
    else
      (integer ?value)
  )
)

(deffunction truncate(?value ?digits)
  (bind ?exp (** 10 ?digits))
  (bind ?x (* ?value ?exp))
  (bind ?x (integer ?x))
  (/ ?x ?exp)
)

; templates
(deftemplate Service
  (slot id (type INTEGER))
  (slot name (allowed-values Sanitary Firemen Policemen))
  (multislot location (type FLOAT)); km
  (slot n_members (type INTEGER))
  (slot movement_speed (type FLOAT)); km/h
  (slot prep_time (type FLOAT)) ; h
)

(deftemplate Emergency
  (slot id (type INTEGER))
  (slot type (allowed-values natural_desaster thief homicide pandemic car_crash))
  (multislot location (type FLOAT)) ; km
  (slot n_affected_people (type INTEGER))
)

(deffacts services_facts

  (Service
    (id 1)
    (name Sanitary)
    (location  2.0 10.0)
    (n_members 100)
    (movement_speed 2.0)
    (prep_time 0.3)
  )

  (Service
    (id 2)
    (name Sanitary)
    (location  4.0 6.0)
    (n_members 100)
    (movement_speed 2.0)
    (prep_time 0.2)
  )

  (Service
    (id 3)
    (name Policemen)
    (location 8.0 1.0)
    (n_members 100)
    (movement_speed 200000.0)
    (prep_time 0.2)
  )

  (Service
    (id 4)
    (name Policemen)
    (location 10.0 10.0)
    (n_members 100)
    (movement_speed 2.0)
    (prep_time 0.5)
  )

  (Service
    (id 5)
    (name Firemen)
    (location 4.0 2.0)
    (n_members 100)
    (movement_speed 2.0)
    (prep_time 0.2)
  )

  (Service
    (id 6)
    (name Firemen)
    (location 10.0 4.0)
    (n_members 100)
    (movement_speed 2.0)
    (prep_time 0.1)
  )

  ; Emergecies

  (Emergency
    (id 7)
    (type thief)
    (location 10.0 2.0)
    (n_affected_people 100)
  )

  (Emergency
    (id 8)
    (type natural_desaster)
    (location 1.0 0.0)
    (n_affected_people 5)
  )
)

(defrule notifyExistenceService
  (Service (name ?name) (location ?loc_X ?loc_Y))
  =>
  (printout t "Service " ?name " situated at (" ?loc_X " " ?loc_Y ") ready!" crlf)
)

(defrule emergencySpotted
  ?e <- (Emergency (id ?id) (type ?type) (location ?loc_X ?loc_Y) (n_affected_people ?n_affected))
  =>
  (printout t "A emergency appeared!" crlf)
  (assert
    (choose-service ?id ?type ?n_affected ?loc_X ?loc_Y)
  )
)

; Emergency type handler

(defrule is-thief
  ?serv <- (choose-service ?id ?type ?n_affected ?x ?y)
  (test (eq ?type thief))
  =>
  (printout t "Is a thief emergency" crlf)
  ; calculate required staff: 1 member/10 people
  (bind ?staff (ceil (/ ?n_affected 10)))
  (assert
    (call Policemen ?id ?x ?y ?staff)
  )
  ; delete choose-service
  (retract ?serv)
)

(defrule is-natural_desaster
  ?serv <- (choose-service ?id ?type ?n_affected ?x ?y)
  (test (eq ?type natural_desaster))
  =>
  (printout t "Is a natural desaster emergency" crlf)
  ; calculate required staff: 1 member/10 people
  (bind ?staff (ceil (/ ?n_affected 10)))
  (assert
    (call Policemen ?id ?x ?y ?staff)
  )
  (assert
    (call Sanitary ?id ?x ?y ?staff)
  )
  (assert
    (call Firemen ?id ?x ?y ?staff)
  )
  ; delete choose-service
  (retract ?serv)
)

(defrule is-homicide
  ?serv <- (choose-service ?id ?type ?n_affected ?x ?y)
  (test (eq ?type homicide))
  =>
  (printout t "Is a homicide emergency" crlf)
  ; calculate required staff: 1 member/10 people
  (bind ?staff (ceil (/ ?n_affected 10)))
  (assert
    (call Policemen ?id ?x ?y ?staff)
  )
  (assert
    (call Sanitary ?id ?x ?y ?staff)
  )
  ; delete choose-service
  (retract ?serv)
)

(defrule is-pandemic
  ?serv <- (choose-service ?id ?type ?n_affected ?x ?y)
  (test (eq ?type pandemic))
  =>
  (printout t "Is a pandemic emergency" crlf)
  ; calculate required staff: 1 member/10 people
  (bind ?staff (ceil (/ ?n_affected 10)))
  (assert
    (call Sanitary ?id ?x ?y ?staff)
  )
  ; delete choose-service
  (retract ?serv)
)

(defrule is-car-crash
  ?serv <- (choose-service ?id ?type ?n_affected ?x ?y)
  (test (eq ?type car_crash))
  =>
  (printout t "Is a car crash emergency" crlf)
  ; calculate required staff: 1 member/10 people
  (bind ?staff (ceil (/ ?n_affected 10)))
  (assert
    (call Policemen ?id ?x ?y ?staff)
  )
  (assert
    (call Firemen ?id ?x ?y ?staff)
  )
  ; delete choose-service
  (retract ?serv)
)

; Service calls

;(defrule attend-emergency
;  ?call <- (call ?name ?type ?x ?y ?staff)
;  =>
;  ; calculate quickest
;  (bind ?min_time -1)
;  (do-for-all-facts ((?service Service)) TRUE
;    (bind ?locx (nth$ 1 (fact-slot-value ?service location)))
;    (bind ?locy (nth$ 2 (fact-slot-value ?service location)))
;    (bind ?dist (sqrt (+ (* (- ?x ?locx) (- ?x ?locx)) (* (- ?y ?locy) (- ?y ?locy)))) )
;    (bind ?mov_time (/ ?dist ?service:movement_speed))
;    (bind ?new_time (+ ?mov_time ?service:prep_time))
;    (if (and (eq ?service:name ?name) ; filter service type
;             (or (< ?min_time 0) (< ?new_time ?min_time)) ; quickest station
;             (<= ?staff ?service:n_members) ; enough staff?
;        )
;    then
;      (bind ?min_time ?new_time)
;      (bind ?sel_serv ?service)
;    )
;  )
;  ; update service
;  (bind ?minx (nth$ 1 (fact-slot-value ?sel_serv location)))
;  (bind ?miny (nth$ 2 (fact-slot-value ?sel_serv location)))
;  (bind ?prep_time (fact-slot-value ?sel_serv prep_time))
;  (bind ?movement_speed (fact-slot-value ?sel_serv movement_speed))
;  (bind ?n_members (fact-slot-value ?sel_serv n_members))
;  (bind ?id (fact-slot-value ?sel_serv id))
;  (retract ?sel_serv)
;  (assert
;    (Service
;      (id ?id)
;      (name ?name)
;      (location ?minx ?miny)
;      (n_members (- ?n_members ?staff))
;      (movement_speed ?movement_speed)
;      (prep_time ?prep_time)
;    )
;  )
;  (printout t ?staff " members from " ?name " station (" ?minx ", " ?miny ") responded emergency: " ?type " (" ?x ", " ?y ") [time = " (truncate ?min_time 4) "h ]"crlf)
;  ; delete call
;  (retract ?call)
;)


(defrule select-station
  ?call <- (call ?name ?emergency_id ?x ?y ?staff)
  =>
  (do-for-all-facts ((?service Service)) TRUE
    (if (eq ?service:name ?name)
     then
      (bind ?id ?service:id)
      (bind ?locx (nth$ 1 (fact-slot-value ?service location)))
      (bind ?locy (nth$ 2 (fact-slot-value ?service location)))
      (bind ?dist (sqrt (+ (* (- ?x ?locx) (- ?x ?locx)) (* (- ?y ?locy) (- ?y ?locy)))) )
      (bind ?mov_time (/ ?dist ?service:movement_speed))
      (bind ?time (+ ?mov_time ?service:prep_time))
      (assert (distance-station ?name ?emergency_id ?service:id ?time ?staff))
    )
  )
  (retract ?call)
)

(defrule attend-emergency
  ?ds <- (distance-station ?type ?emergency_id ?service_id ?time ?staff)
  ?service <- (Service (id ?id_service) (n_members ?n_members))
  (forall (and (distance-station ?emer_id ?serv_id ?ds_time ?s)
               (Service (id ?serv_id) (n_members ?n_mem))
          )
          (test (<= ?time ?ds_time))
  )
  (test (eq ?id_service ?service_id))
  =>
  (retract ?ds)
  ; eliminar staff del servicio
  (bind ?resto (- ?staff ?n_members)); numero de miembros restantes que hacen falta
  (bind ?new_n_members 0)
  (if (>= ?resto 0)
   then
      (bind ?new_n_members 0)
   else
      (bind ?resto 0)
      (bind ?new_n_members (- ?n_members ?staff))
  )
  (modify ?service (n_members ?new_n_members))
  ; actualizar todos los distance-station
  (do-for-all-facts ((?distance_station distance-station)) TRUE
    (bind ?tipo (nth$ 1 (fact-slot-value ?distance_station implied)) )
    (bind ?emergencia_id (nth$ 2 (fact-slot-value ?distance_station implied)) )
    (bind ?servicio_id (nth$ 3 (fact-slot-value ?distance_station implied)) )
    (bind ?tiempo (nth$ 4 (fact-slot-value ?distance_station implied)) )
    (bind ?staff (nth$ 5 (fact-slot-value ?distance_station implied)) )
    (if (and
          (eq ?tipo ?type)
          (eq ?emergencia_id ?emergency_id)
        )
     then
      (retract ?distance_station)
      (assert (distance-station ?tipo ?emergencia_id ?servicio_id ?tiempo ?resto))
    )
  )

  (printout t "Emergency [" ?emergency_id "] " ?type " station [" ?service_id "] time " ?time " -- " ?staff crlf)
)

(defrule finish-emergency-service
  ?end_service <- (end-service ?id ?staff)
  ?serv <- (Service (id ?id_serv) (name ?serv_name) (location ?loc_X ?loc_Y) (n_members ?members) (movement_speed ?speed) (prep_time ?time))
  (test (eq ?id ?id_serv))
  =>
  (retract ?serv)
  (assert
    (Service
      (id ?id)
      (name ?serv_name)
      (location ?loc_X ?loc_Y)
      (n_members (+ ?members ?staff))
      (movement_speed ?speed)
      (prep_time ?time)
    )
  )
  (retract ?end_service)
)
