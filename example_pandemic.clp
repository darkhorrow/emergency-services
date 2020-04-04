
(deffacts example_pandemic

  (Service
    (id 1)
    (name Sanitary)
    (location  0.0 0.0)
    (n_members 100)
    (movement_speed 2.0)
    (prep_time 0.3)
  )

  (Service
    (id 2)
    (name Firemen)
    (location  2.0 10.0)
    (n_members 100)
    (movement_speed 2.0)
    (prep_time 0.3)
  )

  (Service
    (id 2)
    (name Policemen)
    (location  2.0 10.0)
    (n_members 100)
    (movement_speed 2.0)
    (prep_time 0.3)
  )

  (Emergency
    (id 7)
    (type pandemic)
    (location 10.0 2.0)
    (n_affected_people 5)
  )

)
