
(deffacts example_speedest

  (Service
    (id 1)
    (name Sanitary)
    (location  0.0 0.0)
    (n_members 100)
    (movement_speed 2.0)
    (prep_time 20.3)
  )

  (Service
    (id 2)
    (name Sanitary)
    (location  10.0 10.0)
    (n_members 100)
    (movement_speed 20.0)
    (prep_time 0.3)
  )

  (Emergency
    (id 7)
    (type pandemic)
    (location 9.0 8.0)
    (n_affected_people 5)
  )

)
