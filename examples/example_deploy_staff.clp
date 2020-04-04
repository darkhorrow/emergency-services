

(deffacts example_deploy_staff

  (Service
    (id 1)
    (name Sanitary)
    (location  0.0 0.0)
    (n_members 10)
    (movement_speed 2.0)
    (prep_time 0.3)
  )

  (Service
    (id 2)
    (name Sanitary)
    (location  10.0 10.0)
    (n_members 2)
    (movement_speed 2.0)
    (prep_time 0.3)
  )

  (Emergency
    (id 7)
    (type pandemic)
    (location 9.0 8.0)
    (n_affected_people 50)
  )

)
