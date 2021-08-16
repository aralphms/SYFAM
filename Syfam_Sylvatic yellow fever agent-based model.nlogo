globals
[
  number-of-days                              ; number of simulated days (1 day = 10 time steps)
  k-min                                       ; minimum abundance of mosquitoes during the simulation
  alpha                                       ; base coefficient in the equation of adult mosquitoes carrying capacity (current-K)
  beta                                        ; trend coefficient in the equation of current-K
  gamma                                       ; seasonal coefficient in the equation of current-K
  mosquito-transmission-competence            ; probability of an infectious mosquito transmit the virus to a susceptible vertebrate host during an interaction
  monkey-transmission-competence              ; probability of an infectious monkey transmit the virus to a susceptible mosquito during an interaction
  alternative-host-transmission-competence    ; probability of an infectious other-host transmit the virus to a susceptible mosquito during an interaction
  hourly-mosquito-infection-rate              ; a list that counts and stores the infection rate in the mosquito population at each time step
  vertebrate-hosts                            ; a variable that group monkeys and other-hosts
  biting-success                              ; the probability of a blood-seeking mosquito have success (bite the host) during an interaction with a vertebrate host
  monkey-population                           ; the initial population size of monkeys
  monkey-group-size                           ; the initial number of monkeys in each group
  A                                           ; the parameter β0 of the logistic model that controls the larval density
  B                                           ; the parameter β1 of the logistic model that controls the larval density
]

breed [mosquitoes mosquito]
breed [monkeys monkey]
breed [other-hosts other-host]

patches-own
[
  patch-type                            ; if patch is forest or breeding site
  larval-development-time               ; the list that control the time elapsed for each oviposition event
  larval-viral-status                   ; the list that controls if a given oviposition event came from an infectious mosquito
  my-larval-number                      ; current number of immature individuals contained in the patch
  num-potential-infec-mosquitoes        ; current number of oviposition events from infectious mosquitoes contained in the patch
]

mosquitoes-own
[
  blood-seeking?            ; if true, the mosquito will seek a host to perform the blood meal
  engorged?                 ; if true, the mosquito is digesting the blood and maturing the eggs
  time-to-blood-digestion   ; time elapsed (time steps) since the blood meal
  gravid?                   ; if true, the mosquito is looking for breeding sites to lay its eggs
  newly-emerged?            ; if true, the mosquito is a newly emerged adult
  time-since-emergence      ; time elapsed (time steps) since the adult emergence (when > 10 the individual change the status to blood-seeking? = true)
  blood-meals               ; number of blood meals performed by the individual
  oviposition-number        ; number of ovipositions performed by the individual (0 = nulliparous, > 0 = parous)
  blood-meal-level          ; amount of blood ingested by a blood-seeking mosquito (ranges from 0 to 1)
    ;;related to YFV transmission
  susceptible?              ; if true, the mosquito is susceptible
  exposed?                  ; if true, the mosquito is infected but in latent period
  infectious?               ; if true, the mosquito is infected and infectious
  virus-incubation          ; the incubation period of the virus in mosquitoes
  initiators?               ; the mosquitoes selected to initiate the virus transmission
]

monkeys-own
[
  my-group-ID              ; the number that defines the group the monkey belongs to.
  my-territory             ; the patches that represent the monkey's group territory
  my-status                ; if the status of the monkey in its group is alpha or subordinate
  susceptible?             ; if true, the monkey is susceptible
  exposed?                 ; if true, the monkey is infected but in latent period
  infectious?              ; if true, the monkey is infected and infectious
  immune?                  ; if true, the monkey is immune
  latent-period            ; the latent period of the virus
  viremic-period           ; the time in days that individuals will be infectious and can transmit the virus to mosquitoes
]

other-hosts-own
[
  other-host-type          ; if the individual is dead-end or alternative host
  lifespan                 ; how many days each induavidual will live
  lifetime                 ; how many days each induavidual has lived
  susceptible?             ; if true, the other-host is susceptible.
  exposed?                 ; if true, the other-host is infected but in latent period
  infectious?              ; if true, the other-host is infected and infectious, as long as it is not a dead-end host.
  immune?                  ; if true, the other-host is immune
  latent-period            ; the latent period of the virus
  viremic-period           ; the time in days that individuals will be infectious and can transmit the virus to mosquitoes
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SETUP PROCEDURES

to setup
  clear-all
  if seed?                                                           ; if on, a seed can be chosen
  [
   random-seed my-seed
  ]
  create-forest-and-breeding-sites
  insert-mosquitoes
  insert-monkeys
  insert-other-hosts
  larval-density-parameters
  set number-of-days 0

  set k-min (initial-number-of-mosquitoes / 6)                        ;minimum value of current-K  (six time lower than the maximum value)
  set alpha (initial-number-of-mosquitoes + k-min) / 2
  set beta (-(initial-number-of-mosquitoes - k-min) / 1825)
  set gamma (alpha - initial-number-of-mosquitoes)

  set mosquito-transmission-competence mosquito-trans-competence
  set monkey-transmission-competence monkey-trans-competence
  set alternative-host-transmission-competence alt-host-trans-competence
  set biting-success 0.5                                             ; the probability of a blood-seeking mosquito have success (bite the host) during an interaction with a vertebrate host
  set hourly-mosquito-infection-rate []                              ; the list that stores the proportion of infected mosquitoes in each time step (after virus introduction)

  ask turtles [assign-color]
  reset-ticks
end


to create-forest-and-breeding-sites
  ask patches
  [
    set pcolor green
    set patch-type "forest"
  ]

  ask n-of (initial-number-of-mosquitoes / 12) patches with [patch-type = "forest"]  ; the number of breeding sites is proportional to initial number of mosquitoes (1:12)
  [
    set pcolor black
    set patch-type "breeding site"
    set larval-development-time []                       ; the list that stores the time of development of each larval individual in the breeding site patch
    set larval-viral-status []                           ; the list that stores information on whether the larval individual originated from an infectious mosquito.
  ]

  ask patches with [patch-type = "breeding site"] ; inital input - half of breeding site patches start with immature forms
  [
    let init.num.larval random 90
    while [length larval-development-time < init.num.larval] [set larval-development-time fput (1 + random 150) larval-development-time]
    while [length larval-viral-status < init.num.larval] [set larval-viral-status fput 0 larval-viral-status]
    set my-larval-number length larval-development-time
  ]
end


to insert-mosquitoes
  create-mosquitoes initial-number-of-mosquitoes
  [
   set shape "circle"
   set size 1
   move-to one-of patches with [ patch-type = "forest" ]
   set blood-seeking? true
   set engorged? false
   set gravid? false
   set newly-emerged? false
   set blood-meals 0
   set oviposition-number 0
   set blood-meal-level 0
   set time-to-blood-digestion -1

   set susceptible? true
   set exposed? false
   set infectious? false
   set initiators? false
  ]

  ask n-of (initial-number-of-mosquitoes * 0.25) mosquitoes with [blood-seeking?] ; initial Input - 5% gravid mosquitoes
  [
   set blood-seeking? false
   set gravid? true
  ]
  ask n-of (initial-number-of-mosquitoes * 0.25) mosquitoes with [blood-seeking?] ; initial Input - 10% newly-emerged mosquitoes
  [
   move-to one-of patches with [patch-type = "breeding site"]
   set blood-seeking? false
   set newly-emerged? true
   set time-since-emergence random 11
  ]
  ask n-of (initial-number-of-mosquitoes * 0.25) mosquitoes with [blood-seeking?] ; initial Input - 40% blood-fed mosquitoes
  [
   set blood-seeking? false
   set engorged? true
   set time-to-blood-digestion random 51
  ]
end


to insert-monkeys
  set monkey-group-size 6                                  ; the size of the groups
  create-monkeys 1                                         ; create and alocate monkeys in different patches of the grid
  create-monkeys 1 [setxy min-pxcor / 2 max-pycor / 2]
  create-monkeys 1 [setxy min-pxcor / 2 min-pycor / 2]
  create-monkeys 1 [setxy max-pxcor / 2 max-pycor / 2]
  create-monkeys 1 [setxy max-pxcor / 2 min-pycor / 2]

  ask monkeys
  [
   set size 3
   set my-group-ID who
   set my-territory patches in-radius 18 with [patch-type = "forest" or patch-type = "breeding site"]
   set my-status "alpha"
   ;ask monkeys with [my-status = "alpha"] [ask my-territory [set pcolor gray]]
   set susceptible? true
   set exposed? false
   set infectious? false
   set immune? false
  ]

  ask monkeys                                           ; the alpha monkey creates the other monkeys of the group (subordinates)
  [
    hatch monkey-group-size - 1
    [
      rt random-float 360
      fd 3
      set my-status "subordinate"
    ]
  ]
   set monkey-population count monkeys
end


to insert-other-hosts
  create-other-hosts number-of-dead-end-hosts + number-of-alternative-hosts
  [
   ask other-hosts [set other-host-type "Dead-end"]
   ask n-of number-of-alternative-hosts other-hosts [set other-host-type "Alternative"]  ; some individuals will act as alternative hosts
   set shape "square"
   set size 2
   move-to one-of patches with [ patch-type = "forest" ]
   set lifespan random other-hosts-mean-lifespan  ;; initial input for other-hosts lifespan
     ;;related to YFV transmission
   set susceptible? false
   set exposed? false
   set infectious? false
   set immune? true
  ]
  ask other-hosts with [other-host-type = "Alternative"]
    [
      set susceptible? true
      set immune? false
    ]
end

to larval-density-parameters
   let sv1 0.99                    ; probability of 0.99
   let sv2 0.5                     ; probability of 0.5
   let lv1 15                      ; maximum number of larval density in a single breeding-site patch in which the survival probability of the progeny is over 99%
   let lv2 90                      ; number of larval density in a single breeding-site patch in which the survival probability of the progeny is 50%
   let D ln (sv1 / (1 - sv1))
   let C ln (sv2 / (1 - sv2))
   set B (D - C) / (lv1 - lv2)     ; the β1 of the logistic model that controls larval density
   set A D - B * lv1               ; the β0 of the logistic model that controls larval density
end

to assign-color                     ; the colors for representing the virus circulation and individual status
    if susceptible? [ set color white ]
    if exposed? [ set color red ]
    if infectious? [ set color yellow ]
    if breed = monkeys or breed = other-hosts and immune? [set color blue + 2]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;GO PROCEDURES

to go
  tick
  set number-of-days (ticks / 10)                                             ;update the number of days (1 day = 10 time steps)

  larval-development                                                          ;update the time elapsed since oviposition and count the number of oviposition events in the patch
  check-larval-density                                                        ;check and control larval density at the patch
  check-newly-adults                                                          ;check the newly emerged mosquitoes and update time-since-emergence
  set vertebrate-hosts turtles with [breed = monkeys or breed = other-hosts]  ;update the agentset 'vertebrate-hosts'

  move-monkeys                                                                ;update alpha status for groups and move monkeys in the grid
  move-other-hosts                                                            ;move other-hosts in the grid
  digest-host-blood                                                           ;move blood-fed mosquitoes and update their status
  seek-host                                                                   ;detect target hosts and move blood-seeking mosquitoes
  looking-for-breeding-sites                                                  ;detect breeding site patches, move gravid mosquitoes, and lay eggs

  vector-host-interaction                                                     ;procedure for blood meal and YFV transmission

  extrinsic-incubation-period                                                 ;update virus incubation time and YFV status of exposed mosquitoes
  become-viremic                                                              ;update latent-period and YFV status of exposed monkeys and other-hosts
  recovery-or-die                                                             ;update viremic period and YFV status of infectious monkeys and other-hosts; check and eliminate infected monkeys from the simulation
  Check-and-eliminate-mosquito                                                ;check and eliminate mosquitoes from the simulation
  mosquito-adult-emergence                                                    ;(patches) check larval development time and curret-K and sprout newly adult mosquitoes; check larval-viral-status and update YFV status for the newly emerged mosquitoes
  check-other-hosts-lifespan                                                  ;update lifetime,check lifespan, eliminate other-hosts from the simulation; hatch new individuals
  insert-initial-infected-mosquitoes                                          ;at the chosen time step, change randomly selected mosquitoes to infectious status and start YFV transmission

    ;;after YFV emergence, the list starts to store data on the proportion of infected mosquitoes in the population
  if ticks >= start-YFV-circulation [set hourly-mosquito-infection-rate fput mosquito-infection-rate hourly-mosquito-infection-rate]

    ;;stop conditions
  if ticks > start-YFV-circulation and not any? turtles with [infectious? or exposed?] and possible-vertical-transmission-events = 0 [stop]
  if ticks = 21900 [stop]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report current-K                                                    ;updating the parameter current-K to controlling the abundance of adult mosquitoes
  let time 0.5 + (ticks / 3650)
  let cur-k  (alpha + beta * sin (2 * pi * time * (180 / pi)) + gamma * cos (2 * pi * time * (180 / pi)))  ;it simulates seasonal changes of the environment
  report cur-k
end


   ;;updating larval development time and controling larval density
to larval-development
  ask patches with [patch-type = "breeding site" and not empty? larval-development-time]
  [
   let new-values map [x -> x + 1 ] larval-development-time
   set larval-development-time new-values
   set my-larval-number length larval-development-time
  ]
end

to check-larval-density
  ask patches with [patch-type = "breeding site" and not empty? larval-development-time]
  [
   let Z exp (A + (B * length larval-development-time))   ; the odds of the logistic model
   let P (Z / (1 + Z))                                    ; the probability of success for a larval individual to reach the winged stage
      ;;a random number between 0 and 1 is generated, if greater than P
      ;;the breeding site patch randomly select a larval to be eliminated
    while [random-float 1 > P and not empty? larval-development-time]
    [
      let my-choice random length larval-development-time
      set larval-development-time remove-item my-choice larval-development-time
      set larval-viral-status remove-item my-choice larval-viral-status
      set Z exp (A + (B * length larval-development-time))
      set P (Z / (1 + Z))
    ]
  ]
end

   ;; It was assumed that it takes a day to harden the exoskeleton and for the mating of the newly emerged mosquito
   ;; After this period the mosquito can fly and search for blood-sources
to check-newly-adults
  ask mosquitoes with [newly-emerged?]
  [
   set time-since-emergence time-since-emergence + 1
   if  time-since-emergence > 10
   [
     set newly-emerged? false
     set blood-seeking? true
   ]
  ]
end


to move-monkeys
    ;; if there is no alpha individual in the group, a new individual among the subordinates will become the alpha
  ask monkeys
  [
    let my-group my-group-ID
    let monkeys-from-my-group monkeys with [my-group-ID = my-group]
    if not any? monkeys-from-my-group with [my-status = "alpha"]
   [
     set my-status "alpha"
   ]
  ]
   ;;If the individual is outside the boundaries of its territory, it returns to the nearest patch belonging to its territory.
  ask monkeys
    [
     if patch-here != my-territory
      [
       let return-my-territory min-one-of my-territory [distance myself]
       move-to return-my-territory
      ]
    ]
    ;;moving alpha and subordinate individuals
  ask monkeys with [my-status = "alpha"]
  [
   rt random-float 360
   let forward-now 0
   ifelse random-float 1 < 0.2 [set forward-now random-float 10] [set forward-now forward-now]  ;the group will move on average 20% of the time
   forward forward-now
   let my-group my-group-ID
   ask monkeys with [my-group-ID = my-group]
    [
      set heading [heading] of myself + one-of [10 0 -10]         ;the heading of subordinate individuals can vary 10 degrees in relation to alpha individual
      forward forward-now + one-of [1 0 -1]                       ;the movement of subordinate individuals can vary one patch in relation to alpha individual
    ]
  ]
end


to move-other-hosts
   ask other-hosts
  [
   rt random-float 360
   forward random-float 5
  ]
end

to digest-host-blood
  ask mosquitoes with [engorged?]
  [
   ifelse time-to-blood-digestion <= 50                       ;five days for eggs maturation (50 time steps)
    [
     rt random-float 360
     if not blood-seeking? [fd random-float one-of [0 0.5]]   ;engorged mosquitoes move less than non-engorged
     set time-to-blood-digestion time-to-blood-digestion + 1
     ]

     ;;when time-to-blood-digestion > 50
    [
     set engorged? false
     set blood-seeking? false
     set gravid? true
     set time-to-blood-digestion -1
     set blood-meal-level 0                                   ;in case of being greater than 0 and less than 0.5
    ]
   ]
end

to seek-host
  ask mosquitoes with [blood-seeking?]
  [
   let target-hosts vertebrate-hosts in-radius mosquito-detection-radius  ;detecting the potential blood meal source
      ifelse any? target-hosts
       [
        let my-target-host one-of target-hosts
        face my-target-host
        let dist-my-target [distance myself] of my-target-host
        forward random-float dist-my-target
       ]
       [                                          ;if not detected, the mosquito keeps moving and looking for blood sources
        rt random-float 360
        forward random-float 1
       ]
      ]
end

to looking-for-breeding-sites
  ask mosquitoes with [gravid?]
  [
    ifelse patch-type = "breeding site"    ;if a gravid mosquito is on a breeding site patch the eggs will be laid
    [
      lay-eggs
    ]
    [
     let target-patch patches in-radius mosquito-detection-radius with [patch-type = "breeding site"] ;detecting the breeding site patches
     let my-breeding min-one-of target-patch [distance myself]                                        ;selecting the nearest target breeding site patch
     ifelse my-breeding != nobody                                  ;if a breeding site patch is detected and chosen, move to there
      [
       move-to my-breeding
      ]
      [                                                            ;if not detected, the mosquito keeps moving and looking for breeding sites
       rt random-float 360
       forward random-float 1
      ]
     ]
   ]
end

to lay-eggs
  set gravid? false
  set blood-seeking? true
  set oviposition-number oviposition-number + 1
    ;;including the information about the new oviposition event in the lists of the patch
    ;;If the mosquito that laid eggs is in the infectious state, the value one will be put in the larval-viral-status list; otherwise, the value 0 will be put.
  ifelse infectious?
  [
   ask patch-here
   [
      let number-of-eggs random-poisson 15                         ;It was assumed that a gravid female lay an average of 30 eggs, from which half are female
      let next-larval-number length larval-development-time + number-of-eggs
      while [length larval-development-time < next-larval-number] [set larval-development-time fput 0 larval-development-time]
      while [length larval-viral-status < next-larval-number] [set larval-viral-status fput 1 larval-viral-status]
      set num-potential-infec-mosquitoes sum larval-viral-status
   ]
  ]
  [
   ask patch-here
   [
      let number-of-eggs random-poisson 15
      let next-larval-number length larval-development-time + number-of-eggs
      while [length larval-development-time < next-larval-number] [set larval-development-time fput 0 larval-development-time]
      while [length larval-viral-status < next-larval-number] [set larval-viral-status fput 0 larval-viral-status]
      set num-potential-infec-mosquitoes sum larval-viral-status
   ]
  ]
end


to vector-host-interaction
  ask mosquitoes with [blood-seeking? and susceptible?]
  [
    let my-blood-source one-of vertebrate-hosts in-radius 0.5              ;if two or more blood sources, choose one of them
    if my-blood-source != nobody and random-float 1 < biting-success       ;the interaction between the mosquito and the blood source also depends on the biting success
      [
        set engorged? true
        if time-to-blood-digestion = -1 [set time-to-blood-digestion 0]    ;start counting the time of blood digestion
        set blood-meals blood-meals + 1
        let blood-sucked random-float 1
        set blood-meal-level blood-meal-level + blood-sucked                          ;update blood meal level
        if blood-meal-level >= 0.5 [set blood-seeking? false set blood-meal-level 0]  ;if blood meal level is equal or higher than 0.5, change blood-seeking status to false

        if [breed = monkeys and infectious?] of my-blood-source and random-float 1 < monkey-transmission-competence ;;the YFV transmission depends on the transmission competence
         [
          set susceptible? false
          set exposed? true
          assign-color
         ]

        if [breed = other-hosts and infectious?] of my-blood-source and random-float 1 < alternative-host-transmission-competence
         [
          set susceptible? false
          set exposed? true
          assign-color
         ]
        ]
      ]

  ask mosquitoes with [blood-seeking? and exposed?]                     ;the mosquito is infected but still not able to transmit the virus to susceptible hosts
  [
    let my-blood-source one-of vertebrate-hosts in-radius 0.5
    if my-blood-source != nobody and random-float 1 < biting-success
      [
        set engorged? true
        if time-to-blood-digestion = -1 [set time-to-blood-digestion 0]
        set blood-meals blood-meals + 1
        let blood-sucked random-float 1
        set blood-meal-level blood-meal-level + blood-sucked
        if blood-meal-level >= 0.5 [set blood-seeking? false set blood-meal-level 0]
      ]
   ]

 ask mosquitoes with [blood-seeking? and infectious?]                     ;the mosquito is infected and able to transmit the virus to susceptible hosts
  [
    let my-blood-source one-of vertebrate-hosts in-radius 0.5
    if my-blood-source != nobody and random-float 1 < biting-success
      [
        set engorged? true
        if time-to-blood-digestion = -1 [set time-to-blood-digestion 0]
        set blood-meals blood-meals + 1
        let blood-sucked random-float 1
        set blood-meal-level blood-meal-level + blood-sucked
        if blood-meal-level >= 0.5 [set blood-seeking? false set blood-meal-level 0]

        ask my-blood-source
        [
         if susceptible? and random-float 1 < mosquito-transmission-competence
          [
           set susceptible? false
           set exposed? true
           assign-color
          ]
        ]
     ]
  ]
end

to extrinsic-incubation-period
  ask mosquitoes with [exposed?]
  [
   set virus-incubation virus-incubation + 1     ;update the time of virus incubation in exposed mosquitoes
   if virus-incubation > 120                     ;12 days
    [
     set exposed? false
     set infectious? true
     assign-color
    ]
  ]
end

to become-viremic
  ask monkeys with [exposed?]
  [
   set latent-period latent-period + 1   ;update the latent period in exposed monkeys
   if latent-period = 30                 ;3 days
    [
     set exposed? false
     set infectious? true
     assign-color
    ]
  ]

  ask other-hosts with [exposed?]
  [
   set latent-period latent-period + 1
   if latent-period = 30
    [
     set exposed? false
     set infectious? true
     assign-color
    ]
  ]
end

to recovery-or-die
  ask monkeys with [infectious?]
  [
   set viremic-period viremic-period + 0.1       ;update viremic period in monkeys
   if viremic-period > 4                         ;40 time steps
    [
     ifelse random-float 1 < 0.8 [die][set infectious? false set immune? true assign-color]  ;infectious monkeys have an 80% chance of dying.
    ]
  ]

  ask other-hosts with [infectious?]
  [
   set viremic-period viremic-period + 0.1
   if viremic-period > alt-host-viremic-period
    [
     set infectious? false
     set immune? true
     assign-color
    ]
  ]
end

to Check-and-eliminate-mosquito
  ask mosquitoes
  [
   if random-float 1 < (- ln daily-survival-rate / 10) ;this calculates the hourly mortality rate
   [
    die
   ]
  ]
end


to mosquito-adult-emergence
   ask patches with [patch-type = "breeding site" and not empty? larval-development-time]
   [
    let mosquito-abundance count mosquitoes
    if mosquito-abundance < current-K and last larval-development-time > 120   ;12 days
    [
      sprout-mosquitoes 1
       [
        set shape "circle"
        set size 1
        set blood-seeking? false
        set engorged? false
        set gravid? false
        set newly-emerged? true
        set time-since-emergence 0
        set time-to-blood-digestion -1
        set exposed? false
        set initiators? false
        ifelse last larval-viral-status = 1 and random-float 1 < vertical-infection-rate
          [
           set susceptible? false
           set infectious? true
           assign-color
          ]
          [
           set susceptible? true
           set infectious? false
           assign-color
          ]
        ]
        ;;update the patch lists
      set larval-development-time but-last larval-development-time
      set larval-viral-status but-last larval-viral-status
     ]
   ]
end


to  check-other-hosts-lifespan
  ask other-hosts
  [
    set lifetime lifetime + 0.1     ;update lifetime
    if lifetime > lifespan          ;check if the lifetime is higher than lifespan; if so, the individual will hatch a new one and die (a constant population size is assumed)
    [
      hatch 1
      [
       set lifespan random-poisson other-hosts-mean-lifespan
       set lifetime 0
       set exposed? false
       set infectious? false
        ifelse other-host-type = "Dead-end" [set susceptible? false set immune? true] [set susceptible? true set immune? false]
       set latent-period 0
       set viremic-period 0
       assign-color
      ]
      die
    ]
  ]
end

to insert-initial-infected-mosquitoes
  if ticks = start-YFV-circulation and count mosquitoes * prop-initial-infec-mosquitoes >= 1  ;the conditions to start YFV circulation
  [
    ask n-of (count mosquitoes * prop-initial-infec-mosquitoes) mosquitoes
    [
     set susceptible? false
     set exposed? false
     set infectious? true
     set initiators? true
     assign-color
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;model reports

to-report possible-vertical-transmission-events
  let infec-larval sum [num-potential-infec-mosquitoes] of patches with [patch-type = "breeding site"]
  report infec-larval
end

to-report mosquito-infection-rate
  ifelse ticks < start-YFV-circulation
  [report 0]
  [
  let mosq-infec count mosquitoes with [infectious? and not initiators?]
  let num-mosq count mosquitoes
    if num-mosq = 0 [set num-mosq 1]
  let infec-rate mosq-infec / num-mosq
  report infec-rate
  ]
end

to-report max-mosquito-infection-rate
  ifelse ticks < start-YFV-circulation
  [report 0]
  [
  let max-mosq-infec max hourly-mosquito-infection-rate
  report max-mosq-infec
  ]
end

to-report monkey-mortality
  let monk-mort 1 - (count monkeys / monkey-population)
  report monk-mort
end

to-report YFV-persistence-days
  let YFV-circ 0
  ifelse ticks < start-YFV-circulation
  [set YFV-circ YFV-circ]
  [set YFV-circ number-of-days - (start-YFV-circulation / 10)]
  report YFV-circ
end

to-report alt-hosts-infectious-individuals
  let infec-alt-hosts 0
  ifelse alt-host-trans-competence > 0
  [set infec-alt-hosts count other-hosts with [other-host-type = "Alternative" and infectious?]]
  [set infec-alt-hosts infec-alt-hosts]
  report infec-alt-hosts
end


;;;Some extra code to model reports

;to-report nuliparous-proportion
;  let nulip count mosquitoes with [oviposition-number = 0 and not newly-emerged?]
;  let tot-mosq count mosquitoes with [not newly-emerged?]
;  if tot-mosq = 0 [set tot-mosq 1]
;  let nulip-prop nulip / tot-mosq
;  report nulip-prop
;end

;to-report parous-proportion
;  let parous count mosquitoes with [oviposition-number > 0 ]
;  let tot-mosq count mosquitoes with [not newly-emerged?]
;  if tot-mosq = 0 [set tot-mosq 1]
;  let parous-prop parous / tot-mosq
;  report parous-prop
;end


;to-report blood-meal-average
;  let total-blood-meal sum [blood-meals] of mosquitoes with [not newly-emerged?]
;  let tot-mosq count mosquitoes with [not newly-emerged?]
;  if tot-mosq = 0 [set tot-mosq 1]
;  let meal-average total-blood-meal / tot-mosq
;  report meal-average
;end

;to-report mean-oviposition-events-per-breeding-site
;  ask patches with [patch-type = "breeding site"]
;  [
;   ifelse empty? larval-development-time
;    [set my-oviposition-events-number 0]
;    [set my-oviposition-events-number length larval-development-time]
;  ]
;  let mean-ovip mean [my-oviposition-events-number] of patches with [patch-type = "breeding site"]
;  report mean-ovip
;end
@#$#@#$#@
GRAPHICS-WINDOW
779
16
1191
429
-1
-1
4.0
1
10
1
1
1
0
0
0
1
-50
50
-50
50
0
0
1
time steps
30.0

BUTTON
49
17
112
50
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
123
18
186
51
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
196
18
259
51
step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
44
72
259
105
initial-number-of-mosquitoes
initial-number-of-mosquitoes
300
3000
1200.0
1
1
NIL
HORIZONTAL

SLIDER
44
155
258
188
mosquito-detection-radius
mosquito-detection-radius
0.5
10
4.0
0.5
1
NIL
HORIZONTAL

SLIDER
44
114
259
147
daily-survival-rate
daily-survival-rate
0.8
0.99
0.93
0.01
1
NIL
HORIZONTAL

MONITOR
650
18
770
63
days
number-of-days
1
1
11

MONITOR
522
185
618
230
NIL
count mosquitoes
0
1
11

SLIDER
44
198
260
231
vertical-infection-rate
vertical-infection-rate
0
1
0.1
0.005
1
NIL
HORIZONTAL

SLIDER
39
386
260
419
other-hosts-mean-lifespan
other-hosts-mean-lifespan
100
3650
1460.0
1
1
days
HORIZONTAL

SLIDER
39
345
260
378
alt-host-viremic-period
alt-host-viremic-period
1
90
12.0
1
1
days
HORIZONTAL

MONITOR
305
371
494
416
possible vertical transmission events
possible-vertical-transmission-events
0
1
11

MONITOR
420
259
523
304
infectious monkeys
count monkeys with [infectious?]
0
1
11

MONITOR
628
185
772
230
NIL
mosquito-infection-rate
3
1
11

MONITOR
621
130
772
175
NIL
max-mosquito-infection-rate
3
1
11

SLIDER
286
118
505
151
mosquito-trans-competence
mosquito-trans-competence
0
1
0.4
0.01
1
NIL
HORIZONTAL

SLIDER
286
159
505
192
monkey-trans-competence
monkey-trans-competence
0
1
0.4
0.01
1
NIL
HORIZONTAL

SLIDER
286
200
505
233
alt-host-trans-competence
alt-host-trans-competence
0
1
0.4
0.01
1
NIL
HORIZONTAL

MONITOR
426
311
523
356
monkey mortality 
monkey-mortality
3
1
11

SLIDER
40
302
261
335
number-of-alternative-hosts
number-of-alternative-hosts
0
200
30.0
1
1
NIL
HORIZONTAL

INPUTBOX
522
59
642
119
my-seed
1.0
1
0
Number

SWITCH
522
19
642
52
seed?
seed?
1
1
-1000

MONITOR
523
130
610
175
current-K
current-K
1
1
11

SLIDER
286
20
507
53
start-YFV-circulation
start-YFV-circulation
1
7300
3650.0
1
1
time steps
HORIZONTAL

SLIDER
286
66
507
99
prop-initial-infec-mosquitoes
prop-initial-infec-mosquitoes
0.005
0.05
0.02
0.005
1
NIL
HORIZONTAL

MONITOR
650
71
771
116
YFV persistence (days)
YFV-persistence-days
1
1
11

PLOT
532
245
771
420
Monkey Population
Days
Individuals
0.0
600.0
0.0
30.0
true
false
"set-plot-pen-interval 0.1" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count monkeys"
"pen-1" 0.01 1 -2674135 false "" "if ticks > 0 \n[\n plot-pen-reset\n set-plot-pen-color red plotxy (start-YFV-circulation / 10) monkey-population\n]\n"

MONITOR
278
312
421
357
infectious alternative-hosts
alt-hosts-infectious-individuals
0
1
11

MONITOR
278
259
413
304
infectious mosquitoes
count mosquitoes with [infectious?]
0
1
11

SLIDER
40
260
262
293
number-of-dead-end-hosts
number-of-dead-end-hosts
0
200
60.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

The present model aims to reproduce an epizootic event of the Yellow Fever Virus (YFV) in a forest fragment.

The circulation of the YFV in Brazil and other countries in South and Central America occurs through sylvatic cycles. The virus circulates in the forests between mosquitoes and non-human primates and sometimes infect humans who live close to or enter these areas. Most new-world non-human primates are highly susceptible to YFV infection, especially Alouatta (howler monkeys) and Callithrix (marmosets), presenting high lethality during outbreaks. Understanding what ecological factors influence monkey mortality and how long the virus can circulate in a given area after its emergence has been a challenge for researchers and health authorities. A better understanding of these issues can contribute to the preservation of primate species and strategies for preventing human cases and reducing the risk of re-urbanization of yellow fever in Brazil and neighboring countries.

Through the interaction between autonomous agents that represent mosquitoes, monkeys, dead-end hosts, alternative (hypothetical) hosts, and breeding sites, the user can observe the influence of different characteristics (input parameters) assigned to these agents on the model outputs.


## HOW IT WORKS

The model simulates classic SEIR transmission dynamics (Susceptible - Exposed - Infectious - Recovered or Removed).

The model consists of four entities: mosquitoes (circles), monkeys (triangle), other-hosts (squares), and patches (the square cells of the grid that represent the space). The 'mosquitoes' represent female individuals of the genus Haemagogus, the main vectors of YFV in Brazil. The 'monkeys' represent howler monkeys that show high lethality during YFV epizootics. In turn, 'other-hosts' represent terrestrial vertebrates whose population inhabits the forest fragment. The other-hosts agents can assume two different holes when in contact with the virus: 1) individuals are dead-end hosts, which means they are not able to replicate the vírus at sufficient levels in the bloodstream to transmit it again to mosquitoes; or 2) individuals are alternative hosts able to replicate the virus and transmit it to vector mosquitoes. Finally, the model distinguished the patches as 'forest' (green) or 'breeding sites (black).

The agents that represent mosquitoes can assume four state variables: 'newly emerged', 'blood-seeking', 'engorged', and 'gravid'. As for the status of mosquitoes concerning the virus, they can be 'susceptible', 'exposed' (the individual has been infected but is not yet able to transmit the virus to another host), or 'infectious' (when the individual can already transmit the virus to a susceptible host it feeds on). 

The mosquito movement at each time step depends on its status. For a blood-seeking individual, the movement will depend on the detection and proximity of a blood source. When the blood source is close (0.5 patches away or less), the individual will perform the interaction. When the source is detected but is not close, the individual will make movements that may be short or long in the direction of the host. If the blood source is not detected, the individual will randomly move up to one patch per time step. An engorged mosquito stays at rest or makes short movements. A mosquito in gravid status moves up to one patch per time step in a random direction; upon detecting a breeding site patch, the individual moves up to the same. A newly-emerged mosquito does not move on the grid.

The agents representing monkeys are allocated in the grid in five groups of six individuals who move through a limited territory. Each individual has a number that relates him to the group he belongs to, a list containing the patches that represent the group's territory, and the status of each monkey in the group, alpha or subordinate. The alpha individuals define the direction towards which the group will move and whether or not any movement will be performed at each time step. Each group of monkeys has a territory where they move freely. These territories are composed of a radius of 18 patches around a central patch (a total of 1009 patches). When an individual is outside the boundaries of its territory, it immediately returns to the nearest patch belonging to its territory.

Concerning YFV status, a monkey individual can be 'susceptible', 'exposed', 'infectious', or 'immune'. Infected monkeys have an 80% chance of dying (and being removed from the simulation). Monkeys move only 20% of the time, moving up to 10 patches in the time step. The movement of subordinate individuals of the group will be similar to that of the alpha individual.

The other-hosts have a state variable that defines the individual as a dead-end host or alternative host. Individuals can be 'susceptible', 'exposed', 'infectious', or 'immune' by acting as alternative hosts. Dead-end hosts are always in the 'immune' state. Before an other-host individual dies, another individual is immediately generated to keep the population size constant. Mortality due to YFV infection is considered null to these agents. Each other-host individual moves up to 5 patches in a random direction during the time step.

Except for dead-end hosts, all mobile agents have parameters that define the virus latency period and transmission competence. In addition, monkeys and alternative hosts have parameters that define the viremic period of the individuals when in an infectious state, while mosquito individuals once infectious remain in this state. 

Patches of the type 'forest' have no state variables other than their location on the grid, while 'breeding site' patches represent places in the forest where mosquitoes in the state 'gravid' can lay their eggs. The breeding-site patches have lists that indicate if there were and how many eggs were laid in the patch, the time elapsed since each oviposition event, and if an infectious mosquito performed given oviposition. There is also a mechanism that controls larval density so that when the patch receives a high number of eggs, larval mortality also increases. 

For modeling purposes, the day lasts 10 hours (10-time steps), representing the diurnal activity of howler monkeys and Haemagogus mosquitoes since both have daytime behavior and usually rest in the nighttime. The landscape is a grid of 101 x 101 patches with a bounded space, which simulates a forest fragment with approximately 102 hectares. Each patch represents an area of 100m², and the linear distance between the center of two adjacent patches represents 10m.

Transmission of the virus occurs when an infectious mosquito feeds on the blood of a susceptible vertebrate host or, conversely, when a susceptible mosquito becomes infected by feeding on the blood of an infectious vertebrate host. The blood that the mosquitoes ingest from the hosts will allow the development of the eggs, which are deposited in the breeding-site patches found by the females a few days after the blood meal. In turn, these eggs can become new adult mosquitoes in few weeks after the oviposition event, representing the time needed for the development of immature forms. The new adult mosquitoes will allow the virus to remain circulating if the individuals are already infected (vertical transmission from the gravid female to its progeny) or if they feed on the blood of infectious hosts.

The colors of individuals represent the presence of the virus in the population. Four colors are used: susceptible = white; exposed = red; infectious = yellow; immune = light blue.

The user can choose when the epizootic of YFV starts. Infected mosquitoes introduce the vírus in the simulation. 

When the virus no longer circulates in the vector and host populations, the simulation stops.


## HOW TO USE IT

The SETUP button creates individuals according to the parameter values chosen by the user.

Once the simulation has been setup, push the GO button to run the model. GO starts the simulation and runs it continuously until GO is pushed again or the virus is no longer circulating between individuals.

The STEP button allows the user to advance a single time step.

The input parameters in the model interface are the following:

* INITIAL-NUMBER-OF-MOSQUITOES (300-3000): the number of mosquitoes the simulation starts (also, the maximum carrying capacity that will be set to the parameter called current-K)

* DAILY-SURVIVAL-RATE (0.8-0.99): the daily survival probability of each individual of the mosquito population

* MOSQUITO-DETECTION-RADIUS (0.5-10): the maximum distance (in patches) that mosquitoes can detect vertebrate hosts and breeding site patches

* VERTICAL-INFECTION-RATE (0-1): the probability of a newly-emerged adult from a progeny of an infectious mosquito be infectious

* NUMBER-OF-DEAD-END-HOSTS (0-200): the number of dead-end in the grid

* NUMBER-OF-ALTERNATIVE-HOSTS (0-200): the number of the alternative hosts in the grid

* ALT-HOST-VIREMIC-PERIOD (1-90): the viremic period of alternative hosts (days) 

* OTHER-HOST-MEAN-LIFESPAN (100-3650): the mean lifespan of alternative and dead-end hosts (days)

* START-YFV-CIRCULATION (1-7300): the number of time steps elapsed before the YFV transmission starts

* PROP-INITIAL-INFEC-MOSQUITOES (0.005-0.05): the proportion of susceptible mosquitoes that are randomly chosen to change their status to infectious so that the virus starts to circulate

* MOSQUITO-TRANS-COMPETENCE (0-1): the probability of an infectious mosquito transmits the virus to a susceptible vertebrate host during an interaction

* MONKEY-TRANS-COMPETENCE (0-1): the probability of an infectious monkey transmit the virus to a susceptible mosquito during an interaction

* ALT-HOST-TRANS-COMPETENCE (0-1): the probability of an infectious alternative host transmit the virus to a susceptible mosquito during an interaction

* SEED? (ON-OFF): if on, the user can choose a seed (for model reproducibility purposes)

* MY-SEED: to enter the seed number


A series of monitors in the interface show values of model reports and outputs: 

* DAYS - days elapsed since the start of the simulation

* YFV persistence (days) - the time elapsed since YFV emergence

* CURRENT-K - parameter that controls the carrying capacity of mosquito population

* MAX-MOSQUITO-INFECTION-RATE - the maximum proportion of infected mosquitoes during the simulation

* COUNT MOSQUITOES - current number of adult mosquitoes in the simulation

* MOSQUITO-INFECTION-RATE - the current proportion of infected mosquitoes in the population

* INFECTIOUS MONKEY - number of infectious monkeys

* INFECTIOUS ALTERNATIVE HOSTS - number of infectious alternative hosts

* MONKEY MORTALITY - the proportion of dead monkeys in the population

* INFECTIOUS MOSQUITOES - number of infectious individuals 

* POSSIBLE VERTICAL TRANSMISSION EVENTS - the number of larval individuals originated from an infectious mosquito. The future adult mosquitoes will have a given probability of emerging in the infectious state (determined by the vertical-Infection-rate parameter) 


The graphic MONKEY POPULATION shows the size of the monkey population, which tends to decrease after the YFV introduction.


## THINGS TO NOTICE

Throughout the simulation, the mosquito population regularly decreases and increases to simulate the seasonal variations that affect the abundance of mosquitoes during the year. A parameter called 'current-K', representing the carrying capacity at different moments, controls this mechanism.

The model code also includes a logistic function that controls the larval density in each breeding site patch. This function reduces the survival probability of the progeny as more oviposition events are performed in the same patch. This control mechanism can be visualized for each patch in the larval-development-time list and the variable my-oviposition-events-number.

The number of breeding site patches in the grid is proportional to the initial number of mosquitoes, being considered a breeding site patch for every 12 mosquitoes (which can be modified by the user in the model code).

 
## THINGS TO TRY

Given the natural stochasticity of the agent-based models, it is recommended that several repetitions be run for each set of input parameters to obtain average and variance measures.  Alternatively, the user can define a seed and see how the values assigned to a given parameter change the outputs or how different seeds for the same set of parameters also change the outputs.

Try to find a set of parameters that will allow YFV to circulate indefinitely in the environment. Is this possible? (for this purpose, it is recommended to change the stop condition disabling the maximum simulation time in the model code).


## EXTENDING THE MODEL

The model can be modified to investigate many other aspects related to the transmission and spreading dynamics of YFV. Examples are: 

1) include humans and other vectors in the transmission dynamics, simulating an interface between sylvatic and anthropic environment to investigate the effect of YFV vaccination coverage and risk of re-urbanization; 

2) modify the grid to simulate a fragmented environment and investigate factors that influence the spread of the virus from one fragment to another; 

3) Include population dynamics for monkeys.


## NETLOGO FEATURES

Several processes of the model are simulated in order to represent the natural variability of the events. One of the main stochastic events present in the model is the movement of agents, assumed to be random most of the time.

Other stochastic events present in the model are: 1) Random initial allocation of mosquitoes, other-hosts, and breeding site patches in the grid; 2) A female with the status 'blood-seeking' when detecting more than one host at the same time will randomly select one of them to interact with; 3) The introduction of the virus into the environment occurs after a random selection of some mosquitoes, changing their status from 'susceptible' to 'infectious'; 

Some of the modeled stochastic processes are obtained by numbers generated from a uniform random distribution between 0 and 1, that when below or above a specific input parameter or a reference value, determines which event will occur. At each time step, the following events occur according to that rule: 1) if the mosquito will or not die; 2) whether or not there will be an interaction between a given 'blood-seeking' mosquito and a selected vertebrate host; 3) the amount of blood ingested by a mosquito during the interaction with the host; 4) whether or not the virus will be transmitted through an interaction between a 'susceptible' agent and an 'infectious' agent; 5) whether the breeding site patch will eliminate one or more immature individuals in a given time step; 6) whether a monkey with 'infectious' status will die or become immune to the virus after the viremic period; 7) if a newly emerged mosquito from a progeny of an infected female will have the status 'infectious' or 'susceptible'.

When asked to eliminate an immature, the breeding site patch chooses the individual based on a number generated by uniform random distribution, the range of possible values defined according to the number of immatures in the patch. 

Finally, the lifespan of alternative and dead-end hosts are randomly assigned according to a mean value of a Poisson distribution (the user defines the average value).


## RELATED MODELS

epiDEM Basic, epiDEM trave and control, and Virus are related models.

## CREDITS AND REFERENCES

In case this model or the NetLogo software is mentioned in a publication, please include the citations below.

For the model itself:

* Medeiros-Sousa, Antônio Ralph (2021). SYFAM - Sylvatic Yellow Fever Agent-based Model. School of Public Health, University of São Paulo.

Contact: aralphms@usp.br

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Factorial experiment" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>number-of-days</metric>
    <metric>max-mosquito-infection-rate</metric>
    <metric>monkey-mortality</metric>
    <metric>count monkeys</metric>
    <enumeratedValueSet variable="initial-number-of-mosquitoes">
      <value value="1200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-host-trans-competence">
      <value value="0.125"/>
      <value value="0.375"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-hosts-viremic-time">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="biting-success">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-reservoirs">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="my_status_simulation">
      <value value="&quot;&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="partial-immunity?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-hosts-immunity-period">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mosquito-trans-competence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-breeding-sites">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vertical-infection-rate">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-host-mean-lifespan">
      <value value="1460"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily-survival-rate">
      <value value="0.93"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="monkey-trans-competence">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mosquito-detection-radius">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="parous_nuliparous" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="21900"/>
    <metric>count mosquitoes</metric>
    <metric>nuliparous-proportion</metric>
    <metric>parous-proportion</metric>
    <enumeratedValueSet variable="number-of-dead-end-hosts">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-of-mosquitoes">
      <value value="1200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-initial-infec-mosquitoes">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-hosts-viremic-period">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-YFV-circulation">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alt-host-trans-competence">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-alternative-hosts">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mosquito-trans-competence">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="my-seed">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily-survival-rate">
      <value value="0.92"/>
      <value value="0.93"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-host-mean-lifespan">
      <value value="1460"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vertical-infection-rate">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="monkey-trans-competence">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="mosquito-detection-radius" first="2" step="0.5" last="5"/>
  </experiment>
  <experiment name="Local sensitivity experiment" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>number-of-days</metric>
    <metric>max-mosquito-infection-rate</metric>
    <metric>monkey-mortality</metric>
    <metric>count monkeys</metric>
    <enumeratedValueSet variable="number-of-dead-end-hosts">
      <value value="30"/>
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-of-mosquitoes">
      <value value="1200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prop-initial-infec-mosquitoes">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alt-host-viremic-period">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="start-YFV-circulation">
      <value value="3650"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alt-host-trans-competence">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-alternative-hosts">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="seed?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mosquito-trans-competence">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="my-seed">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="daily-survival-rate">
      <value value="0.93"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="other-hosts-mean-lifespan">
      <value value="1460"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vertical-infection-rate">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="monkey-trans-competence">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mosquito-detection-radius">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
