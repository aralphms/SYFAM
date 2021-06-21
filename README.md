# Current SYFAM Code

globals
[
  number-of-days                              ; number of simulated days (1 day = 10 time steps)
  k-min                                       ; minimum abundance of mosquitoes during the simulation
  alpha                                       ; base coefficient in the equation of adult mosquitoes carrying capacity (current-K)
  beta                                        ; trend coefficient in the equation of current-K
  gamma                                       ; seasonal coefficient in the equation of current-K
  mosquito-transmission-competence            ; probability of a infectious mosquito transmit the virus to a susceptible vertebrate host during an interaction
  monkey-transmission-competence              ; probability of a infectious monkey transmit the virus to a susceptible mosquito during an interaction
  alternative-host-transmission-competence    ; probability of a infectious other-host transmit the virus to a susceptible mosquito during an interaction
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
  engorged?                ; if true, the mosquito is digesting the blood and maturing the eggs
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

  ask n-of (initial-number-of-mosquitoes / 24) patches with [patch-type = "breeding site"] ; inital input - half of breeding site patches start with immature forms
  [
    let init.num.larval 1 + random 50
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

  ask n-of (initial-number-of-mosquitoes * 0.05) mosquitoes with [blood-seeking?] ; initial Input - 5% gravid mosquitoes
  [
   set blood-seeking? false
   set gravid? true
  ]
  ask n-of (initial-number-of-mosquitoes * 0.1) mosquitoes with [blood-seeking?] ; initial Input - 10% newly-emerged mosquitoes
  [
   move-to one-of patches with [patch-type = "breeding site"]
   set blood-seeking? false
   set newly-emerged? true
   set time-since-emergence random 11
  ]
  ask n-of (initial-number-of-mosquitoes * 0.4) mosquitoes with [blood-seeking?] ; initial Input - 40% blood-fed mosquitoes
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

to assign-color                     ; the colors for represent the virus circulation and individual status
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

    ;;after YFV emergence the list start to store data on the proportion of infected mosquitoes in the population
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
    ;;if the mosquito that laid eggs is in the infectious state, the value 1 will be put in the larval-viral-status list, otherwise, the value 0 will be put
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
   let mosquito-abundance count mosquitoes
   ask patches with [patch-type = "breeding site" and not empty? larval-development-time]
   [
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

to-report YFV-circulation-days
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
