;***************************************************
;**************** SET-UP PROCEDURES ****************
;***************************************************

;; these are the global variables
globals
[
  ;; pertaining to simulation
  DT
  PS
  SIMULATION-TIME-LIMIT

  ;; pertaining to habitat
  N-POSSIBLE-TPS
  N-POSSIBLE-REFUGES
  OBST-RADIUS-RANGE
  RADIUS-GAP
  GAP-BW-OBSTACLES
  GAP-BW-TPS
  REFUGE-AREA
  REFUGE-ATTRACTION-MULTIPLIER
  refuges
  obstacles
  target-patches
  neutral-patches
  in-order-of-attractiveness

  ;; pertaining to predator and prey
  RHO
  K
  TIME-TO-TARGET
  FORAGE-SPEED
  STALK-SPEED
  ACCEPTABLE-HEADING-OFFSET
  REACTION-TIME
  prey-detected
  has-pursued
  prey-endurance
  predator-endurance
  prey-can-circle-obstacles
  stalking
  kill-dist
  conspicuous
  reaction-counter
  prey-LL
  pred-LL

  ;; pertaining to output
  predator-win
  prey-win
  ticks-to-detection
  number-obstacles
  detect-time
  pursuit-time
  sim-time
  prey-escape-length
  predator-pursuit-length
  prey-curviness
  predator-curviness

  ;; threshholds for stochastic elements
  CONTINUE-ESCAPING
  LOST-PREY
  SET-NEW-HEADING-FORAGE
  TIME-IN-REFUGE

]

;; patches have these attributes
patches-own
[
  patch-attractiveness
  obstacle-cluster
  cluster-radius
  cluster-centre
  i-am-centre
  prey-obstacle-patch
  predator-obstacle-patch
  is-patch-visible?
]

;; predators and prey are both a breed of turtle
breed [ prey a-prey ]
breed [ predators predator ]

;; both predators and prey have these attributes
turtles-own
[
  ;; user-defined
  limb-length
  vision-distance
  vision-angle
  obstacle-sensitivity

  ;; derived & internal
  body-mass
  minimum-duty-factor
  ground-reaction-force
  acceleration
  acceleration-patches
  deceleration
  deceleration-patches
  max-velocity
  max-velocity-patches
  desired-velocity
  current-velocity
  ticks-since-turn
  agility
  agility-patches
  behaviour
  circling-radius
  circling-direction
  desired-position
  new-desired-position
  target-gain
  ideal-heading
  new-ideal-heading
  ideal-heading-gain
  ideal-heading-decay
  desired-heading
  factor
  pursuit-length
  pursuit-angle
]

;; only prey have these attributes
prey-own
[
  foraging-behaviour
  count-down
  time-to-detection
  FID
  wary-distance
  escape-count
]

;; only predators have these attributes
predators-own
[
   pursuit-count-down
   predator-lost-prey
]



;**************** TO SETUP ****************
to setup
  clear-all

  ;; SET UP GLOBAL VARIABLES
  ;; simulation-related
  set DT 0.05                  ;; time increment [s/tick] in each tick (note that very large DTs are likely to cause problematic behaviours, like jumping over obstacles)
  set PS 0.5                   ;; length [m/patch] of each patch (note that very small patches will slow down setup time, but patch sizes that are too lare will likely cause turtles to step over obstacles)
  set SIMULATION-TIME-LIMIT 15 ;; maximum length of the simulation [minutes]

  ;; habitat-related
  set N-POSSIBLE-TPS 10                ;; number of patches to query when assigning target-patches
  set N-POSSIBLE-REFUGES 100           ;; number of patches to query when assigning refuges
  set OBST-RADIUS-RANGE 0.5            ;; range in possible obstacle radius [m]
  set RADIUS-GAP 2                     ;; [patches]
  set GAP-BW-OBSTACLES 3               ;; [patches]
  set GAP-BW-TPS 5.0                   ;; [m]
  set REFUGE-AREA 1.0                  ;; [m]
  set REFUGE-ATTRACTION-MULTIPLIER 500 ;; refuge attraction multiplier (controls the scaling of refuge attractiveness to prey with distance)

  ;; predator and prey related
  set RHO 10                                ;; mass density per unit volume [kg/m^3]
  set K 50                                  ;; contact force constant of the turtle's foot on the ground when running
  set TIME-TO-TARGET 60                     ;; time at which the turtle should stop heading towards desired-position [s]
  set FORAGE-SPEED 0.2                      ;; proportion of maximum speed predator and prey use when foraging
  set STALK-SPEED 0.1                       ;; proportion of maximum speed predator uses when stalking
  set ACCEPTABLE-HEADING-OFFSET 0.5         ;; maximum acceptable difference between desired and current heading before a turn must be made [deg]
  set REACTION-TIME 0.2                     ;; reaction time [s]
  set kill-dist (kill-distance / PS)        ;; distance [patches] at which the predator can kill the prey
  set has-pursued 0                         ;; defines whether or not the predator has pursued or not - 0 for no, and 1 for yes
  set prey-detected 0                       ;; defines whether or not the predator has detected the prey or not - 0 for no, 1 for yes
  set reaction-counter (REACTION-TIME / DT) ;; set the reaction-counter to the reaction time in ticks initially
  set conspicuous -1
  set prey-endurance TRUE                   ;; does prey-endurance-distance have any affect on prey escape length?
  set predator-endurance TRUE               ;; does predator-endurance-distance have any effect on predator pursuit length?
  set prey-can-circle-obstacles TRUE        ;; can the prey circle obstalces?
  set stalking TRUE                         ;; should the predator stalk toward detected prey before pursuing?
  set prey-LL prey-limb-length
  set pred-LL predator-limb-length

  ;; output related
  set prey-win 5                                   ;; predator catches prey (0 = no, 1 = yes, 5 = unknown)
  set predator-win 5                               ;; prey escapes predator (0 = no, 1 = yes, 5 = unknown
  set ticks-to-detection 0                         ;; [ticks] for predator to detect the prey
  set detect-time 0                                ;; [s] for a predator to detect the prey
  set pursuit-time 0                               ;; length of the pursuit [s]
  set sim-time 0                                   ;; length of the simulation [s]
  set prey-escape-length 0                         ;; prey escape path length [m]
  set predator-pursuit-length 0                    ;; predator pursuit path length [m]
  set prey-curviness 0                             ;; prey escape path curviness [??]
  set predator-curviness 0                         ;; predator pursuit path curviness [??]

  ;; threshholds for stochastic elements
  set CONTINUE-ESCAPING (10 * 60)                  ;; (approx) time the prey should continue escaping [s]
  set LOST-PREY 0.5                                ;; (approx) chance that predator loses prey after beginning to pursue
  set SET-NEW-HEADING-FORAGE 20000                 ;; (approx) time after which the turtles should set a new heading when foraging [s]
  set TIME-IN-REFUGE 120                           ;; (approx) time the prey should remain in refuge [s] (if simulation does not terminate when prey enters refuge)

  ;; SET UP OBSTACLES
  setup-obstacles

  ;; SET UP TARGET PATCHES
  setup-target-patches

  ;; SET UP REFUGES
  setup-refuges

  ;; GROUP REMAINING PATCHES
  set neutral-patches patches with [ pcolor = black ] ;; call the remaining black patches "neutral-patches"

  ;; CREATE PREY AND INITIALISE VARIABLES
  create-prey 1
  [
    set shape "mouse top"
    set color orange
    set size 7
    ;; set up the prey's specific attributes
    set limb-length prey-limb-length                    ;; [m]
;    set max-velocity prey-max-velocity
;    set agility prey-agility
;    set acceleration prey-acceleration
;    set deceleration -1 * prey-deceleration
    set vision-distance (prey-vision-distance / PS)    ;; [patches]
    set vision-angle prey-vision-angle                 ;; [degrees]
    set FID (flight-initiation-distance / PS)          ;; [patches]
    set wary-distance (freeze-distance / PS)           ;; [patches]
    set obstacle-sensitivity prey-obstacle-sensitivity ;; the degree to which prey are repulsed by obstacles [close to 0 = not at all, close to 1 = completely repelled]
    set circling-direction 1                           ;; initial circling direction
    set foraging-behaviour "stay"                      ;; makes the prey stay in one place when it starts to forage
    set escape-count 0                                 ;; records how long the prey has been escaping for
    set count-down 20                                  ;; how long the prey should stay in one place while foraging [ticks]
    move-to one-of neutral-patches
  ]

  ;; CREATE PREDATORS AND INITIALISE VARIABLES
  create-predators 1
  [
    set shape "spider"
    set color white
    set size 7
    ;; set up the predator's specific attributes
    set limb-length predator-limb-length                   ;; [m]
;    set max-velocity predator-max-velocity
;    set agility predator-agility
;    set acceleration predator-acceleration
;    set deceleration -1 * predator-deceleration
    set vision-distance (predator-vision-distance / PS)    ;; [patches]
    set vision-angle predator-vision-angle                 ;; [degrees]
    set obstacle-sensitivity predator-obstacle-sensitivity ;; the degree to which predators are repulsed by obstacles [close to 0 = not at all, close to 1 = completely repelled]
    set pursuit-count-down 0                               ;; records the length of the pursuit [ticks]
    set predator-lost-prey 0                               ;; predator starts off not having lost the prey
    move-to one-of neutral-patches
    if any? prey in-radius 1
    [ move-to one-of other neutral-patches]
  ]

  ;; INITIALISE VARIABLES COMMON TO BOTH PREDATORS AND PREY
  ask turtles
  [
    set ticks-since-turn 0 ;; set the initial number of ticks since the last turn to be 0
    ;; Limb length dependent settings (comment out if user wishes to set performance manually as commented out above)
    set body-mass 162 * limb-length ^ (1 / 0.34)                                                                               ;; [kg] (using the scaling relationship for fissipeds from Alexander et al. 1979)
    set minimum-duty-factor 0.1 * body-mass ^ (0.1)                                                                            ;; fraction of the stride where the foot is on the ground (from Alexander et al. 1981)
    set ground-reaction-force 4 * (body-mass / minimum-duty-factor)                                                            ;; [N]
    set acceleration (ground-reaction-force - (9.8 * body-mass)) / body-mass                                                   ;; [m/s^2]
    set deceleration -1 * acceleration                                                                                         ;; [m/s^2]
    set max-velocity 10 ^ (1.478 + 0.2589 * (log (RHO * limb-length ^ 3) 10) - 0.0623 * (log (RHO * limb-length ^ 3) 10 ) ^ 2) ;; [m/s] (from Garland 1983)
    set agility sqrt(K / (RHO * limb-length))                                                                                  ;; maximum speed around a turn of 1 m radius [m/s] (based on Wilson et al. 2015)
    set agility-patches (agility / PS)                                                                                         ;; [patches/s]
    set acceleration-patches (acceleration / PS)                                                                               ;; [patches/s^2]
    set deceleration-patches (deceleration / PS)                                                                               ;; [patches/s^2]
    set max-velocity-patches (max-velocity / PS)                                                                               ;; [patches/s^2]
    set agility-patches (agility / PS)                                                                                         ;; [patches/s^2]
    set ideal-heading-decay 0.95 ^ DT
    set target-gain 1
    set current-velocity 0
    set pursuit-length 0                                                                                                       ;; measures the distance the turtle has moved in patches during pursuit
    set pursuit-angle 0                                                                                                        ;; measures the total angle the turtle has moved through in degrees during pursuit
    set new-desired-position one-of [ other neutral-patches ] of patch-here
  ]

  reset-ticks
end



;**************** TO SETUP-OBSTACLES ****************
;; Sets up clusters of impenetrable brown patches that the turtles must avoid; uses user-defined settings, "proportion-obstacles" (the proportion of the total
;; patches that are obstacles), "obstacle radius" (the average radius, in patches, for a cluster of obstacles), and "obstacle-radius-range" (how many patches
;; the radius should vary by); called by "setup"
to setup-obstacles
  if obstacle-proportion > 0 ;; only do the following if the user has specified obstacles to be set (speeds up setup time)
  [
    set number-obstacles 1

    ;; GENERATE OBSTACLES (which will be brown)
    ask patches with [ random-float 1.0 <= obstacle-proportion ]
    [
      if (count patches with [ pcolor = brown ] / count patches < obstacle-proportion) ;; is the number of brown patches less than the user defined rough-obstacle-proportion?
      [ ;; if yes
        let centre-patch self
        let obstacle-radius-patch (obstacle-radius / PS) ;; set the obstacle radius in patches
        let obstacle-radius-range-patch (OBST-RADIUS-RANGE / PS) ;; set the obstacle radius range in patches
        let random-radius (obstacle-radius-patch - obstacle-radius-range-patch / RADIUS-GAP + random-float obstacle-radius-range-patch) ;; specify a random radius for the patch cluster

        if not any? patches in-radius (random-radius + GAP-BW-OBSTACLES) with [ pcolor = brown ] ;; are there any patches in the randomly defined radius PLUS three patches?
        [ ;; if no
          set i-am-centre 1
          ask patches with [ distance myself < random-radius ]
          [
            set pcolor brown
            set obstacle-cluster number-obstacles ;; set the patch's obstacle-cluster identifier to whatever unique number has been defined for this cluster
            set cluster-radius random-radius ;; set the patch's obstacle-radius identifier to whatever random-radius has been defined for this cluster
            set cluster-centre centre-patch
          ]
        set number-obstacles number-obstacles + 1 ;; change the object-cluster identifier for the next patch cluster
        ]
      ]
    ]

    ;; GET RID OF "EXCESS" OBSTACLE PATCHES
    let excess (count patches with [ pcolor = brown ]) - obstacle-proportion * count patches ;; calculates the excess brown patches
    while [ excess > 1 ] ;; while the excess is greater than 1
    [
      let boundary-patches (patches with [ pcolor = brown and count neighbors4 with [ pcolor = black ] > 1 ])
      ask one-of boundary-patches
      [ set pcolor black ]
      set excess (count patches with [ pcolor = brown ]) - obstacle-proportion * count patches
    ]
  ]

;; CREATE OBSTACLE PATCH SET
set obstacles patches with [ pcolor = brown ]
end



;**************** TO SETUP-TARGET-PATCHES ****************
;; Sets up target-patches for prey to run to while escaping (the criteria for target-patches is that they are as close to as many obstacle clusters as
;; possible if the prey is more agile than the predator, and as close to as few obstacle clusters as possible if the predator is more agile than the
;; prey); uses user-defined setting, "number-of-target-patches"; called by "setup"
to setup-target-patches

  ;; ONLY DO THE FOLLOWING IF THE USER HAS SPECIFIED TARGET PATCHES TO BE SET (SPEEDS UP SETUP TIME)
  if number-of-target-patches > 0
  [
    ;; ONLY BOTHER LOOKING FOR GOOD PATCHES IF THE USER HAS SPECIFIED OBSTACLES TO BE SET
    ifelse obstacle-proportion > 0
    [ ;; if there are obstacles

      ;; Query a subset [N-POSSIBLE-TPS] of the black patches rather than every black patch to reduce set up time
      ask n-of (number-of-target-patches * N-POSSIBLE-TPS) patches with [ pcolor = black ]
      [
        ;; Determine the "attractiveness" of each of the black patches in terms of where they are located with respect to obstacle clusters
        let nearby-radius (2.0 / PS) ;; we want to look in radius 2 m around the patch for obstacles - calculate what this is in patches
        let nearby-obstacles patches in-radius nearby-radius with [ pcolor = brown ]
        ifelse any? nearby-obstacles ;; are there any nearby obstacles?
        [ ;; if yes
          let sorted-nearby-ids sort [ obstacle-cluster ] of nearby-obstacles ;; sorts the nearby obstacle's obstacle-cluster IDs
          let unique-nearby-ids reduce [ [?1 ?2] -> ifelse-value ((first ?1) = ?2) [ ?1 ][ fput ?2 ?1 ] ] (fput (list first sorted-nearby-ids) (but-first sorted-nearby-ids))
          set patch-attractiveness length unique-nearby-ids
        ]
        [ ;; if no nearby obstacles, the patch has attractiveness 0
          set patch-attractiveness 0
        ]
      ]
      ;; set up a subset of the total remaining black patches, containing the user defined number of target patches, and select them in order of highest patch
      ;; attractiveness
      ifelse prey-LL < pred-LL ;; if the prey is smaller (more agile) than the predator
      [
        set in-order-of-attractiveness sort-on [ (- patch-attractiveness) ] patches with [ pcolor = black ] ;; creates a list of all patches in order of descending patch attractiveness
      ]
      [
        set in-order-of-attractiveness sort-on [ ( patch-attractiveness) ] patches with [pcolor = black ] ;; creates a list of all patches in order of ascending patch attractiveness
      ]

      foreach (in-order-of-attractiveness)
      [ [?1] -> if count patches with [ pcolor = yellow ] < number-of-target-patches
        [
          ask ?1
          [
            if not any? patches in-radius (GAP-BW-TPS / PS) with [ pcolor = yellow ]
            [
              set pcolor yellow
            ]
          ]
        ]
      ]
    ]
    [ ;; if there are NOT obstacles, just choose a random selection of black patches and turn them yellow
      let random-tps n-of number-of-target-patches patches with [ pcolor = black ]
      ask random-tps [ set pcolor yellow ]
    ]
  ]
  set target-patches patches with [ pcolor = yellow ] ;; convert this list of patches to a patchset, and call it "target patches"
end



;**************** TO SETUP-REFUGES ****************
;; Sets up refuges with a set radius for prey to run to while escaping; uses user-defined setting, "number-of-refuges"; called by "setup".
to setup-refuges
  if number-of-refuges > 0 ;; only do the following if the user has specified refuges to be set (speeds up setup time)
  [
    let possible-refuges n-of N-POSSIBLE-REFUGES patches with [ pcolor = black ] ;; create an agentset of 100 black patches (can also ask all black patches but this makes the setup slow)
    ask possible-refuges
    [
      let centre-patch self
      let radius (1.0 / PS)

      if any? patches in-radius ( radius + 2) with [ pcolor != black ] ;; are there any obstacles or target patches within the radius PLUS two patches of
                                                                       ;; the refuge?
      [ ;; if yes
        set possible-refuges other possible-refuges

      ]
    ]
    ;; set up a subset of the total remaining black patches, containing the user-defined number of refuges, and select them randomly
    let possible-refuges-list [ self ] of possible-refuges ;; convert our agentset of possible refuges to a list

    foreach (possible-refuges-list) ;; for each entry in this list
    [ [?1] -> if count patches with [ pcolor = green ] < ( number-of-refuges * ( pi * (REFUGE-AREA / PS) ^ 2 )) ;; if the number of green patches is less than the user defined number of
                                                                                                        ;; refuges multiplied by the number of patches we expect the refuges to take
                                                                                                        ;; up (i.e. the area of a refuge)
      [
        ask ?1
        [
          ask patches in-radius (REFUGE-AREA / PS) ;; ask patches in the radius we want the refuge to be to turn green
          [ set pcolor green ]
        ]
      ]
    ]
  ]

  set refuges patches with [ pcolor = green ] ;; set up a patchset containing all the green patches and call it "refuges"
end





;*****************************************************
;**************** RUN-TIME PROCEDURES ****************
;*****************************************************


;**************** TO GO ****************
to go
  ;; ASK THE PREY THE FOLLOWING:
  ask prey
  [
    ifelse pcolor = green and behaviour = "escape" ;; is prey on a green patch (a refuge) and escaping?
    [ ;; if yes, the prey is safe
      set color green ;; make the prey invisible
      set behaviour "safe"
      safe
    ]
    [ ;; if no, the prey is not safe

      ifelse prey-endurance = TRUE
      [ ;; if PREY ENDURANCE is switched ON
        ifelse behaviour = "exhausted" ;; is the prey exhausted?
        [ ;; if yes, the prey is exhausted
          set behaviour "exhausted"
          freeze
        ]
        [ ;; if no, the prey is not exhausted
          if behaviour = "forage" ;; is the prey's behaviour "forage"?
          [ ;; if the prey is foraging
            let detector self
            let detector-heading heading ;; set the prey's current heading as agent-heading
            let detector-speed current-velocity ;; set the prey's current velocity as agent-speed
            let detector-max-speed max-velocity-patches ;; set the prey's maximum velocity as agent-max-speed
            let detector-vision-angle vision-angle ;; set the prey's vision angle as agent-vision-angle
            let detector-vision-dist vision-distance ;; set the prey's vision distance as agent-vision-dist
            let detector-obstacles nobody
            set detector-obstacles obstacles
            let visible-predators predators with [ color != black ] in-radius vision-distance ;; set visible predators to all predators within a radius of the user defined prey
                                                                                              ;; vision distance around the prey
            ask visible-predators ;; ask each visible predator
            [
              let detectee-speed current-velocity ;; let the predator's current velocity be animal-speed
              let detectee-distance [ distance self ] of detector ;; calculate the distance between the predator and the prey
              if not is-visible? detector detector-heading detector-speed detector-max-speed detector-vision-angle detector-vision-dist detectee-speed detectee-distance detector-obstacles
              [ ;; if NOT visible when passed through is-visible?
                set visible-predators other visible-predators ;; remove itself from the list of visible predators
              ]
            ]
            if any? visible-predators ;; are there any visible predators?
            [ ;; if yes
              let distance-to-predator distance min-one-of visible-predators [distance myself] ;; extract the distance from the prey to the nearest predator
              ifelse distance-to-predator <= FID OR member? true ([ behaviour = "pursue" ] of visible-predators)
              ;; is the closest predator within the prey's flight initiation distance, or do any of the visible predator's have behaviour = pursue?
              [ ;; if yes, then escape
                set desired-velocity max-velocity-patches
                get-away-from-predators visible-predators
                ifelse any? [  other target-patches ] of patch-here ;; are there any target patches?
                [ ;; if yes
                  set new-desired-position one-of [ other target-patches ] of patch-here ;; if there are target patches, the prey should want to use them
                ]
                [ ;; if no
                  set new-desired-position one-of [ other neutral-patches ] of patch-here ;; if there are no target patches, the prey should pick any patch
                ]
                calculate-desired-heading ;; calculate the desired heading (we want this to occur regardless of where the prey is in its reaction time?)
                set behaviour "escape"
              ]
              [ ;; if the closest visible predator is NOT within the flight initiation distance and none of the predators are pursuing
                ifelse distance-to-predator <= wary-distance ;; is the predator within the "wary" distance of the prey?
                [ ;; if yes, then freeze
                  set foraging-behaviour "cryptic"
                  freeze
                ]
                [ ;; if no, then forage
                  set behaviour "forage"
                  forage
                ]
              ]
            ]
          ]
        ]
      ]
      [ ;; if PREY-ENDURANCE is switched OFF
        if behaviour = "forage" ;; is the prey's behaviour "forage"?
          [
            let detector self
            let detector-heading heading ;; set the prey's current heading as agent-heading
            let detector-speed current-velocity ;; set the prey's current velocity as agent-speed
            let detector-max-speed max-velocity-patches ;; set the prey's maximum velocity as agent-max-speed
            let detector-vision-angle vision-angle ;; set the prey's vision angle as agent-vision-angle
            let detector-vision-dist vision-distance ;; set the prey's vision distance as agent-vision-dist
            let detector-obstacles nobody
            set detector-obstacles obstacles
            let visible-predators predators with [ color != black ] in-radius vision-distance ;; set visible predators to all predators within a radius of the user defined prey
                                                                                              ;; vision distance around the prey
            ask visible-predators ;; ask each visible predator
            [
              let detectee-speed current-velocity ;; let the predator's current velocity be animal-speed
              let detectee-distance [ distance self ] of detector ;; calculate the distance between the predator and the prey
              if not is-visible? detector detector-heading detector-speed detector-max-speed detector-vision-angle detector-vision-dist detectee-speed detectee-distance detector-obstacles
              [ ;; if NOT visible when passed through is-visible?
                set visible-predators other visible-predators ;; remove itself from the list of visible predators
              ]
            ]
            if any? visible-predators ;; are there any visible predators?
            [ ;; if yes
              let distance-to-predator distance min-one-of visible-predators [distance myself] ;; extract the distance from the prey to the nearest predator
              ifelse distance-to-predator <= FID OR member? true ([ behaviour = "pursue" ] of visible-predators)
              ;; is the closest predator within the prey's flight initiation distance, or do any of the visible predator's have behaviour = pursue?
              [ ;; if yes, then escape
                set desired-velocity max-velocity-patches
                get-away-from-predators visible-predators
                ifelse any? [  other target-patches ] of patch-here ;; are there any target patches?
                [ ;; if yes
                  set new-desired-position one-of [ other target-patches ] of patch-here ;; if there are target patches, the prey should want to use them
                ]
                [ ;; if no
                  set new-desired-position one-of [ other neutral-patches ] of patch-here ;; if there are no target patches, the prey should pick any patch
                ]
                calculate-desired-heading ;; calculate the desired heading (we want this to occur regardless of where the prey is in its reaction time?)
                set behaviour "escape"
              ]
              [ ;; if the closest visible predator is NOT within the flight initiation distance and none of the predators are pursuing
                ifelse distance-to-predator <= wary-distance ;; is the predator within the "wary" distance of the prey?
                [ ;; if yes, then freeze
                  set foraging-behaviour "cryptic"
                  freeze
                ]
                [ ;; if no, then forage
                  set behaviour "forage"
                  forage
                ]
              ]
            ]
          ]
        ]

       ifelse behaviour = "escape" and random-float 1.0 > (DT / CONTINUE-ESCAPING) ;; is the prey's behaviour "escape"?
       [ ;; if yes
         set behaviour "escape"
         ifelse prey-endurance = TRUE ;; is prey-endurance on?
         [ ;; if prey endurance IS on
           ifelse pursuit-length >= (prey-exhaustion-distance / PS) ;; is the prey exhausted?
           [ ;; if yes, go back to foraging
             set behaviour "exhausted"
             freeze
           ]
           [ ;; if no, continue escaping
             set desired-velocity max-velocity-patches
             move desired-velocity ;; escape, with acceleration and deceleration being true
             set escape-count (escape-count + 1) ;; add one tick to escape-count
             set pursuit-length pursuit-length + (current-velocity * DT) ;; add this distance on to pursuit length
           ]
        ]
        [ ;; if prey-endurance is NOT on, continue escaping
          set desired-velocity max-velocity-patches
          move desired-velocity ;; escape, with acceleration and deceleration being true
          set escape-count (escape-count + 1) ;; add one tick to escape-count
          set pursuit-length pursuit-length + (current-velocity * DT) ;; add this distance on to pursuit length
        ]
      ]
      [ ;; if no
        ifelse prey-can-circle-obstacles = TRUE
        [ ;; CIRCLING BEHAVIOUR (if prey-can-circle-obstacles is ON)
          ifelse behaviour = "circle" ;; is the prey circling an obstacle?
          [ ;; if yes, circling
            ifelse prey-endurance = TRUE
            [ ;; if prey endurance IS on
              ifelse pursuit-length >= (prey-exhaustion-distance / PS) ;; is the prey exhausted?
              [ ;; if yes, freeze
                set behaviour "exhausted"
                freeze
              ]
              [ ;; if no, continue circling
                set escape-count (escape-count + 1)
                circle-the-obstacle
              ]
          ]
          [ ;; if prey-endurance is NOT on, keep circling
            set escape-count (escape-count + 1)
            circle-the-obstacle
          ]
       ]
       [ ;; if not circling
         ifelse prey-endurance = TRUE
         [  ;; if prey endurance IS on
            ifelse pursuit-length >= (prey-exhaustion-distance / PS) ;; is the prey exhausted?
            [ ;; if yes, freeze
              set behaviour "exhausted"
              freeze
            ]
            [
              set behaviour "forage"
              forage ;; start "forage" behaviour
             ]
         ]
         [ ;; if prey endurance is OFF
             set behaviour "forage"
             forage ;; start foraging behaviour
         ]
       ]
     ]
     [ ;; NON-CIRCLING BEHAVIOUR (if prey-can-circle-obstacles is OFF)
       ifelse prey-endurance = TRUE
        [  ;; if prey endurance IS on
           ifelse pursuit-length >= (prey-exhaustion-distance / PS) ;; is the prey exhausted?
           [ ;; if yes, freeze
             set behaviour "exhausted"
             freeze
           ]
           [
             set behaviour "forage"
             forage ;; start "forage" behaviour
            ]
        ]
        [ ;; if prey endurance is OFF
            set behaviour "forage"
            forage ;; start foraging behaviour
        ]
      ]
    ]
  ]


    update-prey-color ;; update prey's colour depending on it's behavioural status
    set ticks-since-turn (ticks-since-turn + 1) ;; update the number of ticks since a turn

  ]

  ;; ASK THE PREDATORS THE FOLLOWING:
  ask predators with [ color != black ] ;; ask all non-exhausted predators
  [
    if has-pursued = 1
    [
      set pursuit-count-down pursuit-count-down + 1 ;; if the predator HAS pursued the prey, add one tick to the pursuit-count-down
    ]
    let detector self ;; make the predator the detector
    let detector-heading heading ;; set the predator's current heading as detector-heading
    let detector-speed current-velocity ;; set the predator's current velocity as detector-speed
    let detector-max-speed max-velocity-patches ;; set the predator's maximum velocity as detector-max-speed
    let detector-vision-angle vision-angle ;; set the predator's vision angle as detector-vision-angle
    let detector-vision-dist vision-distance ;; set the predator's vision distance as detector-vision-dist
    let detector-obstacles nobody
    set detector-obstacles obstacles
    let visible-prey prey in-radius vision-distance ;; set visible prey to all prey within a radius of the user defined predator vision distance around the predator
    ask visible-prey ;; ask each visible prey
    [
      let detectee-speed current-velocity ;; set the prey's current velocity as detectee-speed
      let detectee-distance [ distance self ] of detector ;; calculate the distance between the predator and the prey
      if not is-visible? detector detector-heading detector-speed detector-max-speed detector-vision-angle detector-vision-dist detectee-speed detectee-distance detector-obstacles
      [ ;; if NOT visible when passed through is-visible?
        set visible-prey other visible-prey ;; remove itself from the list of visible prey
      ]
    ]

    ifelse any? visible-prey ;; are there any visible prey?
    [ ;; if there ARE visible prey
      let target-prey min-one-of visible-prey [ distance myself ] ;; choose the closest as the predator's target prey
      ifelse [ behaviour = "escape" ] of target-prey or [ behaviour = "exhausted" ] of target-prey ;; is the target prey's behaviour "escape"?
      [ ;; if yes, the target prey is escaping
        if has-pursued = 0 ;; if the predator has not pursued the prey before
        [
          set prey-detected 1
          set ticks-to-detection ticks ;; record the number of ticks it took to detect the prey
        ]
        set behaviour "pursue"
        pursue target-prey ;; the predator has been detected and should start "pursue" behaviour
        set pursuit-length pursuit-length + (current-velocity * DT) ;; add the distance moved to pursuit-length
      ]
      [ ;; if no, the target prey is not escaping
        ifelse [ behaviour = "safe" ] of target-prey ;; is the target prey's behaviour "safe"?
        [ ;; if yes, prey is safe
          set behaviour "search" ;; prey is inaccessible and predator should "search" for new prey
          search
        ]
        [ ;; if no, then the prey is either foraging or exhausted
          if has-pursued = 0 ;; if the predator has not pursued the prey before
          [
            set prey-detected 1
            set ticks-to-detection ticks ;; record the number of ticks it took to detect the prey
          ]
          ifelse stalking = TRUE ;; can the predator stalk?
          [ ;; if the predator CAN stalk
            set behaviour "stalk"
            stalk target-prey ;; stalk target prey
          ]
          [ ;; if the predator CANNOT stalk
            set behaviour "pursue"
            pursue target-prey ;; pursue target prey
            set pursuit-length pursuit-length + (current-velocity * DT) ;; add the distance moved to pursuit length
          ]
        ]
      ]
    ]
    [ ;; if there are no visible prey
      ifelse has-pursued = 1 and random-float 1.0 < LOST-PREY ;; if the predator has "lost the prey" for good
      [ ;; if not lost for good, keep pursuing
        let target-prey min-one-of prey [ distance myself ]
        pursue target-prey
      ]
      [ if has-pursued = 1 ;; if lost for good
        [
          set predator-lost-prey 1
        ]
        set behaviour "search" ;; otherwise, if has-pursued is not 1, then set the predator's behaviour to "search"
        search ;; the predator should start "search" behaviour
      ]
    ]

    update-predator-color ;; update the predator's colour depending on it's behavioural state
    set ticks-since-turn (ticks-since-turn + 1)

    if predator-endurance = TRUE ;; if the endurance switch is ON
    [
      if pursuit-length >= (predator-exhaustion-distance / PS) [ set color black ] ;; if predator has been pursuing for more than the user-defined predator-exhaustion-time, make
                                                                                   ;; the predator turn black
    ]
  ]

  ;; specify conditions where program should stop
  if any? prey with [ color = black ] ;; if the prey gets eaten
  [
    set predator-win 1 ;; set the predator win to true
    set prey-win 0 ;; set the prey win to false
    stop
  ]
  if count predators with [ color = black ] >= count predators ;; if all the predators give up
  [
    set predator-win 0 ;; set the predator win to false
    set prey-win 1 ;; set the prey win to true
    stop
  ]
  if any? prey with [ behaviour = "safe" ] ;; if the prey escape to the refuge
  [
    set predator-win 0 ;; set the predator win to false
    set prey-win 1 ;; set the prey win to true
    stop
  ] ;; stop the program if a prey enters a refuge

;  if predator-lost-prey = 1
;  [
;    set predator-win 0
;    set prey-win 1
;    stop
;  ];; stop the program if the predator loses the prey

  if ticks >= ((SIMULATION-TIME-LIMIT * 60) / DT)
  [
    set predator-win 0
    set prey-win 1
    stop
  ] ;; stop the program at the maximum time limit

  ask turtles ;; ask both predators and prey
  [
    if member? patch-here obstacles ;; if the turtle hits an obstacle
    [
      set current-velocity 0 ;; make it stop moving (set it's current velocity to zero)
    ]
  ]

  ifelse reaction-counter = 0
  [ ;; if reaction-counter IS at zero
    set reaction-counter (REACTION-TIME / DT) ;; restart
  ]
  [ ;; if reaction-counter is NOT at zero
    set reaction-counter (reaction-counter - 1) ;; subtract one tick from the reaction-counter
  ]

  ;; set model outputs
  set detect-time (ticks-to-detection * DT)
  set pursuit-time ([ escape-count ] of a-prey 0 * DT)
  set sim-time (ticks * DT)
  set prey-escape-length ([ pursuit-length ] of a-prey 0 * PS)
  set predator-pursuit-length ([ pursuit-length ] of predator 1 * PS)
  ifelse [pursuit-length] of a-prey 0 > 0
  [ set prey-curviness ((( pi * [ pursuit-angle] of a-prey 0 ) / 180) / ([ pursuit-length ] of a-prey 0 * PS)) ]
  [ set prey-curviness 0 ]
  ifelse [pursuit-length] of predator 1 > 0
  [ set predator-curviness ((( pi * [ pursuit-angle] of predator 1) / 180) / ([ pursuit-length ] of predator 1 * PS)) ]
  [ set predator-curviness 0 ]
  tick
end



;**************** TO FORAGE ****************
;; Makes the turtle forage, which involves running at slow speed for 5 seconds, then stopping for 5 seconds, and so on; calls "running" and "stay"; called by "go" and "safe"
to forage
  ifelse foraging-behaviour = "running" ;; is the prey's foraging behaviour set to "running"?
  [ ;; if yes, then it keeps running
    running
  ]
  [ ;; if no, then it keeps staying
    stay
  ]
end



;**************** TO STAY ****************
;; Makes the turtle stay in one place for 5 seconds, then start searching; calls "search"; called by "forage"
to stay
  set count-down (count-down - DT)   ;decrement-timer
  ifelse count-down <= 0
  [
    set count-down 5
    set foraging-behaviour "running"
    running
  ]
  [
    set desired-velocity 0
    new-random-target-patch
    move desired-velocity
  ]
end

;**************** TO FREEZE ****************
;; Makes the turtle stay in one place; called by "go"
to freeze
  set desired-velocity 0
  if behaviour != "exhausted"
  [
    move desired-velocity
  ]
end


;**************** TO RUNNING ****************
;; Makes the turtle search for 5 seconds, then stay; calls "search" and "stay"; called by "forage"
to running
  set count-down (count-down - DT) ;; decrement-timer
  ifelse count-down <= 0
  [
    set count-down 5
    set foraging-behaviour "stay"
    stay
  ]
  [
    search
  ]
end



;**************** TO SEARCH ****************
;; Makes the turtle forage, at slow speed; calls "move"; called by "go" and "safe"
to search
  new-random-target-patch

  if (random-float 1.0) <= SET-NEW-HEADING-FORAGE
  [
    set new-ideal-heading random-float 360; set a new heading, which will be the current heading plus some random number from 0 to 360
    calculate-desired-heading
  ]
  set desired-velocity (FORAGE-SPEED * max-velocity-patches) ;; set the desired velocity to 20% of maximum velocity
  move desired-velocity
end



;**************** TO MOVE ****************
;; Makes the turtle move around the world, trying to meet its desired heading and desired velocity; calls "accelerate", "calculate-desired-heading", "random-heading",
;; "get-away-from-predators", "new-target-patch", "circle-the-obstacle" and "turn"; called by "go", "forage", "stalk" and "pursue"
to move [ free-speed ] ;; "free-speed" is the desired velocity that move is passed

  if behaviour = "escape" ;; if behaviour is "escape"
  [
    new-target-patch ;; see if you need to pick a new target patch
  ]

  let next-heading-change subtract-headings desired-heading heading ;; set the next heading change to the difference between the current heading and the desired heading (a negative
                                                                    ;; number indicates a left turn, while a positive number indicates a right turn)

  ifelse abs(next-heading-change) < ACCEPTABLE-HEADING-OFFSET ;; is the difference between these two headings is less than 0.5 degrees (i.e. is the animal already going in the desired direction)?
  [ ;; if yes, keep going straight

    set factor 1 ;; set "factor" initially to 1
    set desired-velocity free-speed

    ifelse behaviour = "escape"
    [ ;; DEFINE BEHAVIOURS OF ESCAPING PREY (i.e. not the foraging prey, or the predator)

      if any? refuges ;; are there any refuges?
      [ ;; if yes
        let nearest-refuge min-one-of refuges [ distance self ] ;; choose the nearest refuge
        set factor ( (1 / distance nearest-refuge ) + 1 ) ;; make the time-to-turn dependent on how close the prey is to the refuge. The closer it is to the refuge, the less likely
                                                          ;; the prey is to turn
      ]

      ;; When the prey is escaping, the predator/s will mostly be behind it - so it will not be able to see the predator/s or how close it/they are getting.
      ;; We would like the prey to make a semi-random turn if it hasn't seen the predator in a stochastic time distribution dictated by the user-defined time-to-turn. This will
      ;; ensure the prey can make unpredictable turns to throw the predator off as it gets closer. Hence, time-to-turn is a way to set the prey's unpredictibility of movement

      ;; First we need to see if there are any visible predators
      let detector self
      let detector-heading heading ;; set the prey's current heading as agent-heading
      let detector-speed current-velocity ;; set the prey's current velocity as agent-speed
      let detector-max-speed max-velocity-patches ;; set the prey's maximum velocity as agent-max-speed
      let detector-vision-angle vision-angle ;; set the prey's vision angle as agent-vision-angle
      let detector-vision-dist vision-distance ;; set the prey's vision distance as agent-vision-dist
      let detector-obstacles nobody
      set detector-obstacles obstacles

      let visible-predators predators with [ color != black ] in-radius vision-distance ;; set visible predators to all predators within a radius of the user defined prey vision
                                                                                        ;; distance around the prey
      ask visible-predators ;; ask each visible predator
      [
        let detectee-speed current-velocity ;; let the predator's current velocity be animal-speed
        let detectee-distance [ distance self ] of detector ;; calculate the distance between the predator and the prey
        if not is-visible? detector detector-heading detector-speed detector-max-speed detector-vision-angle detector-vision-dist detectee-speed detectee-distance detector-obstacles
        [ ;; if NOT visible when passed through is-visible?
          set visible-predators other visible-predators ;; remove itself from the list of visible predators
        ]
      ]

      ifelse any? visible-predators ;; are there any visible predators?
      [ ;; if there ARE visible predators, the prey needs to choose an escape direction based on their relative positions
        set desired-velocity max-velocity-patches
        accelerate
        ifelse reaction-counter = 0
        [ ;; if there ARE visible predators and the prey CAN turn
          get-away-from-predators visible-predators
          calculate-desired-heading
          set next-heading-change subtract-headings desired-heading heading
          turn next-heading-change
        ]
        [ ;; if there ARE visible predators but the prey CANNOT turn
          forward current-velocity * DT
        ]
      ]
      [ ;; if there AREN'T visibile predators, the prey can either continue in its current direction, or make a random turn
        ifelse random-float 1.0 > DT / (time-to-turn * factor) ;; did the prey see a predator less than the distance-to-refuge-dependent time-to-turn ago?
        [ ;; if yes, then keep going in the current direction (no turn)

          accelerate
          ifelse member? patch-here obstacles ;; and ( behaviour = "escape") ;; is there an obstacle ahead AND is behaviour "escape"?
          [ ;; if there ARE NO visible predators and there IS an obstacle at the current patch
            ifelse prey-can-circle-obstacles = TRUE
            [ ;; CIRCLING CODE (if prey-can-circle-obstacles is ON)
              set behaviour "circle" ;; set behaviour to circle
              let centre [ cluster-centre ] of patch-here ;; define the patch that the turtle should circle
              set circling-radius [ cluster-radius + 1 ] of centre ;; define the radius that the turtle should circle at
              move-to [ patch-at-heading-and-distance (towards myself) ([ circling-radius ] of myself) ] of centre ;; move the turtle to the patch it should be at to begin circling
              set current-velocity 0 ;; set current velocity to zero
              set circling-direction (-1) ^ (random 2) ;; set the circling direction (positive is a clockwise turn, while negative is an anticlockwise turn)
              set heading towards centre + (-1 * circling-direction * 90) ;; set the heading to be directly perpendicular to the line to the centre patch
              circle-the-obstacle
            ]
            [ ;; NON-CIRCLING CODE (if prey-can-circle-obstacles is OFF)
              let not-stuck min-one-of ( patches in-radius 5 with [ not member? self obstacles ] ) [ distance myself ] ;; create a temporary patch called "not stuck", which is the
                                                                                                                    ;; closest non-obstacle patch to the turtle
              move-to not-stuck ;; make the turtle move to not-stuck, to ensure it never gets stuck in the obstacle
              set current-velocity 0
              accelerate
              set new-ideal-heading heading ;; set the ideal heading to the current heading (because we know its going the right way)
              set ideal-heading-gain 1 ;; make sure ideal heading gain is set to 1
              if patch-here = desired-position ;; has the turtle reached it's desired position?
              [ ;; if yes
                ifelse any? [ other target-patches ] of patch-here ;; are there any other target patches?
                [ set new-desired-position one-of [ other target-patches ] of patch-here ] ;; if yes, set the desired position to one of the other target patches
                [ set new-desired-position one-of [ other neutral-patches ] of patch-here ] ;; if no, set the desired position to one of the neutral patches
              ]
              calculate-desired-heading ;; we know where the turtle wants to go, but an obstacle is going to be in the way, so it needs a new heading to avoid the obstacle
            ]
          ]
          [ ;; if there ARE NO visible predators and there is NOT an obstacle at the current patch
            accelerate
            forward (current-velocity * DT) ;; move forward at the current velocity
            set new-ideal-heading heading ;; set the ideal heading to the current heading
            set ideal-heading-gain 1 ;; make sure ideal heading gain is set to 1
            if patch-here = desired-position ;; has the prey made it to its desired position?
            [ ;; if yes
              ifelse any? [ other target-patches ] of patch-here ;; are there any other target patches?
              [ set new-desired-position one-of [ other target-patches ] of patch-here ] ;; if yes, set the desired position to one of the other target patches
              [ set new-desired-position one-of [ other neutral-patches ] of patch-here ] ;; if no, set the desired position to one of the neutral patches
            ]
            if reaction-counter = 0 ;; if the prey is allowed to turn, calculate the new desired heading
            [
              calculate-desired-heading
            ]
          ]
        ]
        [ ;; if the prey did NOT see the predator less than the distance-to-refuge-depending time-to-turn ago, make a turn
          ifelse reaction-counter = 0
          [ ;; if prey HAS NOT seen the predator and CAN turn
            random-heading
            set next-heading-change subtract-headings desired-heading heading
            turn next-heading-change
            set new-ideal-heading heading
          ]
          [ ;; if the prey HAS NOT seen the predator but CANNOT turn
            set new-ideal-heading heading
          ]
        ]
      ]
    ] ;; END OF ESCAPE-ONLY BEHAVIOURS
    [ ;; DEFINE BEHAVIOURS OF NON-ESCAPING PREY (i.e. for the foraging prey, and the predator)

      ifelse member? patch-here obstacles
      [ ;; if there is an obstacle ahead

        let not-stuck min-one-of ( patches in-radius 5 with [ not member? self obstacles ] ) [ distance myself ] ;; create a temporary patch called "not stuck", which is the closest
                                                                                                                 ;; non-obstacle patch to the turtle
        move-to not-stuck ;; make the turtle move to not-stuck, to ensure it never gets stuck in the obstacle
        set current-velocity 0
        accelerate
        set new-ideal-heading heading ;; set the ideal heading to the current heading (because we know its going the right way)
        set ideal-heading-gain 1 ;; make sure ideal heading gain is set to 1
        if patch-here = desired-position ;; has the turtle reached it's desired position?
        [ ;; if yes
          ifelse any? [ other target-patches ] of patch-here ;; are there any other target patches?
          [ set new-desired-position one-of [ other target-patches ] of patch-here ] ;; if yes, set the desired position to one of the other target patches
          [ set new-desired-position one-of [ other neutral-patches ] of patch-here ] ;; if no, set the desired position to one of the neutral patches
        ]
        calculate-desired-heading ;; we know where the turtle wants to go, but an obstacle is going to be in the way, so it needs a new heading to avoid the obstacle
      ]
      [ ;; if there is no obstacle ahead
        accelerate ;; this should fix the prey's problem of not moving when foraging
        forward (current-velocity * DT) ;; move forward at the current velocity
        set new-ideal-heading heading ;; set the ideal heading to the current heading
        set ideal-heading-gain 1 ;; make sure ideal heading gain is set to 1
        if patch-here = desired-position
        [
          ifelse any? [ other target-patches ] of patch-here ;; are there any other target patches?
          [ set new-desired-position one-of [ other target-patches ] of patch-here ] ;; if yes, set the desired position to one of the other target patches
          [ set new-desired-position one-of [ other neutral-patches ] of patch-here ] ;; if no, set the desired position to one of the neutral patches
         ]
        calculate-desired-heading ;; we want this to occur regardless of the reaction time?
      ]
    ] ;; END OF NON-ESCAPING BEHAVIOURS

  ] ;; END OF NON-TURNING BEHAVIOURS
  [ ;; if the difference between the current heading and the desired heading is MORE than (or equal to) 0.5 degrees
    turn next-heading-change ;; try to turn toward the desired heading
  ]
end



;**************** TO SAFE ****************
;; Prey-specific, for when the prey enters a refuge; calls "forage"; called by "go".
to safe
  ifelse random-float 1.0 < (DT / TIME-IN-REFUGE) ;; did the prey enter the refuge more than TIME-IN-REFUGE ticks ago?
  [ ;; if yes, can leave refuge
    set current-velocity (FORAGE-SPEED * max-velocity-patches) ;; set the desired velocity to 20% of maximum velocity
    set color orange ;; make the prey visible
    forward (2 / PS) ;; move the prey forward 1 to get it out of the refuge
    set behaviour "forage"
    forage ;; set behaviour to forage
  ]
  [ ;; if no, must stay
    set heading random-float 360
  ]
end



;**************** TO STALK ****************
;; Predator-specific, for when the predator locates a target prey and moves slowly toward it; calls "move" and "calculate-desired-heading"
to stalk [target-prey]
  set color grey
  set new-ideal-heading towards target-prey ;; set the ideal heading as being towards the closest prey
  set desired-velocity (STALK-SPEED * max-velocity-patches) ;; set velocity to 10% of predator's maximum velocity
  set new-desired-position patch-at-heading-and-distance towards target-prey distance target-prey
  calculate-desired-heading ;; calculate the desired heading
  move desired-velocity
  if any? prey in-radius kill-dist ;; if the closest prey is within the kill distance
  [
    ask target-prey [ set color black ] ;; then kill it
  ]
end



;**************** TO PURSUE ****************
;; Predator-specific, for when the target prey has "escape" behaviour and the predator is in fast pursuit; calls "move", "turn" and "calculate-desired-heading"
to pursue [target-prey]
  set has-pursued 1
  let next-heading-change subtract-headings desired-heading heading ;; set the next heading change to the difference between the current heading and the heading directly toward the
                                                                    ;; closest prey (a negative number indicates a left turn, while a positive number indicates a right turn)

  ifelse abs(next-heading-change) < ACCEPTABLE-HEADING-OFFSET ;; is the difference between these two headings is less than 0.5 degrees (i.e. is the predator is already going in the right direction)?
  [ ;; if yes, go straight
    set desired-velocity max-velocity-patches ;; the predator should always try to run at maximum speed when pursuing prey
    accelerate

    ifelse reaction-counter = 0
    [ ;; if the predator is going in the right direction and CAN turn
      set new-ideal-heading towards target-prey ;; the predator should always want to move directly towards the prey
    ]
    [ ;; if the predator is going in the right direction and CANNOT turn
      set new-ideal-heading heading
    ]
    set new-desired-position patch-at-heading-and-distance towards target-prey distance target-prey ;; the predator's desired position is always the patch the prey is currently
                                                                                                    ;; occupying
    calculate-desired-heading
    move desired-velocity
  ]

  [ ;; if the difference between the current heading and the desired heading is MORE than (or equal to) 0.5 degrees
    ifelse 0.2 >= (REACTION-TIME / DT)
    [ ;; if the predator needs to turn and CAN
      turn next-heading-change ;; try to turn toward the desired heading
    ]
    [ ;; if the predator needs to turn but CANNOT
      move desired-velocity
    ]
  ]

  if any? prey in-radius kill-dist ;; is the target prey within the kill distance?
  [
    ask target-prey [ set color black ] ;; if yes, then kill it
  ]
end



;**************** TO RANDOM-HEADING ****************
;; Generates a random desired heading for the turtle; calls "calculate-desired-heading"; called by "move"
to random-heading
  set new-ideal-heading heading + (-1) ^ (random 2) * (45 + random-float(135 - 45)) ;; sets a random ideal heading between 45 and 135 degrees from current heading in either
                                                                                    ;; direction (the turtle wants to make a reasonably sized turn, but not turn around completely,
                                                                                    ;; hence the range)
  set ideal-heading-gain 1 ;; make sure ideal heading gain is set to 1
  calculate-desired-heading
end



;**************** TO NEW-RANDOM-TARGET-PATCH ****************
;; If necessary, selects a new random target patch from the set of neutral patches for the turtle. Calls "calculate-desired-heading"; called by "stay" and "search"
to new-random-target-patch
  if patch-here = desired-position or random-float 1.0 < DT / TIME-TO-TARGET ;; is the turtle at the target patch, or has the turtle been trying to get to the target patch for
                                                                             ;; longer than TIME-TO-TARGET?
  [ ;; if yes
    let new-target 0 ;; we don't want the turtle to select the same random patch that it currently has selected, so create a temporary variable "new-target"

    ask patch-here ;; ask the current patch
    [
      set new-target one-of other neutral-patches ;; set new-target to one of the other neutral patches
    ]

    set new-desired-position new-target ;; then set the new-desired-position (target patch) to this new target
    calculate-desired-heading
  ]
end



;**************** TO NEW-TARGET-PATCH ****************
;; If necessary, selects a new target patch from the set of target patches for the turtle. Calls "calculate-desired-heading"; called by "move"
to new-target-patch
  if patch-here = desired-position or random-float 1.0 < DT / TIME-TO-TARGET ;; is the turtle at the target patch, or has the turtle been trying to get to the target patch for
                                                                             ;; longer than TIME-TO-TARGET?
  [ ;; if yes
    let new-target 0 ;; we don't want the turtle to select the same target patch that it currently has selected, so create a temporary variable "new-target"

    ask patch-here ;; ask the current patch
    [
      ifelse any? other target-patches ;; are there any target-patches?
      [ ;; if yes
        set new-target one-of other target-patches ;; set new-target to one of the other target patches
      ]
      [ ;; if no
        set new-target one-of other neutral-patches ;; set new-target to one of the other neutral patches
      ]
    ]
    set new-desired-position new-target ;; then set desired position (target patch) to this new target
    calculate-desired-heading
  ]
end



;**************** TO CALCULATE-DESIRED-HEADING ****************
;; Calculates the desired heading, taking into account the way the turtle would like to go (ideal heading), the repulsive forces of the obstacles, and the attractive forces of the
;; refuges (if applicable); calls "x-forces" and "y-forces"; called by "go", "forage", "move", "stalk", "pursue", and "random-heading"
to calculate-desired-heading

  ;; set initial repulsion and attraction forces for obstacles and refuges to zero
  let x-repulsion-force 0 ;; x repulsion forces (for obstacles)
  let y-repulsion-force 0 ;; y repulsion forces (for obstacles)
  let x-attraction-force 0 ;; x attraction forces (for refuges/target patches)
  let y-attraction-force 0 ;; y attraction forces (for refuges/target patches)
  let ideal-x 0 ;; x component of the ideal heading
  let ideal-y 0 ;; y component of the ideal heading
  let target-x 0 ;; x component of the heading toward the target patch
  let target-y 0 ;; y component of the heading toward the target patch

  set ideal-heading new-ideal-heading
  set desired-position new-desired-position

  ;; set the x and y vectors for the ideal heading
  set ideal-x ideal-heading-gain * sin(ideal-heading)
  set ideal-y ideal-heading-gain * cos(ideal-heading)

  ;; set the x and y vectors for the heading toward target patch
  set target-x target-gain * sin(towards desired-position)
  set target-y target-gain * cos(towards desired-position)

  ifelse any? obstacles ;; are there any obstacles?
  [ ;; if yes
    set x-repulsion-force (obstacle-sensitivity / (1 - obstacle-sensitivity)) * sum [ x-forces myself 1 4 ] of obstacles
    set y-repulsion-force (obstacle-sensitivity / (1 - obstacle-sensitivity)) * sum [ y-forces myself 1 4 ] of obstacles

    if behaviour = "escape"
    [ ;; if the turtle is escaping
      if any? refuges ;; are there any refuges?
      [ ;; if yes, choose the closest one and calculate its attractive force
        let nearest-refuge min-one-of refuges [ distance myself ]
        set x-attraction-force (REFUGE-ATTRACTION-MULTIPLIER * [ x-forces myself 1 2 ] of nearest-refuge) ;; changing the first factor alters the strength of the attractive force exerted by the refuges
        set y-attraction-force (REFUGE-ATTRACTION-MULTIPLIER * [ y-forces myself 1 2 ] of nearest-refuge) ;; a larger factor results in a stronger attractive force
      ]
    ]

  ]
  [ ;; if there are no obstacles
    if behaviour = "escape" and any? refuges ;; are there any refuges?
    [ ;; if yes
      let nearest-refuge min-one-of refuges [ distance myself ]
      set x-attraction-force (REFUGE-ATTRACTION-MULTIPLIER * [ x-forces myself 1 2 ] of nearest-refuge)
      set y-attraction-force (REFUGE-ATTRACTION-MULTIPLIER * [ y-forces myself 1 2 ] of nearest-refuge)
    ]
  ]


  set desired-heading atan (ideal-x + target-x + x-repulsion-force - x-attraction-force) (ideal-y + target-y + y-repulsion-force - y-attraction-force)
  set ideal-heading-gain ideal-heading-gain * ideal-heading-decay
end


;**************** TO GET-AWAY-FROM-PREDATORS ****************
;; Reports the prey's ideal escape heading based on the visible predators (calculates the mean location of all visible predators, and selects the opposite direction); called by
;; "go" and "move"
to get-away-from-predators [ visible-predators ]
  ;; get the x and y coordinates of the visible predators and choose a suitable escape heading away from them
  let predx ([ xcor ] of visible-predators) ;; get the x coordinates of the visible predators in list form
  let predy ([ ycor ] of visible-predators) ;; get the y coordinates of the visible predators in list form
  let xcoord ((sum predx) / (count visible-predators)) ;; take the mean of the x coordinates of the visible predators
  let ycoord ((sum predy) / (count visible-predators)) ;; take the mean of the y coordinates of the visible predators
  set new-ideal-heading ((towards patch-at xcoord ycoord) + 180) ;; set ideal heading to be directly away from the patch comprised of the mean x and y coordinates of the visible predators
end


;**************** TO-REPORT X-FORCES ****************
;; Reports the x-component of the force acting on the turtle from the agent the reporter is passed; called by "calculate-desired-heading"
to-report x-forces [ agent gain exponent ]
  let force gain / (distance agent) ^ exponent ;; the force gets weaker the further away the agent is from the turtle
  report force * sin(towards agent)
end


;**************** TO-REPORT Y-FORCES ****************
;; Reports the y-component of the force acting on the turtle from the agent the reporter is passed; called by "calculate-desired-heading"
to-report y-forces [ agent gain exponent ]
  let force gain / (distance agent) ^ exponent ;; the force gets weaker the further away the agent is from the animal
  report force * cos(towards agent)
end


;**************** TO CIRCLE-THE-OBSTACLE ****************
;; If the turtle crashes into an obstacle, makes the turtle circle the obstacle for a random time then run away; uses user-defined settings "time-spent-circling"; called by "go"
;; and "move"; calls "accelerate", "calculate-desired-heading", and "move"
to circle-the-obstacle

  let exit-circle? false

  ifelse random-float 1.0 > DT / time-spent-circling ;; has the turtle been circling the obstacle for longer than the user-defined time-spent-circling?
  [ ;; if no
    accelerate
    set target-gain 0

    let dist (current-velocity * DT ) ;; set the distance we want the turtle to move
    let theta 2 * asin(dist / (2 * ( circling-radius) )) ;; set the minimum angle the turtle can turn at it's current velocity
    let new-heading (heading + circling-direction * theta / 2) ;; set the new heading appropriately
    let new-heading-circle subtract-headings new-heading heading ;; set the heading change to add to pursuit-angle
    set pursuit-angle pursuit-angle + abs new-heading-circle ;; add this heading change to pursuit-angle

    set heading new-heading ;; set the new heading
    fd (dist)

    ifelse [ pcolor = brown ] of patch-here
    [
      move-to min-one-of neutral-patches [ distance myself ]
      set heading (heading - circling-direction * theta / 2)
      set exit-circle? true
    ]
    [
    set heading (heading + circling-direction * theta / 2)
    ]
  ]
  [
    set exit-circle? true
  ]

  if exit-circle?
  [
    set behaviour "escape"
    set target-gain 1
    set ideal-heading (180 + towards min-one-of obstacles [ distance myself ])
    calculate-desired-heading
    move max-velocity-patches
  ]
end



;**************** TO TURN ****************
;; If the turtle is at a point in it's stride at which it can turn, turns the turtle towards it's desired heading without violating its agility constraints; uses user-defined
;; paramters "predator-agility" and "prey-agility"; calls "accelerate"; called by "move" and "pursue"
to turn [ next-heading-change ]
  ifelse current-velocity > 0
  [ ;; if the current velocity is GREATER THAN zero

      let radius ((current-velocity / agility-patches) ^ 2 );; calculate the smallest radius the prey can turn on at its current velocity
      let dist (current-velocity * DT) ;; calculate the distance it will travel in DT at its current velocity
      let theta (dist / radius) ;; calculate the largest angle it can turn in DT at its radius and velocity

      if abs(next-heading-change) > theta ;; if the desired heading change (without direction) is greater than theta, then the prey cannot make the desired turn
      [
        set next-heading-change ( abs(next-heading-change) * (theta / next-heading-change) ) ;; the prey instead turns theta in the desired direction
        set desired-velocity ( 1 - 0.5 * DT ) * current-velocity
      ]
  ]
  [ ;; if the current velocity is EQUAL TO zero, the animal can make any turn of any radius, so next-heading-change (whatever it is) is doable
    set next-heading-change next-heading-change
  ]

  accelerate

  if behaviour = "escape" or behaviour = "pursue"
  [
    set pursuit-angle pursuit-angle + abs next-heading-change
  ]
  set heading (heading + next-heading-change) ;; set heading to the current heading plus the next-heading-change
  forward (current-velocity * DT) ;; move forward at current velocity
end


;**************** TO-REPORT IS-VISIBLE? ****************
;; Reports whether the "detectee" is visible given the "detector" agent's heading, current speed, and vision cone, as well as the "detectee" agent's current speed; called by "go"
;; and "move"
to-report is-visible? [ detector detector-heading detector-speed detector-max-speed detector-vision-angle detector-vision-dist detectee-speed detectee-distance detector-obstacles ]
  let angular-dist subtract-headings detector-heading (180 + towards myself) ;; calculate the angular distance (in degrees) between the detector and the detectee
  ifelse abs( angular-dist ) < (detector-vision-angle / 2) ;; is the detectee within the detector's vision cone?
  [ ;; if yes

    ifelse [ behaviour = "pursue" ] of detector
    [ ;; if the detector is a pursuing predator
      set conspicuous 1 ;; the prey is conspicuous as the predator already knows where it is
    ]
    [ ;; the detector is NOT a pursuing predator, calculate conspicuousness based on relative speeds of the detector and the detectee
      let alpha 0.1
      let beta 4
      set conspicuous (alpha * exp(detectee-speed * beta)) ;; calculate the conspicuousness of the detectee
    ]
    let max-search-rate ((detector-max-speed * (2 * detector-vision-dist * sin(detector-vision-angle / 2)))) ;; calculate the maximum search rate the detector can employ
    let search-rate (detector-speed * (2 * detector-vision-dist * sin(detector-vision-angle / 2))) ;; calculate the current search rate of the detector
    let prob-detect ((1 - (search-rate / max-search-rate) ^ conspicuous) ^ (1 / conspicuous)) ;; calculate the probability that the detector will detect
                                                                                              ;; the detectee (Gendron & Staddon 1983)
    ifelse prob-detect > 0 ;; is the probability of detection greater than zero?
    [ ;; if yes (probability of being detected is greater than zero)
      ifelse random-float 1.0 <= prob-detect ;; the larger prob-detect is, the less likely it will be smaller than a random float between 0 and 1
      [
        report true
      ]
      [
        report false
      ]
    ]
    [ ;; if no (probability of being detected is zero)
      report false
    ]
  ]
  [ ;; if not within the vision angle
    report random-float 1.0 <= (angular-dist - (vision-angle / 2)) / (180 - (vision-angle / 2)) ;; let there be some semi-random chance of detection
  ]
end

;**************** CHECK-VISIBILITY? ***************
;; Used to shade vision cones as the turtles move around the world; called by "go" and "move" (if not commented out - slows down simulation time considerably)
to check-visibility [agent-heading agent-here agent-vision-dist agent-vision-angle]
  ifelse self = [ patch-here ] of agent-here
  [ set is-patch-visible? true ]
  [
    let angular-dist subtract-headings agent-heading (180 + towards myself)
    ifelse abs(angular-dist) < (agent-vision-angle / 2) AND distance agent-here <= agent-vision-dist
    [ set is-patch-visible? true ]
    [ set is-patch-visible? false ]
  ]
end


;**************** ACCELERATE ****************
;; Accelerates or decelerates (if required) the turtle toward it's desired velocity; called by "move", "pursue", "circle-the-obstacle" and "turn"
to accelerate

  ;; create a temporary variable, m-velocity, which is intially set to the turtle's maximum velocity
  let m-velocity max-velocity-patches

  ;; if the turtle is cirling, m-velocity (the maximum velocity the turtle can run at on the circle) will depend on it's agility
  if behaviour = "circle"
  [
    set m-velocity min(list max-velocity-patches (agility-patches * sqrt(circling-radius)))
  ]

  if current-velocity != desired-velocity ;; if the current velocity is not equal to the desired velocity, then the animal should accelerate or decelerate
  [

    ifelse current-velocity < desired-velocity ;; is the current velocity less than the desired velocity?
    [ ;; if yes, the animal needs to accelerate toward desired velocity
      set current-velocity min(list (current-velocity + acceleration-patches * DT) desired-velocity)
      if current-velocity > m-velocity ;; BUT the turtle cannot accelerate above it's maximum velocity
      [
        set current-velocity m-velocity
      ]
    ]
    [ ;; if no, then current velocity is HIGHER than desired velocity, so animal should decelerate toward desired velocity
      set current-velocity max(list (current-velocity + deceleration-patches * DT) desired-velocity )
      if current-velocity < 0 ;; BUT the turtle cannot decelerate below 0 m/s
      [
        set current-velocity 0
      ]
    ]
  ]

  set current-velocity current-velocity ;; this applies even if the current velocity DOES equal the desired-velocity
end



;**************** TO UPDATE PREY COLOUR ****************
;; Updates the colour of the prey as its behaviour changes; called by "go"
to update-prey-color
  if behaviour = "forage" [ set color orange pen-up ]
  if foraging-behaviour = "cryptic" [ set color magenta pen-up ]
  if behaviour = "exhausted" [ set color yellow pen-up ]
  if behaviour = "circle" [ set color green pen-down ]
  if behaviour = "escape" [ set color pink  pen-down]
end

;**************** TO UPDATE PREDATOR COLOUR ****************
;; Updates the colour of the predator as its behaviour changes; called by "go"
to update-predator-color
  if behaviour = "search" [ set color white pen-up ]
  if behaviour = "stalk" [ set color grey pen-down ]
  if behaviour = "pursue" [ set color blue pen-down ]
end



;*****************************************************
;******************** REFERENCES *********************
;*****************************************************
;; Alexander R.M., Jayes A.S., Maloiy G.M.O. & Wathuta E.M. (1979) Allometry of the limb bones of mammals from shrews (Sorex) to elephant (Loxodonta). Journal of Zoology, 189, 305-314.
;; Alexander R.M., Jayes A.S., Maloiy G.M.O. & Wathuta E.M. (1981) Allometry of the leg muscles of mammals. Journal of Zoology, 194, 539-552.
;; Garland T. (1983) The relation between maximal running speed and body mass in terrestrial mammals. Journal of Zoology, 199, 157-170.
;; Gendron R.P. & Staddon J.E.R. (1983). Searching for cryptic prey: the effect of search rate. The American Naturalist, 212, 172-186.
;; Wilson R.P., Griffiths I.W., Mills M.G.L., Carbone C., Wilson J.W. & Scantlebury D.M. (2015) Mass enhances speed but diminishes turn capacity in terrestrial pursuit predators. Elife, 4, 18.
@#$#@#$#@
GRAPHICS-WINDOW
795
10
1227
443
-1
-1
2.81
1
10
1
1
1
0
1
1
1
-75
75
-75
75
0
0
1
ticks
30.0

BUTTON
19
20
82
53
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
89
20
152
53
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

SLIDER
15
206
187
239
time-to-turn
time-to-turn
1
5
3.0
1
1
s
HORIZONTAL

BUTTON
162
20
225
54
NIL
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

TEXTBOX
16
70
166
88
Prey settings
14
25.0
1

TEXTBOX
215
71
365
89
Predator settings
14
0.0
1

SLIDER
15
132
187
165
prey-vision-distance
prey-vision-distance
5
20
15.0
5
1
m
HORIZONTAL

SLIDER
15
166
187
199
prey-vision-angle
prey-vision-angle
120
300
180.0
30
1
degrees
HORIZONTAL

SLIDER
212
134
412
167
predator-vision-distance
predator-vision-distance
5
20
15.0
5
1
m
HORIZONTAL

SLIDER
212
166
412
199
predator-vision-angle
predator-vision-angle
120
300
180.0
30
1
degrees
HORIZONTAL

SLIDER
442
244
639
277
number-of-refuges
number-of-refuges
0
5
1.0
1
1
NIL
HORIZONTAL

SLIDER
441
97
610
130
obstacle-proportion
obstacle-proportion
0
0.2
0.05
0.05
1
NIL
HORIZONTAL

TEXTBOX
444
70
594
88
Habitat settings
14
63.0
1

SLIDER
442
170
638
203
prey-obstacle-sensitivity
prey-obstacle-sensitivity
0.11
0.99
0.99
0.11
1
NIL
HORIZONTAL

SLIDER
442
203
638
236
predator-obstacle-sensitivity
predator-obstacle-sensitivity
0.11
0.99
0.99
0.11
1
NIL
HORIZONTAL

SLIDER
212
209
412
242
predator-exhaustion-distance
predator-exhaustion-distance
10
1010
510.0
100
1
m
HORIZONTAL

SLIDER
441
130
610
163
obstacle-radius
obstacle-radius
0.5
2
1.0
0.5
1
m
HORIZONTAL

SLIDER
442
276
639
309
number-of-target-patches
number-of-target-patches
0
5
0.0
1
1
NIL
HORIZONTAL

SLIDER
17
362
186
395
time-spent-circling
time-spent-circling
1
5
1.0
1
1
s
HORIZONTAL

SLIDER
15
254
187
287
flight-initiation-distance
flight-initiation-distance
5
prey-vision-distance
5.0
1
1
m
HORIZONTAL

SLIDER
15
286
187
319
freeze-distance
freeze-distance
flight-initiation-distance
prey-vision-distance
10.0
5
1
m
HORIZONTAL

SLIDER
16
320
187
353
prey-exhaustion-distance
prey-exhaustion-distance
10
1010
510.0
100
1
m
HORIZONTAL

MONITOR
629
54
784
99
Number of obstacles
number-obstacles
0
1
11

SLIDER
212
241
412
274
kill-distance
kill-distance
0.5
5
1.0
0.5
1
m
HORIZONTAL

MONITOR
629
10
784
55
Proportion obstacles
count patches with [ pcolor = brown ] / count patches
5
1
11

SLIDER
15
94
187
127
prey-limb-length
prey-limb-length
0.1
1.0
0.5
0.05
1
m
HORIZONTAL

SLIDER
213
94
412
127
predator-limb-length
predator-limb-length
0.1
1.0
1.0
0.05
1
m
HORIZONTAL

MONITOR
629
105
783
150
Prey escaped? (1 for yes, 0 for no)
prey-win
0
1
11

@#$#@#$#@
## WHAT IS IT?

The model simulates a terrestrial predator-prey interaction in habitats with varying numbers of obstacles and refuges. It aims to help understand how these habitat features interact with the predator and prey's relative performance capabilities and specific behaviours to determine the outcome of a pursuit. The model tracks whether or not the prey is detected by the predator (and, if so, the time to detection), whether the prey escapes if detected, along with features of a resulting pursuit such as the length of the prey's escape path and the predator's pursuit path, along with the duration of the pursuit.

If using or modifying the model, please cite:
Wheatley, R., Pavlic, T.P., Levy, O. & Wilson, R.S. (2020). Habitat features and performance interact to determine the outcomes of terrestrial predator–prey pursuits. <i>Journal of Animal Ecology</i>, <i>accepted in press</i>.

## HOW IT WORKS

A predator (spider) and prey (mouse) forage slowly in the simulated world, moving around obstacles as they go. When a predator detects a prey, it stalks directly toward it, only accelerating towards maximum speed once it is detected by the prey. If the prey detects the predator, it freezes, and then moves directly away at maximum speed if the predator comes within its flight initiation distance. The prey may also makes semi-random turns in an attempt to outmanouvre the predator. The predator tries to pursue the prey at maximum speed, always attempting to be moving directly towards the prey. If the predator gets close enough to the prey, it will kill it.

Various factors control the movements the predators prey are allowed to make. Each has a defined maximum speed, acceleration, deceleration, and agility that constrain how fast it can move and the turns it is possible for it to make at a given velocity. These performance capabilities can be set manually or parameterised via scaling relationships with limb length.

There is also the option to add obstacles (brown patches, which both predators and prey must avoid), refuges (green patches, which the prey can hide in), and target patches (yellow patches, areas of habitat that are not refuges but may still be safer for the prey by being in located in areas what will allow the prey to make use of any superior performance capabilities relative to the predator). If there are any refuges or target patches, the prey will tend to head toward them. If the prey enters a refuge, it becomes invisible to the predator, and the simulation ends.

## HOW TO USE IT

1. <i>Set the prey's performance and behavioural parameters by moving the sliders under "prey settings".</i> 
<break></break>
<break></break>
	<b>prey-limb-length</b> controls the prey's maximum speed, maximum acceleration and deceleration, and agility (the maximum speed the prey can make a turn of 1 m radius). <break></break>
<break></break>
<b>prey-vision-distance</b> and <b>prey-vision-angle</b> determine how far the prey can see.
<break></break>
	<b>freeze-distance</b> and <b>flight-initiation-distance</b> determine how the prey responds to a predator. The prey will freeze if it sees a predator within it's freeze distance, and flee if it sees a predator within it's flight inititaion distance. These can be set as the same value as the vision distance if you wish the prey to flee as soon as it detects a predator.
<break></break>
	<b>time-to-turn</b> is a behavioural parameter that determines the mean number of seconds between manoeuvrability gambits by the prey during a pursuit. 
<break></break>
	<b>time-spent-circling</b> is another behavioural parameter that determines how long, on average, prey spend circling obstacles they encounter during a pursuit.
<break></break>
	<b>prey-exhaustion-distance</b> determines the length of pursuit the prey can endure before it becomes exhausted. If this exhaustion distance is reached, the prey will stop moving.
<break></break>
<break></break>
2.  <i>Set the predator's performance and behavioural parameters by moving the sliders under "predator settings".</i> 
<break></break>
<break></break>
	<b>predator-limb-length</b>, <b>predator-vision-distance</b>, <b>predator-vision-angle</b>, and <b>predator-exhaustion-distance</b> are the same as above, only for the predator.
<break></break>
	<b>Kill distance</b> is the distance the predator must get within in order to kill the prey.
<break></break>
<break></break>
3. <i>Set the habiat parameters by moving the sliders under "habitat settings".</i>
<break></break>
<break></break>
	<b>obstacle-proportion</b> determines the proportion of patches that are obstacles.
<break></break>
	<b>obstacle-radius</b> sets the mean size of each obstacle.
<break></break>
	<b>prey-obstacle-sensitivity</b> and <b>predator-obstacle-sensitivity</b> determine how much or little the predators and the prey avoid the obstacles.
<break></break>	
	<b>number-of-refuges</b> sets the number of refuges available in the habitat.
<break></break>	
	<b>number-of-target-patches</b> sets the number of target patches available in the habitat.
<break></break>
<break></break>
4. <i>Run the model by clicking "go".</i>
<break></break>
<break></break>
	Watch the predator and the prey move around the world. What happens? Does the prey eventually escape, or not?

## THINGS TO TRY

1. Try adjusting the predator and prey's limb lengths while keeping the other parameter settings the same. How often does the predator capture the prey when it has much longer limbs vs when it has shorter limbs?
<break></break>
<break></break>
2. Try adjusting the habitat parameter settings while keeping the predator and prey's limb lengths constant. How does increasing the number of obstacles or refuges change how often the prey escapes? How do obstacles affect how long the predator takes to detect the prey?

## EXTENDING THE MODEL

The code is judiciously commented to assist the user with understanding what each section of the code does, and facilitate modification for other purposes.

Some interesting modifications would be:

* Incorporating other habitat features that influence performance capabilities, such as snow cover, uneven terrain, variation in surface friction, etc.;
<break></break>
<break></break>
* Extending to include multiple prey and/or predators, of the same or different species (or other taxonomic group);
<break></break>
<break></break>
* Making the prey's escape strategy dependent on the identity and/or preformance capabilities of the predator in question; 
<break></break>
<break></break>
* Making the prey's various anti-predator behaviours dependent on it's distance to cover, familiarity with the terrain, social behaviours of other prey, etc.;
<break></break>
<break></break>
* Making the predator's predation strategy (pursuit vs ambush) dependent on the identity of the predator and/or the composition of the habitat.

## CREDITS AND REFERENCES

The model uses the following papers to parameterise some of the relationships it is based on (referenced appropriately in the code tab):

Alexander R.M., Jayes A.S., Maloiy G.M.O. & Wathuta E.M. (1979) Allometry of the limb bones of mammals from shrews (Sorex) to elephant (Loxodonta). <i>Journal of Zoology</i>, 189, 305-314.

Alexander R.M., Jayes A.S., Maloiy G.M.O. & Wathuta E.M. (1981) Allometry of the leg muscles of mammals. <i>Journal of Zoology</i>, 194, 539-552.

Garland T. (1983) The relation between maximal running speed and body mass in terrestrial mammals. <i>Journal of Zoology</i>, 199, 157-170.

Gendron R.P. & Staddon J.E.R. (1983). Searching for cryptic prey: the effect of search rate. <i>The American Naturalist</i>, 212, 172-186.

Wilson R.P., Griffiths I.W., Mills M.G.L., Carbone C., Wilson J.W. & Scantlebury D.M. (2015) Mass enhances speed but diminishes turn capacity in terrestrial pursuit predators. <i>eLife</i>, 4, 18.
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

mouse side
false
0
Polygon -7500403 true true 38 162 24 165 19 174 22 192 47 213 90 225 135 230 161 240 178 262 150 246 117 238 73 232 36 220 11 196 7 171 15 153 37 146 46 145
Polygon -7500403 true true 289 142 271 165 237 164 217 185 235 192 254 192 259 199 245 200 248 203 226 199 200 194 155 195 122 185 84 187 91 195 82 192 83 201 72 190 67 199 62 185 46 183 36 165 40 134 57 115 74 106 60 109 90 97 112 94 92 93 130 86 154 88 134 81 183 90 197 94 183 86 212 95 211 88 224 83 235 88 248 97 246 90 257 107 255 97 270 120
Polygon -16777216 true false 234 100 220 96 210 100 214 111 228 116 239 115
Circle -16777216 true false 246 117 20
Line -7500403 true 270 153 282 174
Line -7500403 true 272 153 255 173
Line -7500403 true 269 156 268 177

mouse top
true
0
Polygon -7500403 true true 144 238 153 255 168 260 196 257 214 241 237 234 248 243 237 260 199 278 154 282 133 276 109 270 90 273 83 283 98 279 120 282 156 293 200 287 235 273 256 254 261 238 252 226 232 221 211 228 194 238 183 246 168 246 163 232
Polygon -7500403 true true 120 78 116 62 127 35 139 16 150 4 160 16 173 33 183 60 180 80
Polygon -7500403 true true 119 75 179 75 195 105 190 166 193 215 165 240 135 240 106 213 110 165 105 105
Polygon -7500403 true true 167 69 184 68 193 64 199 65 202 74 194 82 185 79 171 80
Polygon -7500403 true true 133 69 116 68 107 64 101 65 98 74 106 82 115 79 129 80
Polygon -16777216 true false 163 28 171 32 173 40 169 45 166 47
Polygon -16777216 true false 137 28 129 32 127 40 131 45 134 47
Polygon -16777216 true false 150 6 143 14 156 14
Line -7500403 true 161 17 195 10
Line -7500403 true 160 22 187 20
Line -7500403 true 160 22 201 31
Line -7500403 true 140 22 99 31
Line -7500403 true 140 22 113 20
Line -7500403 true 139 17 105 10

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

rabbit
false
0
Polygon -7500403 true true 61 150 76 180 91 195 103 214 91 240 76 255 61 270 76 270 106 255 132 209 151 210 181 210 211 240 196 255 181 255 166 247 151 255 166 270 211 270 241 255 240 210 270 225 285 165 256 135 226 105 166 90 91 105
Polygon -7500403 true true 75 164 94 104 70 82 45 89 19 104 4 149 19 164 37 162 59 153
Polygon -7500403 true true 64 98 96 87 138 26 130 15 97 36 54 86
Polygon -7500403 true true 49 89 57 47 78 4 89 20 70 88
Circle -16777216 true false 37 103 16
Line -16777216 false 44 150 104 150
Line -16777216 false 39 158 84 175
Line -16777216 false 29 159 57 195
Polygon -5825686 true false 0 150 15 165 15 150
Polygon -5825686 true false 76 90 97 47 130 32
Line -16777216 false 180 210 165 180
Line -16777216 false 165 180 180 165
Line -16777216 false 180 165 225 165
Line -16777216 false 180 210 210 240

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

spider
true
0
Polygon -7500403 true true 134 255 104 240 96 210 98 196 114 171 134 150 119 135 119 120 134 105 164 105 179 120 179 135 164 150 185 173 199 195 203 210 194 240 164 255
Line -7500403 true 167 109 170 90
Line -7500403 true 170 91 156 88
Line -7500403 true 130 91 144 88
Line -7500403 true 133 109 130 90
Polygon -7500403 true true 167 117 207 102 216 71 227 27 227 72 212 117 167 132
Polygon -7500403 true true 164 210 158 194 195 195 225 210 195 285 240 210 210 180 164 180
Polygon -7500403 true true 136 210 142 194 105 195 75 210 105 285 60 210 90 180 136 180
Polygon -7500403 true true 133 117 93 102 84 71 73 27 73 72 88 117 133 132
Polygon -7500403 true true 163 140 214 129 234 114 255 74 242 126 216 143 164 152
Polygon -7500403 true true 161 183 203 167 239 180 268 239 249 171 202 153 163 162
Polygon -7500403 true true 137 140 86 129 66 114 45 74 58 126 84 143 136 152
Polygon -7500403 true true 139 183 97 167 61 180 32 239 51 171 98 153 137 162

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

squirrel
false
0
Polygon -7500403 true true 87 267 106 290 145 292 157 288 175 292 209 292 207 281 190 276 174 277 156 271 154 261 157 245 151 230 156 221 171 209 214 165 231 171 239 171 263 154 281 137 294 136 297 126 295 119 279 117 241 145 242 128 262 132 282 124 288 108 269 88 247 73 226 72 213 76 208 88 190 112 151 107 119 117 84 139 61 175 57 210 65 231 79 253 65 243 46 187 49 157 82 109 115 93 146 83 202 49 231 13 181 12 142 6 95 30 50 39 12 96 0 162 23 250 68 275
Polygon -16777216 true false 237 85 249 84 255 92 246 95
Line -16777216 false 221 82 213 93
Line -16777216 false 253 119 266 124
Line -16777216 false 278 110 278 116
Line -16777216 false 149 229 135 211
Line -16777216 false 134 211 115 207
Line -16777216 false 117 207 106 211
Line -16777216 false 91 268 131 290
Line -16777216 false 220 82 213 79
Line -16777216 false 286 126 294 128
Line -16777216 false 193 284 206 285

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
NetLogo 6.1.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="DEFAULT SETTINGS" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 001" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <steppedValueSet variable="prey-limb-length" first="0.1" step="0.1" last="1"/>
    <steppedValueSet variable="prey-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 002" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <steppedValueSet variable="prey-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 003" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <steppedValueSet variable="prey-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="freeze-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 004" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <steppedValueSet variable="prey-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="flight-initiation-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 005" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <steppedValueSet variable="prey-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 006" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <steppedValueSet variable="prey-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-to-turn" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 007" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <steppedValueSet variable="prey-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-spent-circling" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 008" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <steppedValueSet variable="prey-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 009" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <steppedValueSet variable="prey-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 010" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <steppedValueSet variable="prey-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 011" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <steppedValueSet variable="prey-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 012" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <steppedValueSet variable="prey-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="kill-distance" first="0.5" step="0.5" last="5"/>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 013" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <steppedValueSet variable="prey-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-proportion" first="0" step="0.05" last="0.2"/>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 014" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <steppedValueSet variable="prey-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-radius" first="0.5" step="0.5" last="2"/>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 015" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <steppedValueSet variable="prey-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 016" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <steppedValueSet variable="prey-limb-length" first="0.7" step="0.1" last="1"/>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 017" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <steppedValueSet variable="prey-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-refuges" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 018" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <steppedValueSet variable="prey-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-target-patches" first="0" step="1" last="5"/>
  </experiment>
  <experiment name="PS 019" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-distance" first="5" step="5" last="30"/>
    <steppedValueSet variable="prey-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 020" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="freeze-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 021" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="flight-initiation-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 022" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 023" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-to-turn" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 024" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-spent-circling" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 025" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 026" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 027" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 028" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 029" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="kill-distance" first="0.5" step="0.5" last="5"/>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 030" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-proportion" first="0" step="0.05" last="0.2"/>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 031" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-radius" first="0.5" step="0.5" last="2"/>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 032" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 033" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 034" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-refuges" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 035" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-target-patches" first="0" step="1" last="5"/>
  </experiment>
  <experiment name="PS 036" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-angle" first="30" step="30" last="330"/>
    <steppedValueSet variable="freeze-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 037" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="flight-initiation-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 038" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 039" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-to-turn" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 040" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-spent-circling" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 041" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 042" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 043" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 044" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 045" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="kill-distance" first="0.5" step="0.5" last="5"/>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 046" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-proportion" first="0" step="0.05" last="0.2"/>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 047" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-radius" first="0.5" step="0.5" last="2"/>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 048" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 049" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 050" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-refuges" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 051" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-target-patches" first="0" step="1" last="5"/>
  </experiment>
  <experiment name="PS 052" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="freeze-distance" first="5" step="5" last="30"/>
    <steppedValueSet variable="flight-initiation-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 053" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="freeze-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 054" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="freeze-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-to-turn" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 055" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="freeze-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-spent-circling" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 056" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="freeze-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 057" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="freeze-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 058" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="freeze-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 059" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="freeze-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 060" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="freeze-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="kill-distance" first="0.5" step="0.5" last="5"/>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 061" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="freeze-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-proportion" first="0" step="0.05" last="0.2"/>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 062" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="freeze-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-radius" first="0.5" step="0.5" last="2"/>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 063" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="freeze-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 064" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="freeze-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 065" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="freeze-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-refuges" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 066" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="freeze-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-target-patches" first="0" step="1" last="5"/>
  </experiment>
  <experiment name="PS 067" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="flight-initiation-distance" first="5" step="5" last="30"/>
    <steppedValueSet variable="prey-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 068" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="flight-initiation-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-to-turn" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 069" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="flight-initiation-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-spent-circling" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 070" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="flight-initiation-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 071" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="flight-initiation-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 072" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="flight-initiation-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 073" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="flight-initiation-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 074" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="flight-initiation-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="kill-distance" first="0.5" step="0.5" last="5"/>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 075" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="flight-initiation-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-proportion" first="0" step="0.05" last="0.2"/>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 076" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="flight-initiation-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-radius" first="0.5" step="0.5" last="2"/>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 077" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="flight-initiation-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 078" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="flight-initiation-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 079" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="flight-initiation-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-refuges" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 080" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="flight-initiation-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-target-patches" first="0" step="1" last="5"/>
  </experiment>
  <experiment name="PS 081" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-exhaustion-distance" first="10" step="100" last="1010"/>
    <steppedValueSet variable="time-to-turn" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 082" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-spent-circling" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 083" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 084" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 085" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 086" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 087" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="kill-distance" first="0.5" step="0.5" last="5"/>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 088" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-proportion" first="0" step="0.05" last="0.2"/>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 089" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-radius" first="0.5" step="0.5" last="2"/>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 090" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 091" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 092" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-refuges" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 093" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-target-patches" first="0" step="1" last="5"/>
  </experiment>
  <experiment name="PS 094" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-to-turn" first="1" step="1" last="5"/>
    <steppedValueSet variable="time-spent-circling" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 095" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-to-turn" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 096" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-to-turn" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 097" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-to-turn" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 098" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-to-turn" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 099" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-to-turn" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="kill-distance" first="0.5" step="0.5" last="5"/>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 100" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-to-turn" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-proportion" first="0" step="0.05" last="0.2"/>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 101" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-to-turn" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-radius" first="0.5" step="0.5" last="2"/>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 102" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-to-turn" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 103" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-to-turn" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 104" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-to-turn" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-refuges" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 105" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-to-turn" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-target-patches" first="0" step="1" last="5"/>
  </experiment>
  <experiment name="PS 106" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-spent-circling" first="1" step="1" last="5"/>
    <steppedValueSet variable="predator-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 107" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-spent-circling" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 108" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-spent-circling" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 109" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-spent-circling" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 110" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-spent-circling" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="kill-distance" first="0.5" step="0.5" last="5"/>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 111" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-spent-circling" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-proportion" first="0" step="0.05" last="0.2"/>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 112" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-spent-circling" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-radius" first="0.5" step="0.5" last="2"/>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 113" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-spent-circling" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 114" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-spent-circling" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 115" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-spent-circling" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-refuges" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 116" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="time-spent-circling" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-target-patches" first="0" step="1" last="5"/>
  </experiment>
  <experiment name="PS 117" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-limb-length" first="0.1" step="0.1" last="1"/>
    <steppedValueSet variable="predator-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 118" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 119" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 120" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="kill-distance" first="0.5" step="0.5" last="5"/>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 121" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-proportion" first="0" step="0.05" last="0.2"/>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 122" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-radius" first="0.5" step="0.5" last="2"/>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 123" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 124" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 125" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-refuges" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 126" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-target-patches" first="0" step="1" last="5"/>
  </experiment>
  <experiment name="PS 127" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-distance" first="5" step="5" last="30"/>
    <steppedValueSet variable="predator-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 128" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 129" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="kill-distance" first="0.5" step="0.5" last="5"/>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 130" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-proportion" first="0" step="0.05" last="0.2"/>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 131" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-radius" first="0.5" step="0.5" last="2"/>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 132" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 133" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 134" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-refuges" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 135" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-distance" first="5" step="5" last="30"/>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-target-patches" first="0" step="1" last="5"/>
  </experiment>
  <experiment name="PS 136" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-angle" first="30" step="30" last="330"/>
    <steppedValueSet variable="predator-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 137" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="kill-distance" first="0.5" step="0.5" last="5"/>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 138" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-proportion" first="0" step="0.05" last="0.2"/>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 139" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-radius" first="0.5" step="0.5" last="2"/>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 140" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 141" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 142" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-refuges" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 143" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-vision-angle" first="30" step="30" last="330"/>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-target-patches" first="0" step="1" last="5"/>
  </experiment>
  <experiment name="PS 144" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-exhaustion-distance" first="10" step="100" last="1010"/>
    <steppedValueSet variable="kill-distance" first="0.5" step="0.5" last="5"/>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 145" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-proportion" first="0" step="0.05" last="0.2"/>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 146" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-radius" first="0.5" step="0.5" last="2"/>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 147" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 148" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 149" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-refuges" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 150" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-exhaustion-distance" first="10" step="100" last="1010"/>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-target-patches" first="0" step="1" last="5"/>
  </experiment>
  <experiment name="PS 151" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="kill-distance" first="0.5" step="0.5" last="5"/>
    <steppedValueSet variable="obstacle-proportion" first="0" step="0.05" last="0.2"/>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 152" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="kill-distance" first="0.5" step="0.5" last="5"/>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-radius" first="0.5" step="0.5" last="2"/>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 153" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="kill-distance" first="0.5" step="0.5" last="5"/>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 154" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="kill-distance" first="0.5" step="0.5" last="5"/>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 155" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="kill-distance" first="0.5" step="0.5" last="5"/>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-refuges" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 156" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <steppedValueSet variable="kill-distance" first="0.5" step="0.5" last="5"/>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-target-patches" first="0" step="1" last="5"/>
  </experiment>
  <experiment name="PS 157" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-proportion" first="0" step="0.05" last="0.2"/>
    <steppedValueSet variable="obstacle-radius" first="0.5" step="0.5" last="2"/>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 158" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-proportion" first="0" step="0.05" last="0.2"/>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 159" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-proportion" first="0" step="0.05" last="0.2"/>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 160" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-proportion" first="0" step="0.05" last="0.2"/>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-refuges" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 161" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-proportion" first="0" step="0.05" last="0.2"/>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-target-patches" first="0" step="1" last="5"/>
  </experiment>
  <experiment name="PS 162" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-radius" first="0.5" step="0.5" last="2"/>
    <steppedValueSet variable="prey-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 163" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-radius" first="0.5" step="0.5" last="2"/>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 164" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-radius" first="0.5" step="0.5" last="2"/>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-refuges" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 165" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-radius" first="0.5" step="0.5" last="2"/>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-target-patches" first="0" step="1" last="5"/>
  </experiment>
  <experiment name="PS 166" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <steppedValueSet variable="predator-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 167" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-refuges" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 168" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="prey-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-target-patches" first="0" step="1" last="5"/>
  </experiment>
  <experiment name="PS 169" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <steppedValueSet variable="number-of-refuges" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 170" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-obstacle-sensitivity" first="0.11" step="0.44" last="0.99"/>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-target-patches" first="0" step="1" last="5"/>
  </experiment>
  <experiment name="PS 171" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-refuges" first="0" step="1" last="5"/>
    <steppedValueSet variable="number-of-target-patches" first="0" step="1" last="5"/>
  </experiment>
  <experiment name="EXPFORPAPER-performance vs refuges" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="240"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <steppedValueSet variable="number-of-refuges" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="EXPFORPAPER-performance vs obstacles" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="240"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="predator-limb-length" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="obstacle-proportion" first="0" step="0.05" last="0.2"/>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 087-one run that didn't finish" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="610"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 098-one run that didn't finish" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="410"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="PS 122-one run that didn't finish" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="18001"/>
    <metric>number-obstacles</metric>
    <metric>count patches with [ pcolor = brown ] / count patches</metric>
    <metric>prey-win</metric>
    <metric>predator-win</metric>
    <metric>detect-time</metric>
    <metric>pursuit-time</metric>
    <metric>sim-time</metric>
    <metric>prey-escape-length</metric>
    <metric>predator-pursuit-length</metric>
    <metric>prey-curviness</metric>
    <metric>predator-curviness</metric>
    <metric>[ behaviour ] of one-of prey</metric>
    <metric>[ behaviour ] of one-of predators</metric>
    <metric>prey-detected</metric>
    <enumeratedValueSet variable="prey-limb-length">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="freeze-distance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="flight-initiation-distance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-to-turn">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="time-spent-circling">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-limb-length">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-distance">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-vision-angle">
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-exhaustion-distance">
      <value value="510"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kill-distance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-proportion">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="obstacle-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="prey-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="predator-obstacle-sensitivity">
      <value value="0.99"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-refuges">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-target-patches">
      <value value="0"/>
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
