;;extension to enable supercool Networking Extension for generating network types
extensions [nw]

;;cite this: 
;;SETUP VARIABLES
;;defines types of people
breed [ generals gen ] ;; general breed before they're assigned something from the following

breed [ rejectors rej ] ;; 5%
breed [ accept-all acca ] ;; 5%, the crackpots who will accept without question
breed [ ambivalents ambi ] ;; 25%
breed [ accept-some accs ] ;; 65% ish


;;variables that each turtle owns
turtles-own [
  ;;personal characteristics of note
  pass-on-misinfo ;; unalterable boolean

  exposed-to-misinfo ;;numeral indicating number of times exposed to rumor, set by set-up
  accept-misinfo ;;boolean true or false ;; set by subroutine

  institutional-trust ;; or mistrust, as the case being
  
  ;;housekeeping stuff for Behavior Space
  originator ;; boolean
  link-neighbor-num

  ]

;;right now we assume that trust is reciprocal
links-own [ 
  trust-value  ;;number
  pass-it-on  ;; integer
  ]

;global variables
globals [
  ;;commented values are adjustable sliders on GUI
  
  ;;total-nodes ;; total number of agents in model  
  ;;misinfo-spreaders-num ;; number of initial believers
  
  ;;institutional-trust-avg ;;average of institutional trust in a population. Set in GUI
  ;;personal-trust-avg ;; median trust in a population. This is set in the GUI
  ;;acceptance-threshold ;; what is the number to accept rumor? Set in GUI
  
  ;;set in simulation
  rejectors-num
  ambivalents-num
  accept-some-num
  accept-all-num

  max-length ;; for internals only. What is the longest link in this network?
  
  ;;avg-links
  ]




;;INITIAL NETWORK SETUP
to setup
  clear-all
  ;;r:clear ;; procures clean new R workspace
  ;;r:eval "library(VGAM)"
  
  setup-network
  setup-nodes
  
  reset-ticks
  
end


;;sets up nodes and then assigns them to a turtle breed
to setup-nodes
  set-default-shape turtles "default" ;;boring circular nodes, I know
  
  ;;temp counter variable
  let temp-turtles-remain total-nodes
  
  ;;create 5% (plus 5% jitter) rejectors
  let temp-rejectors checkbounds ( 0.05 + random-normal 0 0.01 ) 0 1 
  set rejectors-num round ( temp-rejectors * total-nodes )
  
  
  ;; update total turtles left
  set temp-turtles-remain temp-turtles-remain - rejectors-num

  ;; create 5% (plus jitter) crackpots
  let temp-accept-all checkbounds ( 0.05 + random-normal 0 0.01 ) 0 1 
  set accept-all-num round ( temp-accept-all * total-nodes )
 
  ;; update turtle count
  set temp-turtles-remain temp-turtles-remain - accept-all-num
   
  ;; create ~65% accept some turtles
  let temp-accept-some checkbounds (0.7 + random-normal 0 0.01) 0 1 
  set accept-some-num round ( temp-accept-some * temp-turtles-remain )
  
 ;; update turtles
  set temp-turtles-remain temp-turtles-remain - accept-some-num
 
  set ambivalents-num temp-turtles-remain ;;take whatever is left over
  
  ;;specify 5% to reject everything. They won't pass anything on
  ask n-of rejectors-num generals [
    set breed rejectors
    
  ;;no one's heard anything yet, so no one accepts it
    set pass-on-misinfo false
    ;;set corresponding color
    set color red
    
    ]
  
 
    ;; however, accept-alls will always accept the misinformation and will always pass it on
    ask n-of accept-all-num generals [
      set breed accept-all
      set pass-on-misinfo true
    
      ]
    
    
  ;;cunning shortcut to seed initial misinfo spreaders only among ambivalents and accepters
  ask n-of misinfo-spreaders-num generals [
    set exposed-to-misinfo 1
    set accept-misinfo true
    set color pink
    set originator true
    ]
  
  ;;then assign rest of breeds.
   ask n-of ambivalents-num generals [
    set breed ambivalents
    set accept-misinfo false
    ;;no one's heard anything yet, so pft on accepting it 
    set pass-on-misinfo true
    ;;set corresponding color
    set color blue
    ] 
   
   ;;only one group left. Set everyone else as accept-some
 ask generals [
   set breed accept-some
   set pass-on-misinfo true
   ;; the rest are accept-some and remain white
   ]
 
 
 ;;we assigned average institutional-trust. But in this case, what we actually want is institutional MISTRUST, so we take a difference.
   let temp-mistrust ( 100 - institutional-trust-avg )
 
 ;;and then assign a mistrust value to each turtle drawn from a normal distribution centered upon the mistrust value 
  ask turtles [
    
    set institutional-trust checkbounds ( random-normal temp-mistrust 10 ) 0 100
    ;;link-neighbor-calc
    ]
  
end



;; make sure that the number + jitter does not go out of some pre-determined boundary. 

to-report checkbounds [ number lowerbound upperbound]
  if ( number < lowerbound )
  [ report lowerbound ]
  if (number > upperbound )
  [ report upperbound ]
  if ( number > lowerbound and number < upperbound )
  [ report number ]
  
end


to setup-network
  
  ;;create small-world network with parameters
  if ( network-type = "small-world" ) [
    nw:generate-small-world turtles links ( total-nodes / 20 ) 20 2.0 true [
      setxy (random-xcor * 0.95) (random-ycor * 0.95) 
      set breed generals
      set exposed-to-misinfo 0
      set accept-misinfo false
      set color white
      set originator false
      
      
      ]
    
    ]
  
  ;;create network of preferential attachment (scale free)
  
   if ( network-type = "scale-free" ) [
    nw:generate-preferential-attachment turtles links total-nodes [ 
      ;;nb current configuration sets total-nodes - 1 links. can change
      setxy (random-xcor * 0.95) (random-ycor * 0.95) 
      set breed generals
      set exposed-to-misinfo 0
      set accept-misinfo false
      set color white
      set originator false
    ]
  ]
   
   
   if ( network-type = "star" ) [
    nw:generate-preferential-attachment turtles links total-nodes [ 
     
      setxy (random-xcor * 0.95) (random-ycor * 0.95) 
      set breed generals
      set exposed-to-misinfo 0
      set accept-misinfo false
      set color white
      set originator false
    ]
  ]
   
      if ( network-type = "random" ) [
    nw:generate-random turtles links total-nodes 0.1 [ 
      setxy (random-xcor * 0.95) (random-ycor * 0.95) 
      set breed generals
      set exposed-to-misinfo 0
      set accept-misinfo false
      set color white
      set originator false
    ]
  ]
   
 
 ;;I kept track of the longest link here.
 set max-length ( max [ link-length ] of links )  
  
  ;;we now assign a trust value to each social connection. As no one has said anything, the rumor counter is 0
  ;;nb while the model keeps track of it, we never used the pass-it-on number
  ask links [     
    trust-calc
    set pass-it-on 0 ;;initially, no one will have given the info to anyone else
    set color 3
  ]
  
end

;;this is how we determine whether we trust a neighbor
;;get distance between neighbors
;;draw trust from N(mean, sd), where sd is the distance between neighbors
;;there is greater variability when you are further away from someone...this may need a little work

to trust-calc
  
  ;; Keep in mind that shorter distances should rate higher because we usually trust those
  ;; closer to us more. We normalize this as a percentage
  ;; We set the max value length depending on the network that is generated
  
  ;; R code
  ;;r:put "normd" 
  ;;r:put "m" 
  ;;r:put "sd" .1 ;; let's not go too crazy with the values
  
  ;;let tmp ( r:get "normd + rnorm(1, m, sd)")
  
  let normd ( 1 - ( link-length / max-length ) )
  let m ( personal-trust-avg / 100 - .5 )
  let sd .1
  
  let tmp ( normd + random-normal m sd ) 
  
  set trust-value ( checkbounds tmp 0 1 ) * 100
  
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;GO METHODS. 
;;This should only spread misinfo through the network
to go
  if all? turtles [ exposed-to-misinfo > 0 ] ;;nb this stop condition may never be reached.
  [ stop ] ;;stop if everyone's tried to pass it to everyone else
  
  if all? links [ pass-it-on > pass-it-on-limit ]
  [ stop ] ;; stop if everyone's tried to pass it on to everyone else. NB may never be reached
  
  ;;otherwise...
  spread-misinfo
  tick
end


;;method for spreading misinfo through the network
;; accept-some who have been exposed to the info but has not accepted the info should not pass it on!!
to spread-misinfo
  ask turtles with [ exposed-to-misinfo > 0 and pass-on-misinfo ] 
  ;; ask a turtle if he's heard the info AND can pass it on, so not rejectors
  [
    let target one-of my-links with [ pass-it-on < pass-it-on-limit ]
    if target != nobody  
    [
      ;; ask if they've passed on info yet. This may be modified. 
      ;; after all, you may hear something more than once
      ask target 
      [
        let current-source-trust ( [ trust-value ]  of self )
        ask other-end 
        [
          if ( not accept-misinfo )
          [ 
            hear-misinfo 
            if ( breed = accept-all )
            [ set accept-misinfo true ] ;; crackpots always believe!
            if ( breed = accept-some ) ;; note that we exclude ambivalents here. They listen but will never believe
            [ convince-misinfo current-source-trust ]
          ]
          
        ]

        set pass-it-on ( pass-it-on + 1 )       
 
      ]
        
    ] 
  ] 

end

;;method for accepting misinfo
to hear-misinfo 
  set exposed-to-misinfo ( exposed-to-misinfo + 1 )
end

to convince-misinfo [ trustvalue ] 
  ;; simple additive version: if your averaged personal trust and institutional-mistrust 
  ;; are over the threshold, accept that sucker!
  
   let p trustvalue + institutional-trust 
     if ( p >= ( acceptance-threshold * 2) ) [ 
    set accept-misinfo true
    set color pink 
    ]

  ;;previous version. Note p * 0.01 is correct term
  ;;r:put "prob" p 
  ;;r:put "num" exposed-to-misinfo  
  ;;let threshold ( r:get "dlog(num, prob)" ) 
 

end
@#$#@#$#@
GRAPHICS-WINDOW
270
10
698
459
16
16
12.67
1
10
1
1
1
0
1
1
1
-16
16
-16
16
1
1
1
ticks
30.0

BUTTON
30
40
96
73
Setup
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

SLIDER
27
80
217
113
total-nodes
total-nodes
100
1500
1500
100
1
NIL
HORIZONTAL

SLIDER
25
215
220
248
misinfo-spreaders-num
misinfo-spreaders-num
1
10
10
1
1
NIL
HORIZONTAL

BUTTON
150
40
213
73
Go
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

CHOOSER
30
120
220
165
network-type
network-type
"small-world" "scale-free" "star" "random"
1

SLIDER
25
340
220
373
acceptance-threshold
acceptance-threshold
0
100
75
20
1
NIL
HORIZONTAL

SLIDER
25
175
220
208
pass-it-on-limit
pass-it-on-limit
1
5
1
1
1
NIL
HORIZONTAL

SLIDER
25
295
220
328
personal-trust-avg
personal-trust-avg
0
100
75
25
1
NIL
HORIZONTAL

SLIDER
25
255
220
288
institutional-trust-avg
institutional-trust-avg
0
100
75
25
1
NIL
HORIZONTAL

PLOT
720
15
920
165
plot 1
Time
Rumor Belief
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles with [ accept-misinfo = true ]"

@#$#@#$#@
## WHAT IS IT?

This is the ABM model for "Agent-based modeling (ABM) and the Dissemination of
Erroneous Information: A viral explanation of rumor propagation". It was formerly known as "Did Hillary Clinton throw a lamp or did Harry Reid get into a fistfight? Agent-based modeling (ABM) and the Dissemination of Erroneous Information", authors Wenshuo Zhang, Amanda Cronkhite, and Leslie Caughell, and was presented at the 2015 annual conference of the Midwestern Political Science Association. 

Abstract: To understand the spread of misinformation, political scientists must consider both individual- and community-level factors inﬂuencing a rumor’s movement. Adapting an SIR epidemic model used in medicine, we use agent-based modeling (ABM) to simulate how misinformation spreads in diﬀerent ideal network types. Our results indicate that both network type and composition are critical to the time and extent to which a rumor penetrates in a given community. More importantly, our ﬁndings show that, in certain network conﬁgurations, such as that comprised by Twitter, well-connected agents predisposed to reject information can potentially stop the spread of a rumor. Such “rejectors” are likely not equally distributed across the population though, potentially leaving society’s most informationally vulnerable demographics to form political preferences with both insuﬃcient and inaccurate information. To the extent that good political decisions rest on accurate information, an accepted tenet of normative democratic theory, this poses a substantial problem for democracy.

## HOW IT WORKS

We derive the basic logic to model how misinformation spreads from the SIR epidemic model, which models how a community is infected with a virus. The numbers for immunity, infection and susceptibility are derived from Adam Berinsky's research on misinformation (2012). To quote from it:  "[his] analysis ﬁnds that a majority of people (65%) will believe at least one rumor, with a smaller percentage (25%) ambivalent and small contingents who will always accept (5%) or reject (5%) a rumor." 


## HOW TO USE IT

The different sliders are adjustable parameters in the model, including:

1. total-nodes: total number of turtles in the community
2. network-type: how many connections do turtles form amongst themselves? The choices are small-world, scale-free, star, and random. Please note that our paper only examines Small World and Scale-Free networks
3. pass-it-on-limit: how many times will an infected turtle/believer try to pass on the rumor? This is included in a previous version as we wanted to test differential exposure, but it was only set to 1, or once, in this model. Basically, people will try to tell others only once. In other words, if a listener does not believe the first time, then the speaker will stop trying. 
4. misinfo-spread-num: how many believers/spreaders are there at first?
5. institutional-trust-avg: how much do people trust, on average, their governments and politicians?
6. personal-trust-avg: how much, on average, do people trust their social connections?
7. acceptance-threshold: are people generally more likely or less likely to believe something? We provide low-medium-high acceptance thresholds.

## THINGS TO NOTICE

We provide a graph to plot the percentage of people who believe a rumor over time. The percentage may be a little off, as it only plot the number of believers over the number of people who *can* believe a rumor (nb rejectors will never believe).

For educational purposes, the picture of the community is more interesting. You can use it to visualize the saturation of believers in different types of communities over time.

## EXTENDING THE MODEL

Right now everything, where applicable, uses a normal distribution. If you have a NetLogo-to-R extension installed, you could explore the effect that drawing from other statistical distributions for trust may have on your network. When we tried it, it kept crashing our poor machine.

Second, we tried to take guidance from existing work on political trust - both individual and institutional - to specify how levels of trust varies in a community. I'm sure you could do better!

## NETLOGO FEATURES

The network extension is required and is explicitly installed at the beginning. Don't take that line off!

## CREDITS AND REFERENCES

You may find the code, simulation data and the paper at https://github.com/ZhangWS/mpsa2015. Prof Leslie Caughell is the corresponding author (please see manuscript pdf for contact information).
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
NetLogo 5.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="small world full mpsa" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>count turtles</metric>
    <metric>count links</metric>
    <metric>count links with [ pass-it-on &gt; 0 ]</metric>
    <metric>mean [ count link-neighbors ] of turtles with [ originator ]</metric>
    <metric>mean [ trust-value ] of links</metric>
    <metric>mean [ institutional-trust ] of turtles</metric>
    <metric>count ambivalents</metric>
    <metric>mean [ count link-neighbors ] of ambivalents</metric>
    <metric>mean [ nw:clustering-coefficient ] of ambivalents</metric>
    <metric>mean [ nw:betweenness-centrality ] of ambivalents</metric>
    <metric>mean [ nw:eigenvector-centrality ] of ambivalents</metric>
    <metric>mean [ nw:page-rank ] of ambivalents</metric>
    <metric>mean [ nw:closeness-centrality ] of ambivalents</metric>
    <metric>mean [ institutional-trust ] of ambivalents</metric>
    <metric>count rejectors</metric>
    <metric>mean [ count link-neighbors ] of rejectors</metric>
    <metric>mean [ nw:clustering-coefficient ] of rejectors</metric>
    <metric>mean [ nw:betweenness-centrality ] of rejectors</metric>
    <metric>mean [ nw:eigenvector-centrality ] of rejectors</metric>
    <metric>mean [ nw:page-rank ] of rejectors</metric>
    <metric>mean [ nw:closeness-centrality ] of rejectors</metric>
    <metric>mean [ institutional-trust ] of rejectors</metric>
    <metric>count accept-all</metric>
    <metric>mean [ count link-neighbors ] of accept-all</metric>
    <metric>mean [ nw:clustering-coefficient ] of accept-all</metric>
    <metric>mean [ nw:betweenness-centrality ] of accept-all</metric>
    <metric>mean [ nw:eigenvector-centrality ] of accept-all</metric>
    <metric>mean [ nw:page-rank ] of accept-all</metric>
    <metric>mean [ nw:closeness-centrality ] of accept-all</metric>
    <metric>mean [ institutional-trust ] of accept-all</metric>
    <metric>count accept-some</metric>
    <metric>mean [ count link-neighbors ] of accept-some</metric>
    <metric>mean [ nw:clustering-coefficient ] of accept-some</metric>
    <metric>mean [ nw:betweenness-centrality ] of accept-some</metric>
    <metric>mean [ nw:eigenvector-centrality ] of accept-some</metric>
    <metric>mean [ nw:page-rank ] of accept-some</metric>
    <metric>mean [ nw:closeness-centrality ] of accept-some</metric>
    <metric>mean [ institutional-trust ] of accept-some</metric>
    <metric>count turtles with [ accept-misinfo ]</metric>
    <metric>mean [ exposed-to-misinfo ] of turtles</metric>
    <enumeratedValueSet variable="total-nodes">
      <value value="125"/>
      <value value="500"/>
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="misinfo-spreaders-num">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;small-world&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="personal-trust-avg">
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pass-it-on-limit">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acceptance-threshold">
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-trust-avg">
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="scale-free full run" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>count turtles</metric>
    <metric>count links</metric>
    <metric>count links with [ pass-it-on &gt; 0 ]</metric>
    <metric>mean [ count link-neighbors ] of turtles with [ originator ]</metric>
    <metric>mean [ trust-value ] of links</metric>
    <metric>mean [ institutional-trust ] of turtles</metric>
    <metric>count ambivalents</metric>
    <metric>mean [ count link-neighbors ] of ambivalents</metric>
    <metric>mean [ nw:clustering-coefficient ] of ambivalents</metric>
    <metric>mean [ nw:betweenness-centrality ] of ambivalents</metric>
    <metric>mean [ nw:eigenvector-centrality ] of ambivalents</metric>
    <metric>mean [ nw:page-rank ] of ambivalents</metric>
    <metric>mean [ nw:closeness-centrality ] of ambivalents</metric>
    <metric>mean [ institutional-trust ] of ambivalents</metric>
    <metric>count rejectors</metric>
    <metric>mean [ count link-neighbors ] of rejectors</metric>
    <metric>mean [ nw:clustering-coefficient ] of rejectors</metric>
    <metric>mean [ nw:betweenness-centrality ] of rejectors</metric>
    <metric>mean [ nw:eigenvector-centrality ] of rejectors</metric>
    <metric>mean [ nw:page-rank ] of rejectors</metric>
    <metric>mean [ nw:closeness-centrality ] of rejectors</metric>
    <metric>mean [ institutional-trust ] of rejectors</metric>
    <metric>count accept-all</metric>
    <metric>mean [ count link-neighbors ] of accept-all</metric>
    <metric>mean [ nw:clustering-coefficient ] of accept-all</metric>
    <metric>mean [ nw:betweenness-centrality ] of accept-all</metric>
    <metric>mean [ nw:eigenvector-centrality ] of accept-all</metric>
    <metric>mean [ nw:page-rank ] of accept-all</metric>
    <metric>mean [ nw:closeness-centrality ] of accept-all</metric>
    <metric>mean [ institutional-trust ] of accept-all</metric>
    <metric>count accept-some</metric>
    <metric>mean [ count link-neighbors ] of accept-some</metric>
    <metric>mean [ nw:clustering-coefficient ] of accept-some</metric>
    <metric>mean [ nw:betweenness-centrality ] of accept-some</metric>
    <metric>mean [ nw:eigenvector-centrality ] of accept-some</metric>
    <metric>mean [ nw:page-rank ] of accept-some</metric>
    <metric>mean [ nw:closeness-centrality ] of accept-some</metric>
    <metric>mean [ institutional-trust ] of accept-some</metric>
    <metric>count turtles with [ accept-misinfo ]</metric>
    <metric>mean [ exposed-to-misinfo ] of turtles</metric>
    <enumeratedValueSet variable="total-nodes">
      <value value="125"/>
      <value value="500"/>
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="misinfo-spreaders-num">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;scale-free&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="personal-trust-avg">
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pass-it-on-limit">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acceptance-threshold">
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-trust-avg">
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="random full run" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="500"/>
    <metric>count turtles</metric>
    <metric>count links</metric>
    <metric>mean [ count link-neighbors ] of turtles with [ originator ]</metric>
    <metric>count ambivalents</metric>
    <metric>mean [ count link-neighbors ] of ambivalents</metric>
    <metric>mean [ nw:clustering-coefficient ] of ambivalents</metric>
    <metric>mean [ nw:betweenness-centrality ] of ambivalents</metric>
    <metric>mean [ nw:eigenvector-centrality ] of ambivalents</metric>
    <metric>mean [ nw:page-rank ] of ambivalents</metric>
    <metric>mean [ nw:closeness-centrality ] of ambivalents</metric>
    <metric>count rejectors</metric>
    <metric>mean [ count link-neighbors ] of rejectors</metric>
    <metric>mean [ nw:clustering-coefficient ] of rejectors</metric>
    <metric>mean [ nw:betweenness-centrality ] of rejectors</metric>
    <metric>mean [ nw:eigenvector-centrality ] of rejectors</metric>
    <metric>mean [ nw:page-rank ] of rejectors</metric>
    <metric>mean [ nw:closeness-centrality ] of rejectors</metric>
    <metric>count accept-all</metric>
    <metric>mean [ count link-neighbors ] of accept-all</metric>
    <metric>mean [ nw:clustering-coefficient ] of accept-all</metric>
    <metric>mean [ nw:betweenness-centrality ] of accept-all</metric>
    <metric>mean [ nw:eigenvector-centrality ] of accept-all</metric>
    <metric>mean [ nw:page-rank ] of accept-all</metric>
    <metric>mean [ nw:closeness-centrality ] of accept-all</metric>
    <metric>count accept-some</metric>
    <metric>mean [ count link-neighbors ] of accept-some</metric>
    <metric>mean [ nw:clustering-coefficient ] of accept-some</metric>
    <metric>mean [ nw:betweenness-centrality ] of accept-some</metric>
    <metric>mean [ nw:eigenvector-centrality ] of accept-some</metric>
    <metric>mean [ nw:page-rank ] of accept-some</metric>
    <metric>mean [ nw:closeness-centrality ] of accept-some</metric>
    <metric>count turtles with [ accept-misinfo ]</metric>
    <metric>mean [ exposed-to-misinfo ] of turtles</metric>
    <enumeratedValueSet variable="total-nodes">
      <value value="125"/>
      <value value="500"/>
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="misinfo-spreaders-num">
      <value value="1"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="personal-trust-avg">
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pass-it-on-limit">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="acceptance-threshold">
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-trust-avg">
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
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
1
@#$#@#$#@
