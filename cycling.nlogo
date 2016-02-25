;;;;;;;;;;
;; INIT ;;
;;;;;;;;;;

extensions [ 
  nw 
  gis 
  time
  sql
]

globals [ 
  time
  time-str
  last-tick-time
  tick-durations
  tick-duration-mean
  last-spatial-resolution
  last-temporal-resolution
  last-avg-speed
  
  failed-links
  failed-nodes
  edges-dataset
  residents-dataset
  employees-dataset
  status                         ;; reports the current system status to the user
  cnt-cyclists                   ;; total count of all created cyclists
  r-nodes                        ;; temporary helper variable used for route calculation
  r-links                        ;; temporary helper variable used for route calculation
  oneway_TF?                     ;; temporary helper variable used for street-network setup
  oneway_FT?                     ;; temporary helper variable used for street-network setup
  street-restricted?             ;; temporary helper variable used for street-network setup
  street-push?                   ;; temporary helper variable used for street-network setup
  max-residents                  ;; maximum value of the resident grid
  max-employees                  ;; maximum value of the employee grid
  add-returning-cyclists-list    ;; temporary helper list of cyclists that reached their target and need to return
  heatmap-visible?
]

breed [ vertices-scalebar vertex-scalebar ]
breed [ vertices vertex ]
breed [ nav-vertices nav-vertex ]
breed [ cyclists cyclist ]
breed [ counting-stations counting-station ]

undirected-link-breed [scalebars scalebar]
undirected-link-breed [streets street]
directed-link-breed [oneways oneway]
undirected-link-breed [nav-streets nav-street]
directed-link-breed [nav-oneways nav-oneway]

links-own [ 
  name
  restricted?     ;; is this link restricted? = Not allowed for cycling
  push?           ;; Does this link require cyclists to push their bike
  weight          ;; cost of cycling this link
  counter         ;; number of bicycles that have passed this link
]

vertices-own [ 
  die?            ;; bool to mark vertices for deletion
  pathends        ;; contains endnodes of all paths that connect to this vertex
  paths-nodes     ;; contains nodes of the paths to the endnodes
  paths-links     ;; contains links of the paths to the endnodes
] 

nav-vertices-own [ 
  die?            ;; bool to mark vertices for deletion
  pathends        ;; contains endnodes of all paths that connect to this vertex
  paths-nodes     ;; contains nodes of the paths to the endnodes
  paths-links     ;; contains links of the paths to the endnodes
] 

cyclists-own [ 
  next-location   ;; location they are currently cycling towards
  prev-location   ;; last location that the cyclist has passed
  route-step      ;; the number of route nodes that have been cycled by this cyclist
  route-nodes     ;; contains all nodes of the cyclists route
  route-start     ;; the node that the cyclist started form
  route-end       ;; the node where the cyclist wants to get
  route-links     ;; contains all links of the cyclists route
  current-link    ;; the link (street-segment) that the cyclist currently cycles on
  finished?       ;; has the target been reached by the cyclist?
  waiting?        ;; is the cyclist currently waiting at the target to return later
  returning?      ;; is the cyclist on his way back to where it came from
  speed-factor    ;; individual speed-factor of this cyclist
] 

counting-stations-own [
  station-name
  registered-cyclists
]

patches-own [
  residents
  employees
]


;;;;;;;;;;;;;;;;;;;;;;
;; SETUP PROCEDURES ;;
;;;;;;;;;;;;;;;;;;;;;;

;; create street network
to setup
  display
  set failed-links 0
  set failed-nodes 0
  clear-all
  reset-ticks
  reset-timer
  calc-resolution
  set time time:create "0-01-01 00:00:00"
  set time-str time:show time "HH:mm:ss"
  set last-tick-time timer
  
  set status "Loading street dataset ..."
  ;gis:load-coordinate-system "netlogodata/edges.prj"
  ;gis:load-coordinate-system "netlogodata/epsg4326_wgs84.prj"
  gis:load-coordinate-system "netlogodata/epsg32633_utm33N.prj"
  set edges-dataset gis:load-dataset "netlogodata/edges.shp"
  let envelope gis:envelope-of edges-dataset
  
  let x-size item 1 envelope - item 0 envelope
  let y-size item 3 envelope - item 2 envelope
    
  set x-size (x-size / scale-factor)
  set y-size (y-size / scale-factor)
  
  resize-world 0 x-size 0 y-size
  set-patch-size 1
  
  gis:set-world-envelope envelope
  
  set status "Loading resident dataset ..."
  set residents-dataset gis:load-dataset "netlogodata/residents_250m_raster.shp"
  gis:apply-coverage residents-dataset "HWS01" residents 
  set max-residents max [ residents ] of patches
  
  set status "Loading employee dataset ..."
  set employees-dataset gis:load-dataset "netlogodata/employees_250m_raster.shp"
  gis:apply-coverage employees-dataset "BESCH01" employees
  set max-employees max [ employees ] of patches  
  
  ask patches [
    ifelse residents < 0 OR residents >= 0 [][set employees 0]
    ifelse employees < 0 OR employees >= 0 [][set employees 0]
  ] 

  update-resolutions
  
  let street-segments gis:feature-list-of edges-dataset
  let total-segments length street-segments
  let processed-segments 0
  
  ;; loop line features (= street segments)
  foreach street-segments [ 
    
    ifelse gis:property-value ? "oneway_FT" = 1 [ set oneway_FT? true ] [set oneway_FT? false]
    ifelse gis:property-value ? "oneway_TF" = 1 [ set oneway_TF? true ] [set oneway_TF? false]
    ifelse gis:property-value ? "restrictio" = 1 and gis:property-value ? "restrict_1" = 1 [ set street-push? true ] [set street-push? false]
    ifelse gis:property-value ? "restrictio" = 2 and gis:property-value ? "restrict_1" = 2 [ set street-restricted? true ] [set street-restricted? false]
    
    let street-name gis:property-value ? "NAME1"

    ;; loop vertex lists for each street segment
    foreach gis:vertex-lists-of ? [ 
      let i 0 let len (length ?) - 1
      
      let first-vertex nobody
      let last-vertex nobody
      let current-vertex nobody
      let previous-vertex nobody
      let street-segment nobody
      let street-length 0
      let pathdirectional [] 
      
      ;; loop vertices (GIS) and create a list of vertices in netlogo
      foreach ? [  
        let coords gis:location-of ?
        if not empty? coords [
          
          ;; if junctions-only = "on"
          ;;    only consider start and end points of a street segment (= nodes of the graph) for the network graph 
          ;; if junctions-only = "off"
          ;;    consider all vertices of a street segment for the network graph 
          if i = 0 or i = len or not junctions-only [ 
             create-vertices 1 [
                set xcor item 0 coords
                set ycor item 1 coords
                set hidden? true 
                set pathends []
                set paths-nodes []
                set paths-links []
                
                set current-vertex self 
                ;; use existing vertex at this location if there is one and mark new vertex for deletion
                let existing-vertex one-of other nav-vertices-here with [ xcor = [xcor] of myself and ycor = [ycor] of myself ]
                if existing-vertex != nobody [ 
                  set current-vertex existing-vertex
                  set die? true
                ]
               
                ;; create link to previous vertex and save the street type characteristics
                if previous-vertex != nobody and previous-vertex != current-vertex [
                     ask current-vertex [
                       ifelse oneway_FT? = false and oneway_TF? = false 
                       [ create-street-with previous-vertex [ set street-segment self] ]
                       [
                         if oneway_FT? = true [ create-oneway-from previous-vertex [ set street-segment self ] ]
                         if oneway_TF? = true [ create-oneway-to previous-vertex [ set street-segment self ] ]
                       ]
                       if is-link? street-segment [
                         ask street-segment [ 
                           set push? street-push? set restricted? street-restricted? 
                           set name street-name
                           ifelse push? = true 
                           [ set weight link-length / avg-push-speed ]
                           [ set weight link-length / avg-speed]
                           set street-length street-length + weight ]
                       ]
                     ]
                ]
                
                set previous-vertex current-vertex
         
                ;; add this vertex to the a temporary list (of all vertices of this polyline) 
                set pathdirectional lput current-vertex pathdirectional         
                
                if i = 0 [ set first-vertex current-vertex set breed nav-vertices] 
                if i = len [ set last-vertex current-vertex set breed nav-vertices] 
                
                if die? = true [ die ] 
             ]
          ]
        ]
        set i i + 1
      ]
      
      ;; create auxiliary direct link between origin and destination for shortest path computation
      ask first-vertex  [
        if self != last-vertex [
           ifelse oneway_FT? = false and oneway_TF? = false 
           [ create-nav-street-with last-vertex [set street-segment self] ]
           [
             if oneway_FT? = true [ create-nav-oneway-from last-vertex [set street-segment self] ]
             if oneway_TF? = true [ create-nav-oneway-to last-vertex [set street-segment self] ]
           ]
           if is-link? street-segment [
             ask street-segment [ set hidden? true set push? street-push? set restricted? street-restricted? set weight street-length ] 
           ]  
        ]
      ] 
             
      ;; compute paths of each vertex to the start and end vertices of the path that it belongs to
      foreach pathdirectional [ask ? [
          let p-nodes []
          let p-links []
          
          if last-vertex != ? and position last-vertex pathends = false [  
            set pathends lput last-vertex pathends 
            set p-nodes sublist pathdirectional position ? pathdirectional length pathdirectional
            set paths-nodes lput p-nodes paths-nodes
            set p-links get-node-links p-nodes
            set paths-links lput p-links paths-links
          ]
          
          if first-vertex != ? and position first-vertex pathends = false [ 
            set pathends lput first-vertex pathends 
            set p-nodes reverse sublist pathdirectional 0 (position ? pathdirectional + 1)
            set paths-nodes lput p-nodes paths-nodes
            set p-links get-node-links p-nodes
            set paths-links lput p-links paths-links
          ]   
      ]] 
      
    ]
    
    set processed-segments processed-segments + 1
    set status (word "Setup Streetnetwork " round (processed-segments / total-segments * 100) "%")
  ]
  
  ;; visualize "special"streets
  ask oneways [ 
    set shape "oneway"
  ]
  ask (link-set streets oneways) [ 
    if push? = true [ set shape "push" set thickness 0.5]
    if restricted? = true [ set shape "restricted" set thickness 2]
  ]
  
  if not junctions-only [
    setup-counting-stations
  ]
  
  draw-scalebar
  display
end

;;;;;;;;;;;;;;;;;;;;;
;; TICK PROCEDURES ;;
;;;;;;;;;;;;;;;;;;;;;

to timed-go
  let tick-duration (timer - last-tick-time)
  let tick-duration-expected (temporal-resolution / time-factor)
  let tick-duration-tolerated ( tick-duration-expected * 1.1 ) 
  
  if not is-list? tick-durations [ set tick-durations (list) ]
  if tick-duration > 0 [
    set tick-durations fput tick-duration tick-durations
  ]
  if length tick-durations >= 10 [ 
    set tick-durations sublist tick-durations 0 10 
    set tick-duration-mean mean butlast butfirst sort tick-durations
  ]
  
  if tick-duration >= tick-duration-expected [
    
    if (tick-duration-mean > tick-duration-tolerated or auto-time-factor) and length tick-durations >= 10[
        set time-factor ceiling(time-factor / (tick-duration-mean / tick-duration-expected))
        if (tick-duration-mean > tick-duration-tolerated) [
          type "cannot simulate that fast. Automatically reduced time-factor to " print time-factor
        ]
    ]
    
    go false
    
    ;; stop when all cyclists have reached their target
    ;;if not any? cyclists with [finished? != true] [ update-display stop ]  
    
    
    set last-tick-time timer
    set time time:plus time temporal-resolution "seconds"
    set time-str time:show time "HH:mm:ss"
  ]
end

to go [ step ] 
    update-resolutions
    
    if auto-add-cyclists? [
      let auto-add-probability 0.01
      let auto-add-runs 0
      let auto-add-runs-max ((temporal-resolution / 60) * auto-add-num / auto-add-probability)
      while [auto-add-runs < auto-add-runs-max]
      [
        if random (1 / auto-add-probability) = 0 [
          add-cyclists 1 nobody nobody false
        ]
        set auto-add-runs auto-add-runs + 1
      ]
    ]
    
    ;; move all cyclists that have not reached their target
    set add-returning-cyclists-list []
    ask cyclists with [finished? != true] [ cycle ] 
    
    if return? [
      foreach add-returning-cyclists-list [
        add-cyclists 1 item 0 ? item 1 ? true 
      ]
    ]
    
    if auto-remove-cyclists? [
     clear-finished-cyclists
    ]
    
    if heatmap-visible? [ update-heatmap false ]
   
    ;;station-stat
    tick
end
  
;;;;;;;;;;;;;;;;;;;;;;;
;; BUTTON PROCEDURES ;;
;;;;;;;;;;;;;;;;;;;;;;;

to reset
  clear-all-plots
  clear-ticks
  clear-cyclists
  set cnt-cyclists 0
  ask links [ set counter 0 ]
  disable-heatmap
  ask counting-stations [ set registered-cyclists 0 ]
  reset-ticks
  display
end

to manual-add-cyclists
  add-cyclists manual-add-num nobody nobody false
  display
end

to clear-cyclists
  ask cyclists [ remove-route-ends self die ]             ;; remove old cyclists
  display
end

to clear-finished-cyclists
  ask cyclists with [ finished? ] [ remove-route-ends self die ]             ;; remove old cyclists
  display
end

to disable-heatmap
  set heatmap-visible? false
  ask ( link-set streets oneways) [ set color gray ]
  display
end

to update-heatmap [now]
 let max-counter max [counter] of links
 ifelse max-counter > 0 [
   ask ( link-set streets oneways) [
     ifelse counter = 0
     [ set color 1 ]
     [ set color yellow - 4  + counter / max-counter * 8 ]
   ]
 ]
 [ ask ( link-set streets oneways) [set color 1] ]
 if now [display]
end

to setup-counting-stations
  ask counting-stations [ die ]
  
  ;; define counting-station
  let station-names (list "Wallnergasse" "Rudolfskai/Staatsbrücke")
  let station-colors (list orange yellow)
  let station-vertices (list 26455 70379)
  
  (foreach station-vertices station-names station-colors[
    create-counting-stations 1 [
      move-to (turtle ?1)
      set station-name ?2
      set hidden? false
      set size 15
      set shape "star"
      set color ?3
    ]
  ])
end

;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER PROCEDURES ;;
;;;;;;;;;;;;;;;;;;;;;;;

to add-cyclists [ num location target is-returning?]

  let created 0

  while [created < num] [
    
    ;; create cyclists and targets on random vertices
    create-cyclists 1 [
      
      if is-returning? != true [
        let srcdst selectsrcdst false
        set location item 0 srcdst
        set target item 1 srcdst
      ]    
      
      let path false
      ifelse from-sql? = true [
        set path sqlgetpath nobody nobody
        if path = false [
          ;;ask self [die] 
        ]
      ]
      [
        set path getpath location target
      ]
      ifelse path = false [
        ;; always count up if it is returning to avoid being stuck in a loop --> 
        ;; since location,target do not change for returning cyclists it is expected that the 
        ;; path calculation will also fail next time, so just delete it instead of retrying
        if is-returning? = true [ set created created + 1 ]  
        ask self [die]
      ]
      [
        set location item 0 path
        set target item 1 path
        set r-nodes item 2 path
        set r-links item 3 path
        
        ;; save the route variables to the turtle
        set speed-factor 0.5 + random-float 1
        if is-returning? != true [ set cnt-cyclists cnt-cyclists + 1 ]
        set finished? false
        set waiting? is-returning?
        set returning? is-returning?
        set route-step 0
        set route-links r-links
        set current-link item 0 route-links
        set route-nodes r-nodes
        set route-start location
        set route-end target
        set prev-location nobody
        set next-location item 0 route-nodes 
        face next-location
        
        ;; visualize the cyclist and the target
        move-to location
        set color green set size 20
        ask target [ set hidden? false set size 15 set shape "circle" set color red ] 
        
        set created created + 1
        set status (word "Add cyclists " round (created / num * 100) "%")
      ]
    ]
  ]
end

to cycle
  ifelse waiting? = true [ 
    let avg-waiting-ticks (avg-waiting-time * 60 / temporal-resolution)
    if random avg-waiting-ticks = 0 [
      set waiting? false
    ]
  ]
  [  
    ;; set distance to travel per tick as speed in m/s * individual speed factor * duration of one tick in seconds / factor of world to map units
    let remaining-distance (avg-speed / 3.6) * speed-factor * temporal-resolution / scale-factor 
    if is-turtle? prev-location and is-turtle? next-location [
      if is-link? (street [who] of prev-location [who] of next-location) [
        if [push?] of (street [who] of prev-location [who] of next-location) = true 
        [ set remaining-distance (remaining-distance / avg-speed * avg-push-speed) ]
      ]
    ]
     
    ;; move directly to next-location if it is less than 1 step away / move the rest of 1 step towards the following vertex
    while [distance next-location < remaining-distance] [ 
      set remaining-distance remaining-distance - distance next-location 
      move-to next-location
      
      ;; select next intermediate target if there is another item in the route-nodes list and face it
      ifelse route-step < length route-nodes [ 
        set prev-location next-location
        set next-location item route-step route-nodes
        face next-location 
        
        ifelse route-step < length route-links
        [ set current-link item route-step route-links
          if is-link? current-link [
           ask current-link [ set counter counter + 1 ]
          ]
        ]
        [ set current-link nobody ]
        
        set route-step route-step + 1
      ]  
      [ 
        set remaining-distance 0
        if returning? != true and return? [
          set add-returning-cyclists-list lput (list route-end route-start) add-returning-cyclists-list
        ]
        set finished? true
      ]
      
      ;; register at counting-station (if there is one)
      let counting-station one-of counting-stations with [ xcor = [xcor] of myself and ycor = [ycor] of myself ]
      if counting-station != nobody[ ask counting-station [ set registered-cyclists registered-cyclists + 1] ]  
    ]
    
    jump remaining-distance
  ]
end

to-report get-node-links [nodes]
   let node-links []
   if length nodes >= 2[
     (foreach butlast nodes butfirst nodes [
       let n1 [who] of ?1
       let n2 [who] of ?2
       let n-link (street n1 n2)
       if not is-link? n-link [ set n-link (street n2 n1) ]
       if not is-link? n-link [ set n-link (oneway n1 n2) ]
       if not is-link? n-link [ set n-link (oneway n2 n1) ]
       if not is-link? n-link [ set n-link (nav-street n1 n2) ]
       if not is-link? n-link [ set n-link (nav-street n2 n1) ]
       if not is-link? n-link [ set n-link (nav-oneway n1 n2) ]
       if not is-link? n-link [ set n-link (nav-oneway n2 n1) ]
       if not is-link? n-link [ report false ]
       set node-links lput n-link node-links
     ])
   ]
   report node-links
end 

to remove-route-ends [ cyclist ]
  if is-turtle? [ route-start ] of cyclist and is-turtle? [ route-end ] of cyclist [
    ask (turtle-set [ route-start ] of cyclist [ route-end ] of cyclist) [ 
      set hidden? true 
      if breed != nav-vertices [
        ask my-links with [ breed = nav-streets or breed = nav-oneways ] [die]
      ]
    ]
  ]
end

to update-resolutions
    if last-spatial-resolution != spatial-resolution or last-avg-speed != avg-speed[
        set temporal-resolution round( (spatial-resolution / (avg-speed / 3.6)) * 100) / 100
        set last-spatial-resolution spatial-resolution
        set last-avg-speed avg-speed
    ]
    if last-temporal-resolution != temporal-resolution or last-avg-speed != avg-speed[
      set spatial-resolution round( (temporal-resolution * (avg-speed / 3.6)) * 100) / 100
      set last-temporal-resolution temporal-resolution
      set last-avg-speed avg-speed
    ]
end

to-report selectsrcdst [ nav-only ]
  
  let src nobody
  let dst nobody
  
  ifelse nav-only [
       let ran random max-residents
        ask one-of patches with [ residents > ran  and any? nav-vertices-here with [ any? my-links with [ restricted? != true ] ] ] [
          set src one-of nav-vertices-here with [ any? my-links with [ restricted? != true ] ]
        ]
        
      set ran random max-employees
      ask one-of patches with [ employees > ran  and any? nav-vertices-here with [ any? my-links with [ restricted? != true ] and self != src ] ] [
        set dst one-of nav-vertices-here with [ any? my-links with [ restricted? != true ] and self != src ]
      ]
      
  ]
  [
       let ran random max-residents
        ask one-of patches with [ residents > ran  and any? (turtle-set vertices-here nav-vertices-here) with [ any? my-links with [ restricted? != true ] ] ] [
          set src one-of (turtle-set vertices-here nav-vertices-here) with [ any? my-links with [ restricted? != true ] ]
        ]
        
      set ran random max-employees
      ask one-of patches with [ employees > ran  and any? (turtle-set vertices-here nav-vertices-here) with [ any? my-links with [ restricted? != true ] and self != src ] ] [
        set dst one-of (turtle-set vertices-here nav-vertices-here) with [ any? my-links with [ restricted? != true ] and self != src ]
      ]
  ]
          
  report list src dst
end

to integratetonav [ vertex ]
  ask vertex [
   if breed != nav-vertices [ 
        let new-links []
        ifelse not any? my-links with [breed = oneways]
        [ create-nav-streets-with (turtle-set pathends) [ set new-links lput self new-links set hidden? true] ]
        [ 
          foreach pathends [
            let pathind position ? pathends
            let path-links item pathind paths-links
            if position one-of my-in-links path-links != false [
              create-nav-oneway-from ?1 [ set new-links lput self new-links set hidden? true]
            ]
            if position one-of my-out-links path-links != false [
              create-nav-oneway-to ?1 [ set new-links lput self new-links set hidden? true]
            ]
            
          ]
        ]

        if false [
        ask (link-set new-links) [            
          let pathind position other-end [pathends] of myself
          let p-links item pathind [paths-links] of myself
          let segment-weight 0
          foreach p-links [ if is-link? ? [
            ask ? [set segment-weight segment-weight + link-length]
          ]]
          set weight segment-weight
          set hidden? true
        ]
        ]
   ]
  ]
end

to-report getpath [ src dst ]

      integratetonav src
      integratetonav dst 
          
   ;; compute the shortest-path between cyclist and target
      nw:set-context (turtle-set nav-vertices src dst) (link-set nav-streets nav-oneways) with [ restricted? != true ]
      ask src [  
        set r-nodes nw:turtles-on-weighted-path-to dst "weight"
      ] 
            
      ;; add intermediary vertices to the shortest-path route
      if not junctions-only and not empty? r-nodes and length r-nodes >= 2 [
        
        let smoothroute []
        
        ;; loop origin/destination pairs and add the path between them to the route
        (foreach butlast r-nodes butfirst r-nodes  [              
          ask ?1 [
            let pos position ?2 pathends
            if pos != false [
              foreach item pos paths-nodes [
                set smoothroute lput ? smoothroute
              ]
            ]
          ]
        ])
        
        ;; add the path between the last junction and the target if the target is not a junction itself                  
        ask last r-nodes [
          if breed != nav-vertices [
            let pos position item 1 reverse r-nodes pathends
            if pos != false [
              foreach reverse item pos paths-nodes [
                set smoothroute lput ? smoothroute
              ]
            ]
          ]
        ]
        
        set r-nodes remove-duplicates smoothroute
      ]

      ;; create a list of all links between the route nodes
      set r-links get-node-links r-nodes
      
      ;; remove cyclist if the route could not be computed, otherwise continue
      if length r-nodes < 2  or r-links = false
      [ 
        removecyclistlinks src dst
        report false
      ]
      
      report (list src dst r-nodes r-links)
end

to removecyclistlinks [ src dst ]
  ask (turtle-set src dst) [ 
    set hidden? true  
    if breed != nav-vertices [     
      ask my-links with [ breed = nav-streets or breed = nav-oneways ] [die]
    ] 
  ]
          
  if r-links = false [ set failed-links (failed-links + 1) ]
  if length r-nodes < 2 [ set failed-nodes (failed-nodes + 1) ]
end

;;;;;;;;;;;;;;;;;;
;; GIS OVERLAYS ;;
;;;;;;;;;;;;;;;;;;

to display-ds-off [col]
  ask patches[set pcolor col]
  display
end

to display-ds-residents [col]
  let minval min [ residents ] of patches
  let maxval max [ residents ] of patches
  ask patches with [ residents >= 0 OR residents <= 0] [ set pcolor scale-color col residents minval maxval ] 
  display
end

to display-ds-employees [col]
  let minval min [ employees ] of patches
  let maxval max [ employees ] of patches
  ask patches with [ employees >= 0 OR employees <= 0] [ set pcolor scale-color col employees minval maxval ] 
  display
end

to drawing-off
  clear-drawing
  display
end

to drawing-streets
  gis:set-drawing-color grey
  gis:draw gis:load-dataset "netlogodata/edges.shp" 1
  display
end

to drawing-wms
  clear-drawing
  let transparency_byte (transparency / 100 * 255 )
  gis:import-wms-drawing url crs lyr transparency_byte
  display
end

to display-vect-off
  disable-heatmap
  ask (link-set streets oneways) [set hidden? true]
  display
end

to display-vect-links
  disable-heatmap
  ask (link-set streets oneways) [set hidden? false]
  display
end

to display-vect-heatmap
  set heatmap-visible? true
  update-heatmap false
  ask (link-set streets oneways) [set hidden? false]
  display
end


;;;;;;;;;;;;;;;;;;;;;
;; POPULATE DB SQL ;;
;;;;;;;;;;;;;;;;;;;;;

to-report sqlroutingtest [ src dst]
  let len 0
  sql:exec-direct (word "SELECT cost FROM pgr_dijkstra('SELECT gid AS id, start_id::int4 AS source, end_id::int4 AS target, shape_leng::float8 AS cost FROM network', " src "," dst ",false, false);")
  while [sql:row-available?] [
    set len len + item 0 sql:fetch-row
  ]

  report len
end

to sqlsavepath [ src dst path]
  let pathstr []
  foreach path 
  [
    set pathstr lput [who] of ? pathstr
  ]
  
  ;;sql:configure "defaultconnection" [["brand" "postgresql"] ["host" "localhost"] ["port" 5432] ["database" "test"]  ["user" "postgres"] ["password" "admin"] ]
  sql:connect [["brand" "postgresql"] ["host" "localhost"] ["port" 5432] ["database" "test"]  ["user" "postgres"] ["password" "admin"] ]
  sql:exec-update (word "INSERT INTO netlogo (src,dst,path) VALUES ('" [who] of src "','" [who] of dst "','" pathstr "');") []
  sql:disconnect
end

to-report sqlgetpath [ src dst]
  ;;sql:configure "defaultconnection" [["brand" "postgresql"] ["host" "localhost"] ["port" 5432] ["database" "test"]  ["user" "postgres"] ["password" "admin"] ]
  
  sql:connect [["brand" "postgresql"] ["host" "localhost"] ["port" 5432] ["database" "test"]  ["user" "postgres"] ["password" "admin"] ]
  ifelse src = nobody and dst = nobody [
   ifelse sqlsamples > 0 [
      sql:exec-query (word "SELECT * FROM (SELECT src, dst, path FROM netlogo LIMIT " sqlsamples ") subset ORDER BY RANDOM() LIMIT 1;") []
    ]
    [
      sql:exec-query (word "SELECT src, dst, path FROM netlogo ORDER BY RANDOM() LIMIT 1;") []
    ]
  ]
  [
     sql:exec-query (word "SELECT src, dst, path FROM netlogo WHERE src = '" src "' and dst = '" dst "' LIMIT 1;") []
  ]
  if sql:resultset-available? = true [
    let row sql:fetch-row
    let srcagent (turtle read-from-string item 0 row)
    let dstagent (turtle read-from-string item 1 row)
    
    let pathids read-from-string item 2 row
    let pathagents []
    foreach pathids [
      set pathagents lput (turtle ?) pathagents
    ]
    let pathlinks get-node-links pathagents
    sql:disconnect
    report (list  srcagent dstagent pathagents pathlinks)
  ]
  sql:disconnect
  report false
end

to sqlgeneratepaths
  let cnt 100000
  let num 0
  let sqltime timer
  
  while [num < cnt] [
    
    if (num mod 1000) = 0 [
         print (word num "/"cnt)
         print ((timer - sqltime))
         set sqltime timer           
       ]
      
    let srcdst selectsrcdst true
    let src item 0 srcdst
    let dst item 1 srcdst
    if sqlgeneratepath src dst true[
      set num num + 1
    ]
  ]
end

to-report sqlgeneratepath [ srclst dstlst duplicates]
  let sqlsuccess false
  ask dstlst[
   ask srclst[
     let indb? false
     if duplicates = false [
       set indb? sqlgetpath self myself
     ]
     
     if indb? = false[ 
       
       let path getpath self myself
       if path != false [
         set r-nodes item 2 path
         
         if length r-nodes > 1
         [ 
           sqlsavepath self myself r-nodes
           set sqlsuccess true
         ]
       ]
     ]
   ]
 ]
  ifelse sqlsuccess [
    report true
  ]
  [
    report false
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;
;; OUTPUT STATISTICS ;;
;;;;;;;;;;;;;;;;;;;;;;;

to stat
  file-open "stat.csv"
  ask links [
    file-type [who] of end1 file-type "_" file-type [who] of end2 file-type "," file-print counter
  ]
  file-close 
end

to station-stat
  file-open "stat-station.csv"
  ask counting-stations [
    file-type station-name file-type "," file-type time-str file-type "," file-print registered-cyclists
  ]
  file-close 
end

;;;;;;;;;;
;; MISC ;;
;;;;;;;;;;

to calc-resolution
  clear-output
  ;let envelope gis:world-envelope
  gis:load-coordinate-system "netlogodata/epsg32633_utm33N.prj"
  let envelope gis:envelope-of gis:load-dataset "netlogodata/edges.shp"
  
  let x-size precision ((item 1 envelope - item 0 envelope) / scale-factor) 0 
  let y-size precision ((item 3 envelope - item 2 envelope) / scale-factor) 0
  let pnum (x-size * y-size)
  
  ;;let p-size-old patch-size
  ;;let x-size-old world-width 
  ;;let y-size-old world-height 
  ;;let x-size-ratio x-size-old / x-size
  ;;let y-size-ratio y-size-old / y-size
  ;;let p-size-ratio max list x-size-ratio y-size-ratio
  ;;let p-size p-size-old / p-size-ratio
  
  output-type "world-patches: " output-print pnum
  output-type "x-patches: " output-print x-size 
  output-type "y-patches: " output-print y-size
end

to draw-scalebar
  ask vertices-scalebar [die]
  let barwidth 20
  let baroffset 30
  let barlength 150
  let barunit " Meters"
  let barvalue (barlength * scale-factor)
  set barvalue precision barvalue ((-1) * digitcnt barvalue + 1)
  set barlength barvalue / scale-factor
  if barvalue >= 1000 [ set barvalue barvalue / 1000 set barunit " KM"]
  let barlabel (word barvalue barunit)
  let x1 (baroffset / patch-size)
  let x2 (x1 + barlength)
  let y (world-height - (baroffset / patch-size))
  let scalebar-thickness  (barwidth / patch-size)
  
  let vert1 nobody
  let vert2 nobody
  create-vertices-scalebar 1 [set vert1 self set xcor x1 set ycor y]
  create-vertices-scalebar 1 [set vert2 self set xcor x2 set ycor y]
  ask vert1 [create-scalebar-with vert2 [set color red set label-color white set thickness scalebar-thickness set label barlabel]]
end

to-report digitcnt [ num ]
  let cnt 0
  while [num > 0] [
    set num precision (num / 10) 0
    set cnt (cnt + 1)
  ]
  report cnt
end
@#$#@#$#@
GRAPHICS-WINDOW
509
10
1561
1186
-1
-1
1.0
1
12
1
1
1
0
1
1
1
0
1041
0
1144
1
1
1
ticks
1000.0

BUTTON
174
216
230
249
NIL
setup\n
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
380
428
453
461
Add cyclists
manual-add-cyclists
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
105
286
160
319
go
timed-go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
279
404
372
464
manual-add-num
100
1
0
Number

BUTTON
175
286
230
319
step
go true
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
25
216
158
249
junctions-only
junctions-only
1
1
-1000

SLIDER
266
783
481
816
avg-speed
avg-speed
0
30
20
1
1
km/h
HORIZONTAL

SLIDER
266
822
483
855
avg-push-speed
avg-push-speed
0
10
3
1
1
km/h
HORIZONTAL

MONITOR
256
110
309
155
existing
count cyclists
17
1
11

MONITOR
319
110
372
155
cycling
count cyclists  with [finished? != true]
17
1
11

MONITOR
379
110
431
155
waiting
count cyclists with [waiting? = true]
17
1
11

MONITOR
255
59
310
104
simulated
cnt-cyclists
17
1
11

MONITOR
436
110
486
155
finished
count cyclists with [finished? = true]
17
1
11

MONITOR
255
10
422
55
status
status
0
1
11

BUTTON
278
368
372
401
del all cyclists
clear-cyclists
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
27
286
94
319
Reset
reset
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
265
574
346
634
auto-add-num
10
1
0
Number

SWITCH
265
537
476
570
auto-add-cyclists?
auto-add-cyclists?
1
1
-1000

SWITCH
264
499
476
532
auto-remove-cyclists?
auto-remove-cyclists?
0
1
-1000

BUTTON
380
368
455
401
Del finished
clear-finished-cyclists
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
160
532
226
565
Heatmap
display-vect-heatmap
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
372
282
489
327
Wallnergasse [red]
[registered-cyclists] of one-of counting-stations with [ station-name = \"Wallnergasse\"]
17
1
11

PLOT
253
163
489
283
Counting stations
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"Wallnergasse" 1.0 0 -955883 true "" "if count counting-stations with [station-name = \"Wallnergasse\"] > 0 [plot [registered-cyclists] of one-of counting-stations with [ station-name = \"Wallnergasse\"]]"
"Rudolfskai" 1.0 0 -1184463 true "" "if count counting-stations with [station-name = \"Wallnergasse\"] > 0 [plot [registered-cyclists] of one-of counting-stations with [ station-name = \"Rudolfskai/Staatsbrücke\"]]"

MONITOR
253
282
373
327
Rudolfskai [yellow]
[registered-cyclists] of one-of counting-stations with [ station-name = \"Rudolfskai/Staatsbrücke\"]
17
1
11

SWITCH
268
676
358
709
return?
return?
1
1
-1000

INPUTBOX
267
715
356
775
avg-waiting-time
10
1
0
Number

TEXTBOX
307
647
457
666
Cyclist parameters
15
0.0
1

TEXTBOX
260
471
485
490
Automatically add/remove cyclists
15
0.0
1

TEXTBOX
276
339
483
377
Manually add/remove cyclists
15
0.0
1

MONITOR
410
58
485
103
NIL
failed-links
17
1
11

MONITOR
319
58
403
103
NIL
failed-nodes
17
1
11

TEXTBOX
364
725
490
794
Cyclists will wait this many minutes on average before they return home.
11
0.0
1

TEXTBOX
365
673
492
728
Should cyclists return to their origin once they have reached the target?
11
0.0
1

TEXTBOX
356
582
480
624
The number of cyclists to be added every minute on average.
11
0.0
1

INPUTBOX
20
407
124
467
temporal-resolution
1
1
0
Number

INPUTBOX
129
407
224
467
spatial-resolution
5.56
1
0
Number

TEXTBOX
68
441
110
459
seconds
11
0.0
1

TEXTBOX
181
441
222
459
meters
11
0.0
1

TEXTBOX
326
748
348
766
min
11
0.0
1

TEXTBOX
309
605
341
647
#/min
11
0.0
1

TEXTBOX
348
437
363
455
#
11
0.0
1

TEXTBOX
27
395
235
413
resolutions depend on each other and avg-speed
9
0.0
1

SWITCH
135
359
225
392
auto-time-factor
auto-time-factor
1
1
-1000

MONITOR
424
10
486
55
time
time-str
17
1
11

BUTTON
185
944
248
977
gen
sqlgeneratepaths
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
254
944
309
977
stat
stat
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
233
920
383
938
BETA
11
0.0
1

SWITCH
185
986
309
1019
from-sql?
from-sql?
1
1
-1000

INPUTBOX
185
1025
310
1085
sqlsamples
100000
1
0
Number

BUTTON
89
586
156
619
Residents
display-ds-residents red
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
159
586
225
619
Employees
display-ds-employees blue
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
95
483
154
502
Overlays
15
0.0
1

BUTTON
89
532
158
565
Streets
display-vect-links
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
22
586
85
619
OFF
display-ds-off black
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
163
640
227
673
WMS
drawing-wms
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
21
735
227
795
URL
http://maps.omniscale.net/v1/mapsosc-b697cf5a/map
1
0
String

INPUTBOX
131
798
227
858
CRS
EPSG:32633
1
0
String

INPUTBOX
20
798
128
858
LYR
osm
1
0
String

SLIDER
21
699
227
732
Transparency
Transparency
0
100
0
1
1
NIL
HORIZONTAL

BUTTON
104
66
230
99
Calculate
calc-resolution
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
26
66
95
126
scale-factor
10
1
0
Number

OUTPUT
25
129
230
188
12

TEXTBOX
99
112
264
140
--> 1 patch = x * x meters
11
0.0
1

TEXTBOX
31
44
242
76
1. Define suitable patch-resolution
13
0.0
1

BUTTON
90
640
159
673
Streets
drawing-streets
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
21
640
84
673
OFF
drawing-off
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
83
623
233
641
Graphics overlays
11
0.0
1

TEXTBOX
86
570
236
588
Patch Overlays
11
0.0
1

TEXTBOX
84
515
234
533
Vector Overlays
11
0.0
1

TEXTBOX
43
334
238
372
Time / Simulation speed
15
0.0
1

BUTTON
21
532
85
565
OFF
display-vect-off
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
19
359
137
392
time-factor
time-factor
1
100
18
1
1
x Real-Time
HORIZONTAL

TEXTBOX
89
263
239
281
3. Run Model
13
0.0
1

TEXTBOX
87
192
237
210
2. Load Datasets
13
0.0
1

TEXTBOX
80
680
230
698
WMS Configuration
11
0.0
1

TEXTBOX
104
12
254
31
SETUP
15
0.0
1

@#$#@#$#@
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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.1.0
@#$#@#$#@
setup
display-cities
display-countries
@#$#@#$#@
@#$#@#$#@
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

oneway
0.0
-0.2 0 0.0 1.0
0.0 1 4.0 4.0 2.0 2.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

push
0.0
-0.2 0 0.0 1.0
0.0 1 4.0 4.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

restricted
0.0
-0.2 1 4.0 4.0 2.0 2.0
0.0 1 1.0 0.0
0.2 1 4.0 4.0 2.0 2.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
