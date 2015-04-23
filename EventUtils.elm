module EventUtils
    ( floataway
    , vector
    , vectorBounce10
    , vectorFriction
    , effectEvent
    , runIxEvents
    , forever
    , setFPS
    , resetFPS
    , activate
    , toggle
    ) where

import Debug
import DataTypes exposing (..)
import Dict as D
import List as L
import IxArray as Ix
import NodeUtil exposing (getPos, getNodeByIxId, updateNodeInfoScene, updateNodeQTData, updateNodePrefix, getBoundingRect)
import Maybe


unsafeHead (h :: rest) = h

forever = 1/0
never = 0


getRec (ES rec) = rec
getFps (ES rec) = rec.state.fps
getState (ES rec) = rec.state

setFPS currTime fps (ES rec) =
    let
        dt = (1000/fps)
        s = rec.state
        ns = { s | fps <- dt, nextUpdate <- (currTime+dt) }
    in
        ES { rec | state <- ns }


resetFPS currTime (ES rec) =
    let
        s = rec.state
        dt = s.fps
        ns = { s | nextUpdate <- (currTime+dt) }
    in
        ES { rec | state <- ns }

activate : Scene -> EventSet -> EventSet
activate scene (ES es) =
    let
        s = es.state
        ns = { s | lastUpdate <- scene.currTime, nextUpdate <- (scene.currTime + es.state.fps) }
    in
        ES  { es | eventStatus <- True  
            , state <- ns
            }  


toggle : Scene -> EventSet -> EventSet
toggle scene (ES es) =
    let
        s = es.state
        ns = { s | lastUpdate <- scene.currTime, nextUpdate <- (scene.currTime + es.state.fps) }
    in
        ES  { es | eventStatus <- not (es.eventStatus)  
            , state <- ns
            }  

runEvent : Scene -> EventSet -> Ix.IxArray EventSet -> Ix.IxArray Node -> (Ix.IxArray EventSet, Ix.IxArray Node)
runEvent scene (ES eventSet) ixES ixnodes =
    let
        s = eventSet.state
        --a = Debug.log "delta" (scene.currTime - s.lastUpdate)
        --nu = s.nextUpdate + s.fps
        nu = scene.currTime + s.fps  --hmm, is the jitter around the average noticable?
        affectedNodes = L.filterMap (getNodeByIxId ixnodes) eventSet.nodeIds
        ((ES newEventSet), updatedEventNodes) = eventSet.act (ES eventSet) scene affectedNodes
        eventedState = newEventSet.state
        qtEventedNodes = L.map (updateNodeInfoScene scene) updatedEventNodes
        newState = {eventedState | nextUpdate <- nu, lastUpdate <- scene.currTime }
        newNewEventSet = ES ({ newEventSet | state <- newState })
        updatedIxEs = Ix.set eventSet.id newNewEventSet ixES 
    in       
       --(ixES, ixnodes)
        (updatedIxEs, Ix.updateIxArrayFromList ixnodes qtEventedNodes)



--effectEvent scene : (a -> b -> b) is a fold function where a ~ EventSet, b ~ (IxArray EventSet, IxArray Node)
effectEvent : Scene -> EventSet -> (Ix.IxArray EventSet, Ix.IxArray Node) -> (Ix.IxArray EventSet, Ix.IxArray Node)
effectEvent scene (ES eventSet) (ixES, ixnodes) =
    let
        notExpired = (eventSet.state.runTill > scene.currTime)
        update = (eventSet.state.nextUpdate < scene.currTime)
        expired = not notExpired
    in    
        case eventSet.eventStatus of
            True ->    
                if  | (notExpired && update) -> runEvent scene (ES eventSet) ixES ixnodes
                    | expired ->
                        let
                            newES = ES { eventSet | eventStatus <- False }
                            updatedIxES = Ix.set eventSet.id newES ixES
                        in
                            (updatedIxES, ixnodes)
                
                    | otherwise -> (ixES, ixnodes)
            False ->
                (ixES, ixnodes)


--             [a]  [a]   [a]         [a]
-- fold : (a -> b -> b) -> b -> [a] -> b
runIxEvents : Scene -> Ix.IxArray EventSet -> Ix.IxArray Node -> (Ix.IxArray EventSet, Ix.IxArray Node) 
runIxEvents scene ixes ixnodes = 
    let
        es = Ix.getArrayList ixes
    in
        L.foldl (effectEvent scene) (ixes, ixnodes) es 






{-
 |
 | using these two functons in an effect sends an element floating
 | off the top of a collage                              
 |
 -}

incy node = 
    { node | y <- node.y + 1 } 

floataway : EventSet -> Scene -> List Node -> (EventSet, List Node) 
floataway es scene nodes =
    let
        newNodes = L.map incy nodes 
    in 
        (es, newNodes)

reflect : Int -> Int -> Int -> Float -> (Int, Float)
reflect val l u v =
    if | val < l -> (l, -v)
       | val > u -> (u, -v)
       | otherwise -> (val, v)

addDelta : Float -> Int -> Int -> Int -> Int -> Node -> Node
addDelta n sx sy w h node =
    let
        ax = node.x + (floor (n * node.vx))
        ay = node.y - (floor (n * node.vy))
        exx = w//2
        exy = h//2
        (newx, newvx) = reflect ax (sx - exx) (exx + sx) node.vx
        (newy, newvy) = reflect ay (sy - exy)  (exy + sy) node.vy
    in
        { node | x <- newx, y <- newy, vx <- newvx, vy <- newvy }


vector : EventSet -> Scene -> List Node -> (EventSet, List Node)
vector es scene nodes =
    let
        newNodes = L.map (addDelta 15.0 scene.x scene.y scene.w scene.h) nodes
    in
        (es, newNodes)


vectorBounce10 : EventSet -> Scene -> List Node -> (EventSet, List Node)
vectorBounce10 (ES es) scene nodes =
    let
        newNodes = L.map (addDelta 15.0 scene.x scene.y scene.w scene.h) nodes
        state = es.state
        old = unsafeHead nodes
        new = unsafeHead newNodes
        incr = if ((old.vx /= new.vx) || (old.vy /= new.vy)) then 1 else 0
        newcnt  = if (state.count + incr) /= 10 then (state.count + incr) else 0
        newstatus = if (state.count + incr) == 10 then False else True
        newstate = { state | count <- newcnt }
    in
        ((ES {es| state <- newstate, eventStatus <- newstatus} ), newNodes)



invert easing time = 1 - (easing (1 - time))

easeInQuad : Float -> Float
easeInQuad time = time ^ 2
easeOutCirc time = sqrt (1 - (time - 1) ^ 2)
easeInCirc = invert easeOutCirc
easeOutQuad = invert easeInQuad


min_value = 0.0000000000000001
easeOutSine time = sin (time * (pi / 2))

easeInSine = invert easeOutSine

vectorFriction : EventSet -> Scene -> List Node -> (EventSet, List Node)
vectorFriction (ES es) scene nodes =
    let
        s = es.state
        node = unsafeHead nodes
        velo = if (s.flag == True) then (node.unit - min_value) else (easeInSine s.unit)
        newNodes = L.map (addDelta ((toFloat s.count) * velo) scene.x scene.y scene.w scene.h) nodes
        newstatus = if velo < min_value then False else True 
        --a = Debug.log "velo" (velo, s.flag)
        newstate = { s | unit <- velo, flag <- False }
        {-
        -}
    in
        --((ES es), nodes)
        ((ES {es| state <- newstate, eventStatus <- newstatus} ), newNodes)




