module Triggers
    ( trigger 
    , springTrigger
    , runTriggers
    ) where

import DataTypes exposing (..)
import IxArray as Ix
import NodeUtil exposing (getHOVByName, getHOVById)
import EventUtils exposing (activate, toggle)
import List
import Debug


{-
 | run triggers function (and accompanying fold fucntion
 |
 |
 -}

--          trig  IxArr Eventset  IxArr EventSet 
-- (Scene -> a   ->     b ->    b)
trigFold : Scene -> EventTrigger -> Ix.IxArray EventSet -> Ix.IxArray EventSet
trigFold scene (ET trig) ixaES =
    trig.trigger trig.id scene ixaES
    

runTriggers : Scene -> List EventTrigger -> Ix.IxArray EventSet -> Ix.IxArray EventSet
runTriggers scene triggers ixaEventSets =
    List.foldl (trigFold scene) ixaEventSets triggers 



{-
 |
 | some sample triggers
 |
 | triggers are passed the ID of the event set they are triggering
 -}

dbh : HOV -> HOV
dbh hov =
    case hov of
        None -> hov
        _  -> Debug.log "tr_dbh" hov





trigger : Int -> Scene -> Ix.IxArray EventSet -> Ix.IxArray EventSet
trigger esid scene ixaEventSets =
    let
        green = getHOVByName "green" scene.lastDropTar
    in
        case ({-dbh-} green) of
            None    -> ixaEventSets
            _       -> 
                        let
                            (Just es)  = Ix.get esid ixaEventSets
                            nes = toggle scene es
                        in
                            Ix.set esid nes ixaEventSets

{-
 |
 -}

springSetEventRec rec =
    let    
        s = rec.state
        newState = { s | flag <- True }
    in
        { rec | eventStatus <- True, state <- newState }


springTrigger : Int -> Scene -> Ix.IxArray EventSet -> Ix.IxArray EventSet
springTrigger esid scene ixaEventSets =
    let
        --blue = getHOVById id scene.droppedHovs
        blue = getHOVByName "blue" scene.droppedHovs
        --nid = Debug.log "trid" id
    in
        case ({-dbh-} blue) of
            None -> ixaEventSets 
            _    -> 
                    let
                        (Just (ES rec)) = Ix.get esid ixaEventSets
                        nes =  ES (springSetEventRec rec) 
                    in
                        Ix.set esid nes ixaEventSets                    


