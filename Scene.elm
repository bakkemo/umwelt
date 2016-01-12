module Scene
    ( renderScene 
    , updateScene
    , relativeMouse
    , Input
    , InputNoFPS
    , sceneUpdate
    , sceneUpdateNoFPS
    ) where


import Touch exposing (Touch, touches)
import IxArray as Ix
import DataTypes exposing (..)  
import NodeUtil exposing  (..)
import EventUtils exposing (..)
import Triggers exposing (..)
import GJK exposing (..)
import Dict
import Basics as B
import Debug
import Time exposing (Time)
import Color exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import List as L
--import List 
--import Window
--import Mouse
--import Maybe
--import Basics as B
--import Graphics.Input as GI
--import Graphics.Collage
--import Graphics.Element
--import Debug
---import Time as T
--import Array as A
--import Dict as D
--import TestNodes (..)





-- currently what we use...TODO need to add window pos eventually 
type alias Input = (Float, ((Int,Int), List Touch, Time)) 
type alias InputNoFPS = (Float,((Int,Int), List Touch)) 
mouseId = 0


-- need to map from window-space to collage-space
relativeMouse : (Int, Int) -> (Int, Int) -> (Int, Int)
relativeMouse (ox, oy) (x,y) = (x - ox, -y + oy)



-- since there can only be about 5 "touched nodes", just throw them on the end as a
-- quick drawing order fix to draw them on top
renderScene scene =
    let
        a = 1
       -- (Just n1) = Ix.get 0 scene.ixNodes
       -- (Just n2) = Ix.get 1 scene.ixNodes
    in 
        color grey <| collage scene.w scene.h (L.concat (L.map (moveNodeCase scene.ixNodes) ((Ix.getArrayList scene.ixNodes) ++ scene.tnodes)){-++ drawMink scene.up n1 n2-}) 

startFrame r = 
    let
        msg = "/~/~/~/~/~/~/\n/~/~/~/~/~/~/\nENTERING FRAME(" ++ (toString r) ++")"
        a = Debug.log msg "|"
    in
        r


endFrame r =
    let
        msg = "/~/~/~/~/~/~/\n/~/~/~/~/~/~/\nLEAVING FRAME(" ++ (toString r) ++")"
        a = Debug.log msg "|"
    in
        r

-- the meat of the whole thing.. the big state updater.
--TODO:LIST:TODO 
updateScene : Scene -> Float -> (Int, Int) -> List Touch -> List HOV -> List HOV -> Scene
updateScene scene ts (x,y) touches hovs mhovs =
    let
        -- hovs dropped since the last udate need delta's collapsed
        droppedHov = getDroppedHovs scene.hovs hovs
        droppedTouches = getDroppedTouches scene.touches touches
        droppedMHov = getDroppedHovs scene.mHovs mhovs 
        targets = L.concat (L.map (touchToHOVId scene)  (droppedTouches))  -- drop targets...(TODO.. does this include dropee id? composites?)
        touchAffect = (hovToUpdatedIxNode scene.rmp scene.ixNodes droppedHov touches droppedTouches) -- : HOV -> Maybe Node...need to make Maybe [Node]
        atouchedNodes : List Node
        atouchedNodes = L.concat (L.filterMap touchAffect (hovs ++ droppedHov))  -- map over all extant HOVs to get list of updated (dragged) nodes
        qtCentTouchedNodes = L.map (updateNodePrefix scene.w scene.h 3) atouchedNodes -- get new QuadTree center data for those nodes
        qtTouchedNodes = L.map (updateNodeQTData scene.w scene.h scene.qtDepth) qtCentTouchedNodes -- get new QT corner data for those nodes
        ixNodes = Ix.updateIxArrayFromList scene.ixNodes qtTouchedNodes -- this gives the touch effects to nodes
        -- update node and event state, if state needs to be updated THIS time through the loop off a trigger, flip next two lines 
        (updatedIxEvents, eventedIxNodes) = runIxEvents scene scene.ixEventSets ixNodes  
        qtInfo = getQTInfo (Ix.getArrayList eventedIxNodes)
        possibles = deDup (L.concat (L.map qtPairs (qtIntersections  qtInfo)))
        ncd = walkPossibles eventedIxNodes (L.concat (L.map qtPairs (qtIntersections  qtInfo))) Dict.empty  -- walkPossibles essential the same as getCollisionDict
        ixes = runTriggers scene scene.triggers updatedIxEvents -- updating events to on/off...
        --x = startFrame scene.up
        rawCollisionData = L.map (collisionMap scene.up eventedIxNodes) possibles   -- this is essentially the raw collision info in a list...TODO:
                                                                                -- pick a collision dict method and drop this...no need to run
                                                                                -- collision 3 times a frame.
        collisionsData = L.map (substParent eventedIxNodes) (L.filter isCollision rawCollisionData)
        --z = endFrame scene.up
        cd = getCollisionsDict eventedIxNodes possibles
    in
        { scene |
          ixNodes = eventedIxNodes
        , hovs = hovs
        , tnodes = atouchedNodes
        , lastDropTar = {-dbgh-} targets
        , touches = touches
        , mouse = (x,y)
        , mHovs = mhovs
        , droppedMHovs = droppedMHov
        , droppedHovs = droppedHov
        , droppedTouches = droppedTouches
        , up = scene.up + 1
        , ixEventSets = ixes
        , qtData = qtInfo
        , possibles = possibles
        , collData = collisionsData
        , collisionDict = cd
        , currTime = ts
        }   



-- need something to fold
sceneUpdate : Input -> Scene -> Scene
sceneUpdate (ts, ((x,y), touches, fps)) scene =
    let
        -- calculate hover information
        -- N.B. will contain 'None's often
        hovs = L.concat (L.map (touchBaseToHOVId scene) touches)
        mHovs = touchToHOVId scene {x = x, y = y, id = mouseId} 
    in
        updateScene scene ts (x,y) touches hovs mHovs

-- need something to fold
sceneUpdateNoFPS : InputNoFPS -> Scene -> Scene
sceneUpdateNoFPS (ts, ((x,y), touches)) scene =
    let
        -- calculate hover information
        -- N.B. will contain 'None's often
        hovs = L.concat (L.map (touchBaseToHOVId scene) touches)
        mHovs = touchToHOVId scene {x = x, y = y, id = mouseId} 
    in
        updateScene scene ts (x,y) touches hovs mHovs

