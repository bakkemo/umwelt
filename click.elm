import Window
import Mouse
import Touch exposing (Touch, touches)
import List as L
import Maybe
import Basics as B
import Text as T
import Graphics.Input as GI
import Graphics.Collage
import Graphics.Element exposing (..)
import Debug
import Time as T
import Array as A
import Dict as D
import IxArray as Ix
import DataTypes exposing (..)  
import NodeUtil exposing (..)
import TestNodes exposing (..)
import EventUtils exposing (..)
import Triggers exposing (..)
import Scene exposing (..)
import GJK exposing (..)
import Setup exposing (..)
import Signal

defstate =
    { count = 15
    , unit = 0.0
    , runTill = forever
    , fps = 0.0 
    , lastUpdate = 0.0
    , nextUpdate = 0.0
    , flag = True
    }


testEvent2 = ES 
    { nodeIds = [-1]
    , id = 0           
    , name = "dropongreenfloataway"
    , tags = ["green"]
    , eventStatus = False
    , act = floataway
    , state = defstate
    } 

floatEvent = setFPS 1 20 testEvent2
releaseEvent = setFPS 1 60 testEvent


testEvent = ES 
    { nodeIds = [-1]
    , id = 0           
    , name = "bluerelease"
    , tags = ["blue"]
    , eventStatus = False 
    --, act = vector
    --, act = vectorBounce10
    , act = vectorFriction
    , state = defstate
    } 


testTrigger =
    ET 
    { id = -1        -- id of the related eventset
    , eventName = "dropongreenfloataway"   -- name of eventset
    , trigger = trigger  
    }


testTrigger2 =
    ET 
    { id = -1     
    , eventName = "bluerelease"
    , trigger = springTrigger 
    }



combine signals = L.foldr (Signal.map2 (::)) (Signal.constant []) signals


unp = updateNodePrefix sceneWidth sceneHeight 3--testScene.w testScene.h 
uni = updateNodeInfo sceneWidth sceneHeight 3

sceneWidth = 300
sceneHeight = 300
sceneQTDepth = 3

nodes : List Node
nodes = [yellowDot,greenDot,blueDot, trig1,trig2{-,trig4,trig5,, ,trig3-}]
eventSets : List EventSet
eventSets = [floatEvent, releaseEvent {-,testEvent2-} ] 

testScene : Scene
testScene =
    { up=0
    , x = 0
    , y = 0
    , w = sceneWidth
    , h = sceneHeight
    , tnodes = []
    , ixNodes = Ix.listToIxArray (L.map uni nodes)
    , lastDropTar=[]
    , droppedHovs = []
    , droppedTouches = []
    , hovs = []
    , mHovs = []
    , ixEventSets = (Ix.fromList eventSets)
    , triggers = [testTrigger,testTrigger2] 
    , tags = D.empty
    , droppedMHovs = []
    , touches = [] 
    , mouse = (0, 0)
    , rmp = relativeMouse (sceneWidth//2, sceneHeight//2)
    , qtData = D.empty
    , qtDepth = sceneQTDepth
    , collisionDict = D.empty
    , possibles = []
    , collData = []
    , currTime = 0.0
    , initialized = False
    }

utestScene = updateTags testScene


mkText = Signal.map show 
mt = mkText
lift = Signal.map

input : Signal Input
input = T.timestamp (Signal.map3 (,,) Mouse.position touches (T.fps 120)) 

inputNoFPS : Signal InputNoFPS
inputNoFPS = T.timestamp (Signal.map2 (,) Mouse.position touches) 

inpn = mt input

relmp = Signal.map (relativeMouse (150,150)) Mouse.position

--foldp (a -> b -> b) -> b -> Signal a -> Signal b
scenic : Signal Scene
--scenic = Signal.foldp sceneUpdate utestScene input
scenic = Signal.foldp sceneUpdateNoFPS utestScene inputNoFPS


foo : Signal (List Element)
foo = combine [lift renderScene scenic, mkText touches, mkText input, mkText scenic]

bar : Signal Element
bar = lift (flow down) foo


nodesig = lift .collisionDict scenic
nurb = (lift (L.concat << (L.map qtPairs) << qtIntersections << .qtData) scenic)
--nord = lift2 getCollisionsDict nodesig nurb 
poss = lift .possibles scenic
qt = lift .qtData scenic
nds = lift .ixNodes scenic
coldata = lift .collData scenic
coldict = lift .collisionDict scenic
parnode = lift (Ix.get 3) nds
chnode = lift (Ix.get 1) nds
dot = lift (Ix.get 0) nds
chnode2 = lift (Ix.get 2) nds
hovs = lift .hovs scenic
movs = lift .mHovs scenic
ct = lift .currTime scenic

type alias Foo = { name : String, pos : (Float,Float), id : Int, tlp : String, trp: String, blp : String, brp : String {- tel : (Int,Int,Float,Float,Float,Float)-} }

info : Maybe Node -> Foo
info mn {-(Just node)-} =
    case mn of
        (Just node) ->
            { pos = getPos node 
            , id = node.id
            , name = node.name
            , tlp = node.qt_tlp
            , trp = node.qt_trp
            , blp = node.qt_blp
            , brp = node.qt_brp
            -- , tel = (node.x,node.y,node.dx,node.dy,node.px,node.py)
            }               
        Nothing ->        
            { pos = (0.0, 0.0) 
            , id = -2
            , name = ""
            , tlp = ""
            , trp = ""
            , blp = ""
            , brp = ""
            -- , tel = (node.x,node.y,node.dx,node.dy,node.px,node.py)
            }               


n0 = lift info (lift (Ix.get 0) nds)
n1 =  lift info (lift (Ix.get 1) nds)
--n2 =  lift info (lift (Ix.get 2) nds)
--n3 =  lift info (lift (Ix.get 3) nds)
--n4 =  lift info (lift (Ix.get 4) nds)
--n5 =  (lift (Ix.get 5) nds)



pidtup node = (node.id, node.parentId)


pids = lift ((L.map pidtup) << Ix.getArrayList) nds



main : Signal Element
main =  
    lift (flow down) (combine [lift renderScene scenic, mt relmp,mt movs,mt coldata{-mt poss mt scenic-}{-, mt n2, mt n3, mt n4-} {-,mt input,-} ])   
