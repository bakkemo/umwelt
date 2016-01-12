module DataTypes
    ( HOV(..)
    , NodeSet
    , Node
    , NodeId
    , Rect
    , BasePositionId
    , PositionId
    , EventSet(..)
    , EventTrigger(..)
    , Scene
    , MouseMap
    , DragFunc
    , DragHandler(..)
    , EventState
    , BoundType (..)
    , QTData
    , Bunk
    , emptyNode
    , emptyEventSet
    ) where



import Dict
import Touch exposing (Touch, touches)
import Graphics.Collage exposing (filled, circle)
import Array as A
import IxArray as Ix
import Time exposing (Time) 
import Color exposing (yellow)

-- haver type carries node ID, a helpful string, and the touch ID
-- associated with it
type HOV = Hov (Int, String, (Int, Int)) Int | None
type BoundType = Circ | Poly


-- Type to abstract away from implementation
type alias NodeSet = List Node
type alias NodeId = Int

type alias TagMap = Dict.Dict String List NodeId

type alias ANodeSet = A.Array Node

-- when all you care is if there is a bounding box
type alias Rect a = 
    { a | x : Int
    , y : Int
    , w : Int
    , h : Int 
    }          

-- when all you care about is a (x0,y0,id)
type alias BasePositionId a = 
    { a | x0 : Int
    , y0 : Int
    , id : Int
    }          

-- when all you care about is a (x,y,id)
type alias PositionId a = 
    { a | x : Int
    , y : Int
    , id : Int
    }          


type alias EventState =
    { unit : Float 
    , count : Int
    , fps : Float
    , runTill : Time
    , lastUpdate : Time
    , nextUpdate : Time 
    , flag : Bool
    }

emptyEventState =
    { unit = 0.0 
    , count = 0 
    , fps = 0.0 
    , runTill = 0.0 
    , lastUpdate = 0.0 
    , nextUpdate = 0.0 
    , flag = False
    }

type alias QTData =
    { tl : String
    , tr : String
    , bl : String
    , br : String
    }

type EventSet = ES 
    { nodeIds : List NodeId
    , id : Int   -- id of the event set
    , name : String
    , tags : List EventTag
    , eventStatus : Bool
    , state : EventState    
    , act : EventSet -> Scene -> List Node -> (EventSet, List Node) -- does the action need all the scene data?
    } 

nullAct : EventSet -> Scene -> List Node -> (EventSet, List Node)
nullAct es s ln = (es, ln)

emptyEventSet = ES
    { nodeIds = []
    , id = -2   -- id of the event set
    , name = ""
    , tags = []
    , eventStatus = False
    , state = emptyEventState  
    , act = nullAct -- does the action need all the scene data?
    } 

type alias EventTag = String -- temporary definition

type EventTrigger = ET
    { id : Int
    , eventName : String
    , trigger : Int -> Scene -> Ix.IxArray EventSet -> Ix.IxArray EventSet
    }


-- record to wrap extra info around a Form 
type alias Node = 
    { x : Int
    , y : Int       
    , dx : Float
    , dy : Float
    , vx : Float
    , vy : Float
    , px : Float
    , py : Float
    , tl : (Int, Int)
    , br : (Int, Int)
    , unit : Float
    , prefix : String
    , qtUpdate : Bool
    , qt_tlp : String
    , qt_trp : String
    , qt_blp : String
    , qt_brp : String
    , w : Int
    , h : Int
    , id : NodeId
    , parentId : NodeId
    , parentName : String
    , childIds : List NodeId
    , childNames : List String
    , tags : List EventTag
    , bound : BoundType
    , boundPoly : A.Array (Float,Float)
    , name : String
    , dh : DragHandler
    , el : Graphics.Collage.Form
    }

emptyNode : Node
emptyNode = 
    { x = 0
    , y = 0       
    , dx = 0.0
    , dy = 0.0
    , vx = 0.0
    , vy = 0.0
    , px = 0.0
    , py = 0.0
    , tl = (0, 0)
    , br = (0, 0)
    , unit = 0.0
    , prefix = ""
    , qtUpdate = False
    , qt_tlp = ""
    , qt_trp = ""
    , qt_blp = ""
    , qt_brp = ""
    , w = 0
    , h = 0
    , id = -1
    , parentId = -2
    , parentName = ""
    , childIds = []
    , childNames = []
    , tags = []
    , bound = Circ
    , boundPoly = A.empty 
    , name = ""
    , dh = DH noDrag 
    , el = (circle 20 |> filled yellow) 
    }


noDrag : DragFunc 
noDrag node children touch dropped = [node]

-- a collection of nodes that might need touch support
-- maintains previous touches and mouse to determined dropped ones
type alias Scene =  
    { x : Int   -- x,y location of collage
    , y : Int
    , w : Int   -- width and height, for relative mouse position mostly
    , h : Int
    , rmp : MouseMap  -- relative mouse position in collage from page coords
    , tnodes : NodeSet -- nodes that are currently experiencing touch/drag
    , ixNodes : Ix.IxArray Node -- structure to store nodes
    , mHovs : List HOV   -- Hovs resulting from the mouse (x,y)
    , droppedMHovs : List HOV -- dropped mouse Hovs
    , hovs : List HOV   -- Hovs resulting from touches
    , touches : List Touch -- current touches (the better to calculate dropped touches)
    , mouse : (Int, Int) -- because you might want it :)
    , droppedHovs : List HOV -- dropped (lifted) hovs
    , droppedTouches : List Touch -- dropped touches
    , up  : Int -- if you need an id for something use this.
    , ixEventSets : Ix.IxArray EventSet
    , tags : Dict.Dict EventTag Int 
    , triggers : List EventTrigger
    , lastDropTar : List HOV -- hovs that a released touch landed on
    , qtData : Dict.Dict Int (List Int)
    , qtDepth : Int
    , collisionDict : Dict.Dict (Int, Int) Bool
    , possibles : List (Int, Int)
    , collData : List ((Int, Int), Bool)
    , currTime : Time
    , initialized : Bool
    }

type alias MouseMap = ((Int,Int) -> (Int, Int))
type alias DragFunc = Node -> List Node -> Touch -> Bool -> List Node
type DragHandler = DH (DragFunc)



type alias Bunk =
    { depth : Int
    , sim : List (Float, Float)
    , d : (Float, Float)
    , bp1 : List (Float, Float)
    , bp2 : List (Float, Float)
    , h : List (Float, Float)
    }



{-
type PersonInput = { dt : Int, ambientInfection : Bool }

type InfectionState = PersonInput -> PersonState -> PersonState


type  PersonState = 
    { infection : InfectionState
    , timeUntilTransition : Time
    , transitionRate : Float
    }

-}       
