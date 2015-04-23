module TestNodes
    ( greenDot
    , blueDot
    , yellowDot
    , trig1
    , trig2
    , trig3
    , trig4
    , trig5
    , testMink
    ) where


import DataTypes exposing (..)
import Graphics.Collage exposing (..)
import NodeUtil exposing (..)
import Array as A
import IxArray as Ix
import Color exposing (..)
import List as L

blueCirc : Form
blueCirc = circle 20 |> filled lightBlue
greenCirc = circle 20 |> filled lightGreen
yellowCirc = circle 20 |> filled yellow

boundary1 = [(-15.0,-10.0),(0.0,15.0),(12.0,-5.0)]
boundary2 = [(-20.0,-7.0),(0.0,17.0),(13.0,-13.0)]

poly1 = polygon boundary1 |> outlined defaultLine
poly2 = polygon boundary2 |> outlined defaultLine



dot : Node
dot =   { x = 0
        , y = 0
        , dy = 0
        , dx = 0
        , vx = 0
        , vy = 0
        , px = 0
        , py = 0
        , tl = (0,0)
        , br = (0,0)
        , unit = 0
        , prefix = "" 
        , qt_trp = ""
        , qt_tlp = ""
        , qt_brp = ""
        , qt_blp = ""
        , qtUpdate = False
        , w = 20
        , h = 20
        , tags = []
        , bound = Circ
        , boundPoly = A.empty
        , id = 0
        , parentId = -1
        , parentName = ""
        , childIds = []
        , childNames = []
        , name = ""
        , dh = DH simpleDrag
        , el = (circle 20 |> filled yellow) 
        }

blueDot = { dot | x <- 30, y <- 80, id <- 1, name <- "blue", tl <- (-20,20), br <- (20,-20), childNames <- [], dh <- DH springDrag, el <- blueCirc}
greenDot = { dot | x <- -30, y <- -30, id <- 2, tl <- (-20,20), br <- (20,-20), name <- "green", el <- greenCirc }
yellowDot = { dot | x <- 100, y <- 100, id <- 3, tl <- (-20,20), br <- (20,-20), name <- "yellow", el <- yellowCirc }
trig1x = { dot | id <- 4, name <- "trig1", tl <- (-40,20), br <- (40,-40), bound <- Poly, boundPoly <- (A.fromList boundary1), el <- poly1}
trig2x = { dot | id <- 5, name <- "trig2", tl <- (-60,50), br <- (40,-40), bound <- Poly, boundPoly <- (A.fromList boundary2), el <- poly2, x <- 32 }
trig3 = { dot | y <- 5, id <- 6, name <- "trig3", tl <- (-40,20), br <- (40,-40), bound <- Poly, boundPoly <- (A.fromList boundary1), childNames <-["trig1","trig2"], el <- poly1 }
trig4 = { dot | id <- 7, name <- "trig4", tl <- (-60,50), br <- (40,-40), bound <- Poly, boundPoly <- (A.fromList boundary2), el <- poly2, y <- -32 }
trig5 = { dot | id <- 8, name <- "trig5", tl <- (-60,50), br <- (40,-40), bound <- Poly, boundPoly <- (A.fromList boundary2), el <- poly2, y <- 90 }


trig1 = {-updateNodeInfo 0 0 0-} trig1x
trig2 = {-updateNodeInfo 0 0 0-} trig2x

testMink : Bunk
testMink = { depth = 0
           , sim = [(45,18),(-2,-11)]
           , d = (-13311,21573)
           , h = [(-13,-1),(2,24),(14,4)]
           , bp2 = [(12,-7),(32,17),(45,-13)]
           , bp1 = [(-2,-11),(18,13),(45,18),(58,-12),(43,-37),(10,-31)]
           }

setCompositeDeltas (dx,dy) node = { node | px <- dx, py <- dy }

{-
 |
 | this drag function simply accumulates a drag (uses the x and y from a touch)
 | into the dx and dx fields of a not. Upon drop, it updates the node x and y
 | and clears the deltas
 |
 | 'vx' and 'vy' in the calculation does not correspond to vx,vy in the node
 | which is less than ideal as a source of confusion
 | composite objects also complicates things
 -}

simpleDrag : DragFunc 
simpleDrag node children touch dropped =
    let
        vx = touch.x - touch.x0
        vy = touch.y - touch.y0
        nx = node.x + vx
        ny = node.y - vy
        newNode = case dropped of
                    False -> { node |  dx <- toFloat vx, dy <- toFloat vy }            
                    True  -> { node | x <- nx, y <- ny, dx <- 0, dy <- 0 } 
        pos = getPos newNode

    in
        newNode :: (L.map (setCompositeDeltas pos) children)



{-
 |
 | this drag function induces an easing funtion on the modulus of he
 | touch deltas i.e. the deltas accumulate in the node deltas as
 | a decreasing function of distance... like pulling back on a pinball
 | lever
 |
 -}

--     [0..1.0] -> [0..1.0]
easeIn : Float -> Float
easeIn time = time ^ 3

invert easing time = 1 - (easing (1 - time))

springDrag : DragFunc 
springDrag node children touch dropped =
    let
        travel = toFloat 20
        draw = toFloat 60
        dx = touch.x - touch.x0
        dy = touch.y - touch.y0
        udx = toFloat (max 1 (abs dx))
        udy = toFloat (max 1 (abs dy))
        len = sqrt (udx*udx + udy*udy)
        normdx = (toFloat dx) / len
        normdy = (toFloat dy) / len
        vx = -normdx
        vy = -normdy
        unit = min 1.0 (len/draw)

        newdx = normdx * (travel * (invert easeIn unit))    
        newdy = normdy * (travel * (invert easeIn unit))    

    in
        case dropped of
            False -> [{ node |  dx <- newdx, dy <- newdy }]            
            True  -> [{ node |  dx <- 0, dy <- 0, vx <- vx, vy <- vy, unit <- unit}]  





