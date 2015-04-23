module GJK
    ( collision
    , polyFromList
    , getCollisionsDict
    , walkPossibles
    , collisionMap
    , deDup
    , isCollision
    , substParent
--    , doSimplexD
--    , drawMink
--    , testMink                             
    ) where

import DataTypes exposing (..)
import NodeUtil exposing (getPos)
import IxArray as Ix
import Array as A
import List as L
import Debug
import Dict as D
import Collision as C


type alias Poly = A.Array (Float,Float)
type alias Pt = (Float, Float)

-- a suuport is a function from a poly and a direction to a vertex index in the poly
-- maybe not
type alias Support = Poly -> (Float, Float) -> (Float,Float)
type alias Mink a = (a, (a -> Pt -> Pt))


unsafeHead (h :: rest) = h


{-
 - all this flipping back and forth from array representaion to list representation
 - is with the expectation that caching the indexes of the vertices that represent
 - the converged simplex is a useful optimization at some point
 -}
polyFromList : List (Float, Float) -> Poly
polyFromList list = A.fromList list


{-
 - utility functions on Points 
 -}
dot : Pt -> Pt -> Float
dot (x1,y1) (x2,y2) = (x1*x2) + (y1*y2)

--sub a b = b - a -- subtract a from b
sub : Pt -> Pt -> Pt
sub (ax, ay) (bx,by) = (bx-ax, by-ay)

add : Pt -> Pt -> Pt
add (ax, ay) (bx,by) = (bx+ax, by+ay)

mul : Float -> Pt -> Pt
mul n (x,y) = (n*x, n*y)

neg : Pt -> Pt
neg (x,y) = (-x,-y)

-- same as sub...reads better in some cases - as in "from a to b"
from : Pt -> Pt -> Pt
from (ax, ay) (bx, by) = (bx-ax, by-ay)


{-
 - the sensible version (for our purposes) of the
 - cross product for 2D vectors. note that this is
 - actually the scalar value of the Z (or K of ijk) component of
 - the 3D cross product, which is great, since that direction
 - is all we care about for a point in a triangle test
 - 
 - MATH, BITCHES!
 -} 
cross2D : Pt -> Pt -> Float 
cross2D (ax, ay) (bx, by) = ax*by - ay*bx


{-
 - implements a x (b x c) = b(a.c) - c(a.b)
 -}
trip : Pt -> Pt -> Pt -> Pt
trip a b c =
    let
        bac = mul (dot a c) b
        cab = mul (dot a b) c
    in
        sub cab bac


{-
 - calculate the polygon vertex furthest in the direction of d
 - via Poly array
 -}
polySupport : Poly -> (Float, Float) -> (Float, Float)
polySupport poly d =
    let
        list = A.toList poly
        dotList = L.map (dot d) list
        decorated = (L.map2 (,)) dotList list
    in
        case (L.maximum decorated) of
            Just (m, p) -> p
            otherwise   -> (-10000,-10000)


{-
 - synthesize a "vertex" for the circle. normalize the vector and scale
 - to obtain intersection with the vector. then translate by node position
 -}
circSupport : Node -> (Float, Float) -> (Float,Float)
circSupport node (a,b) =
    let
        len = sqrt (a*a + b*b)
        r = toFloat node.w -- circles us the width field for radius
    in
        ((a*r/len),(b*r/len))


{-
 - calculate the polygon vertex furthest in the direction of d
 - via list of points 
 -}
listSupport list d =
    let
        dotList = L.map (dot d) list
        decorated = (L.map2 (,)) dotList list
    in
        case (L.maximum decorated) of
            Just (m, p) -> p
            otherwise   -> (-10000,-10000)


{-
 - calculate the Minkowski difference vertex furthest in the direction of d
 - via list of points 
 -}
calcMinkSupportBP bp1 bp2 d =
    let
        p1 = listSupport bp1 (neg d)
        p2 = listSupport bp2 d
     in
        sub p1 p2

{-
 - calculate the support of the Minkowski difference
 - of two nodes via node ID
 -}
calcMinkSupportByID : Ix.IxArray Node -> Int -> Int -> (Float, Float) -> (Float,Float) 
calcMinkSupportByID ixNodes id1 id2 d =
    let
        (Just node1) = Ix.get id1 ixNodes -- these should in fact be total
        (Just node2) = Ix.get id2 ixNodes -- these should in fact be total
        c1 =  getPos node1
        c2 =  getPos node2
        v1 = getSupport node1 (neg d)
        v2 = getSupport node2 d
     in
        sub (add v1 c1) (add v2 c2)


{-
 - if any simplex points or the support direction are (0,0), then
 - we had coinciding shape vertices in opposite directions, thus we intersect
 -}
anyZero : Pt -> Pt -> Pt -> Bool
anyZero a b c =  (a == (0.0, 0.0)) || (b == (0.0, 0.0)) || (c == (0.0, 0.0))


{-
 - who needs more than circles and polygons?
 -}
getSupport : Node -> (Float, Float) -> (Float, Float)
getSupport node d =
    if | node.bound == Poly -> polySupport node.boundPoly d
       | node.bound == Circ -> circSupport node d


myGetSupport : Node -> (Float, Float) -> (Float, Float)
myGetSupport node d =
    let
        c = getPos node
        v = 
            if | node.bound == Poly -> polySupport node.boundPoly d
            | node.bound == Circ -> circSupport node d
    in
        add v c




{-
 - calculate the support of the Minkowski difference
 - of two nodes 
 -}
calcMinkSupport : Node -> Node -> (Float, Float) -> (Float,Float) 
calcMinkSupport node1 node2 d =
    let
        x = if d == (0,0) then Debug.log "(0,0) passed to CMS" d else d 
        c1 =  getPos node1
        c2 =  getPos node2
        v1 = getSupport node1 (neg d)
        v2 = getSupport node2 d
        p1 = add c1 v1
        p2 = add c2 v2
     in
        sub p1 p2

getNodeSupport : Node -> (Float, Float) -> (Float, Float)
getNodeSupport node d =
    let
        c = (getPos node)
        v = getSupport node d
        a =
           case node.bound of
              Circ -> add v c
              Poly -> add v c
    in
        a


-- pass bc as first parameter
getDirectionVector : Pt -> Pt -> Pt
getDirectionVector (x1, y1) (x2, y2) =
    let
        -- try the triple cross
        d = trip (x1,y1) (x2,y2) (x1,y1)
        colinear = (d == (0,0))
        --s = if colinear then Debug.log "bc colinear with c0" 1 else 1

        -- bc and c0 are colinear and gave us a 0 cross product.
        -- inject bc into 3d and get ANY perp (the algorithm should
        -- not care in this case) so now crossing bc=(x1,y1,0) with c=(-x2, -y2, 1)
        -- remember the second parameter was c0 = -c
        --
        -- (u2v3 - u3v2)i - (u1v3 - u3v1)j + (who cares)k
        -- so...
        -- px = (y1*1 - 0*(-y2)),  py = -(x1*1 - 0*(-x2))   
        -- px = y1                 py = -x1
        -- 
        -- which is a detailed derivation of the obvious :)
    in
        if colinear then (y1, -x1) else d 

{-
-}
collision : Int -> (Ix.IxArray Node) -> Int -> Int -> Bool
collision frame ixNodes id1 id2 = 
    let
        (Just node1) = Ix.get id1 ixNodes -- these should in fact be total
        (Just node2) = Ix.get id2 ixNodes -- these should in fact be total
        oblivious = (node1.parentId == node2.parentId) || (L.length node1.childIds > 0) || (L.length node2.childIds > 0)
        intersects = C.collision 200 (node1, getNodeSupport) (node2, getNodeSupport)
    in
        if  | oblivious -> False 
            | otherwise -> intersects

{-

 - the loop of collision recurses with the nodes (for support calculations) and a  
 - (simplex, direction) pair. The simplex can be one or two points, and an attempt
 - to enclose the origin by adding a third "support" point in the given direction
 - If the simplex is one point, then encloses will fail on its patern match, and
 - a second vertex will be added in the next recursion
 -}
{-
collision : Int -> (Ix.IxArray Node) -> Int -> Int -> Bool
collision frame ixNodes id1 id2 = 
    let
        (Just node1) = Ix.get id1 ixNodes -- these should in fact be total
        (Just node2) = Ix.get id2 ixNodes -- these should in fact be total
        oblivious = (node1.parentId == node2.parentId) || (L.length node1.childIds > 0) || (L.length node2.childIds > 0)
        d1 = (1.0, 0.0)
        d2 = neg d1
        c = calcMinkSupport node1 node2 d1
        b = calcMinkSupport node1 node2 d2

        -- simplex is cb and direction is (cb x c0 x cb)
        cb = from c b
        c0 = neg c
        d = getDirectionVector cb c0
        (intersects, (sim, newD)) = doSimplex frame 0 node1 node2 ([b,c], d)
        cint = C.collision 200 (node1, myGetSupport) (node2, myGetSupport)
    in
        if  | oblivious -> False 
            | otherwise -> intersects
-}


{-
 - a pair of sanity functions
 -}
bail d bool ret =
    let
        a = if bool then Debug.log ("GJK exiting at depth: " ++ (toString d)) ret else ret
        b = 1 
    in
        ret

rep frame depth sim l1 l2 =
    let
        a = if ((depth > 200) && (frame > 0)) then Debug.log "s+" (frame,sim,l1,l2) else (frame,sim,l1,l2)
    in
        depth


{-
 - the loop of collision. Recurses with the nodes (for support calculations) and a  
 - (simplex, direction) pair. The simplex can be one or two points, and an attempt
 - to enclose the origin by adding a third "support" point in the given direction
 - If the simplex is one point, then encloses will fail on its patern match, and
 - a second vertex will be added in the next recursion

TODO: remove parameters frame and depth in released code

 -}
doSimplex : Int -> Int -> Node -> Node -> (List Pt, Pt) -> (Bool, (List Pt, Pt)) 
doSimplex frame depth node1 node2 (sim, d) =-- (True, ([(0.0,0.0)], (0.0,0.0)))
    let
        a = (calcMinkSupport node1 node2 d)
        notPastOrig = ((dot a d) < 0)       -- if not past origin, there is no intersection
        b = unsafeHead sim
        supportError = anyZero a b d        -- shared vertexes is going to count as intersection
        (intersects, (newSim, newDir)) = enclosesOrigin a sim
       -- (aInt, (ans, and)) = C.enclosesOrigin a sim
    in
       if | notPastOrig -> bail depth (intersects) (False, ([], (toFloat depth,toFloat depth)))
          | supportError -> bail depth  (intersects) (True, ([], (0,0)))
          | intersects -> bail depth  (False) (True, (sim, d))
          | (depth > 200) -> bail depth  (intersects) (False, (newSim, newDir)) 
          | otherwise -> doSimplex frame (depth+1) node1 node2 (newSim, newDir)


{-
 -- version that limits the recursion to a certain depth (for debugging purposes)

doSimplexD : Int -> Int -> [(Float, Float)] -> [(Float, Float)] -> ([Pt], Pt) -> (Bool, ([Pt], Pt)) 
doSimplexD frame depth bp1 bp2 (sim, d) =
    let
        a = (calcMinkSupportBP bp1 bp2 d)   -- new point in direction of d
        notPastOrig = ((dot a d) < 0)       -- if not past origin, there is no intersection
        b = head sim
        supportError = anyZero a b d        -- shared vertexes is going to count as intersection
        (intersects, (newSim, newDir)) = enclosesOrigin a sim
        --z = Debug.log "d" (depth, frame)
    in
       if | notPastOrig -> bail (False, ([], (0,0)))
          | supportError -> bail (True, ([], (0,0)))
          | intersects -> bail (True, (sim, d))
          | (depth > frame) -> bail  (False, (newSim, newDir)) 
          | otherwise -> doSimplexD frame (depth+1) bp1 bp2 (newSim, newDir)
-}


{-
 - The handleNSimplex functions are named for the size of the simplex BEFORE a is added :)
 -}
enclosesOrigin : Pt -> List Pt -> (Bool, (List Pt, Pt))
enclosesOrigin a sim =
    case sim of
        b :: []         -> handle0Simplex a b      -- 0-simplex case
        b :: c :: []    -> handle1Simplex a b c
        _               -> Debug.log "Impossible simplex" (False, (sim,(0,0)))


{-
 - simplex is a single point, we will be adding a to simplex one way or another ([a,b] or [a])
 -}
handle0Simplex a b =
    let
        ab = (from a b)
        a0 = neg a
        sameDirection = ((dot ab a0) > 0)
        (newSim, newDir) = if sameDirection then ([a,b], (perp ab a0)) else ([a], a0)
    in
        (False, (newSim, newDir))


{-
 - simplex is a line segment [b,c], adding 'a' gives us a 2-Simplex. We now either enclose the
 - origin, or will be replacing the simplex with the closest sub-component to the origin
 -}
handle1Simplex a b c =
    let
        -- a is our new local point of reference
        a0 = neg a
        ab = from a b
        ac = from a c
        abp = perp ab (neg ac) -- perpendicular to ab facing away from c
        acp = perp ac (neg ab) -- perpendicular to ac facing away from a
    in
        if  | (isSameDirection abp a0) -> -- region 4 or 5
                if (isSameDirection ab a0) then (False, ([a,b], abp)) else (False, ([a], a0))
            | (isSameDirection acp a0) -> -- region 6 or 5
                if (isSameDirection ac a0) then (False, ([a,c], acp)) else (False, ([a], a0)) 
            | otherwise -> (True, ([b,c], a0)) 

-- perpendicular to a in direction of b (2D case)
--perp a b = trip a b a
perp a b =
   let
      p = trip a b a
      p2 = if (p == (0,0)) then Debug.log "GJK perp err" p else p
   in
      p

-- not sure this gets inlined :/
isSameDirection a b = (dot a b) > 0



{-

testMink = { depth = 0
           , sim = [(45,18),(-2,-11)]
           , d = (-13311,21573)
           , bp1 = [(-13,-1),(2,24),(14,4)]
           , bp2 = [(12,-7),(32,17),(45,-13)]
           , hull = [(-2,-11),(18,13),(45,18),(58,-12),(43,-37),(10,-31)]
           }

fmove thing pt = move pt thing 

drawDir d = [traced {defaultLine | color <- darkRed} (path [(0,0),d])]

drawSim sim =
    if  | ((length sim) > 1) -> [traced {defaultLine | color <- darkBlue} (path sim)]
        | otherwise -> map (fmove (filled darkBlue (circle 5))) sim

drawMink frame node1 node2 =
    let
        cent = filled red (circle 2)
        vert = filled purple (circle 3)
        dot = filled darkBlue (circle 5)
        --hull = minkowski testMink.bp1 testMink.bp2
        hull = [(-2.0,-11.0),(18,13),(45,18),(58,-12),(43,-37),(10,-31)]
        pts = map (fmove vert) testMink.hull 
        mink = alpha 0.3 (filled lightBlue (polygon hull)) 
        (f,(sim,d)) = doSimplexD frame 0 testMink.bp1 testMink.bp2 (testMink.sim, testMink.d)
        --a = Debug.log "res" (f, sim, d)
        fee = drawSim sim--[(-50,-50)] 
    in
        [move (0,0) mink, move (0,0) cent] ++ pts ++ fee ++ (drawDir d)

-}



{-
encloses : Pt -> [Pt] -> Bool
encloses a sim =
    case sim of
        b :: c :: [] -> enclosesOriginCheck a b c
        _ -> False


 - enclosesOriginCheck takes 3 points A, B, C (determining a triangle, with
 - the assumption that the second 2 points (B,C) are a 1D simplex, and 
 - that A was generated as the support of a Minkowski sum in the direction
 - CB x B0 x CB as per GJK) i.e. all the fail fast "passed the origin" checks
 - of GJK have already been performed - A is past the origin in the direction 
 - of the perpendicular of CB, and the most recently "added" point
 -

enclosesOriginCheck : Pt -> Pt -> Pt -> Bool
enclosesOriginCheck (ax,ay) (bx,by) (cx, cy) =
    let
        ab = (bx-ax, by-ay)
        ac = (cx-ax, cy-ay)
        a0 = (-ax, -ay)
        abx = cross2D ab a0
        acx = cross2D ac a0

    -- by construction, A is past the origin from CB, so if A0 is on different sides
    -- of AB and AC, then the triangle encloses the origin, which happens <=>
    -- (AB x A0) and (AC x A0) have opposite sign (N.B. 2D cross is a scalar)
    
    in
        -- I have no idea if the XORed one liner is faster, but this should be clearer 
        if  | ((abx < 0) && (acx > 0)) -> True
            | ((abx > 0) && (acx < 0)) -> True
            | otherwise -> False
 -}



-- replace (a,b) with ((a,b), collision a b). i.e Do 'a' and 'b' collide? 
-- currently only used in scene.elm. TODO: test speed of 
-- (Dict.fromList (map (collisionMap ixNodes) (deDup possibles)) vs getCollisionDict vs walkPossibles 
collisionMap : Int -> Ix.IxArray Node -> (Int, Int) -> ((Int,Int), Bool)
collisionMap frame ixNodes (a,b) = ((a,b), collision frame ixNodes a b)

isCollision : ((Int,Int), Bool) -> Bool
isCollision ((a,b), bool) = bool 


substParent : Ix.IxArray Node -> ((Int,Int), Bool) -> ((Int,Int),Bool)
substParent ixNodes ((a,b), bool) =
    let
        (Just node1) = Ix.get a ixNodes
        (Just node2) = Ix.get b ixNodes
    in

        ((node1.parentId, node2.parentId), bool)


collisionFold : Ix.IxArray Node -> (Int, Int) -> D.Dict (Int, Int) Bool -> D.Dict (Int, Int) Bool
collisionFold ixNodes (a,b) dict =
    let
        newDict= if | D.member (a,b) dict -> dict
                    | (collision -1 ixNodes a b) -> D.insert (a,b) True dict
                    | otherwise -> dict
    in
        newDict
        
getCollisionsDict : Ix.IxArray Node -> List (Int, Int) -> D.Dict (Int, Int) Bool
getCollisionsDict ixNodes possibles = L.foldr (collisionFold ixNodes) D.empty possibles 

updateCollisionDict : Ix.IxArray Node -> Int -> Int -> D.Dict (Int, Int) Bool -> D.Dict (Int, Int) Bool
updateCollisionDict ixNodes a b dict =
    if | D.member (a,b) dict -> dict
       | otherwise -> D.insert (a,b) (collision -2 ixNodes a b) dict

        
walkPossibles : Ix.IxArray Node -> List (Int, Int) -> D.Dict (Int, Int) Bool -> D.Dict (Int, Int) Bool
walkPossibles ixNodes possibles dict =
    case possibles of
        (a,b) :: rest -> walkPossibles ixNodes rest (updateCollisionDict ixNodes a b dict) 
        [] -> dict


{-
 - since nodes can intersect in multiple quadtree spacial partitions, it should
 - be faster to just remove the duplicates. N.B. There is an invarient that a < b in (a,b)

 - easiest solution is to dump everything into a dict, and grab the keys at the end

 -}
deDup : List (Int,Int) -> List (Int, Int)
deDup possibles =
    let
        res = deDup' possibles D.empty
    in
        D.keys res

deDup' possibles dict = 
    case possibles of
        a :: rest -> deDup' rest (D.insert a a dict)
        [] -> dict



{-
updateDictOnCollision : Ix.IxArray Node -> (Int, Int) ->  D.Dict (Int, Int) Bool -> D.Dict (Int, Int) Bool
updateDictOnCollision ixNode (a,b) dict =
    let
        col = collision ixNodes a b
    in
        if col then  D.insert (a,b) True dict 

updateDict : Int -> D.Dict Int Bool -> D.Dict Int Bool
updateDict a dict =--\
    if | D.member a dict -> dict--\
       | otherwise -> D.insert a True dict


flo : [Int] -> D.Dict Int Bool -> D.Dict Int Bool
flo list dict =--\
    case list of--\
        a :: rest -> flo rest (updateDict a dict)--\
        [] -> dict
-}

{-
dial (a,b) n = (n*a, n*b)

norm (a,b) =
    let
        len = sqrt (a*a + b*b)
    in
        ((a/len),(b/len))

len (a,b) = sqrt (a*a + b*b)

-}
