module Collision
    ( collision
    , polySupport
    , Mink
    ) where

import List as L
--import Debug

type alias Pt = (Float, Float)
type alias Mink a = (a, (a -> Pt -> Pt))

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

-- perpendicular to a in direction of b (2D case)
perp a b = trip a b a

-- not sure this gets inlined :/
isSameDirection a b = (dot a b) > 0

{-
 - calculate the polygon vertex furthest in the direction of d
 - via list of points 
 -}
polySupport : List Pt -> Pt -> Pt
polySupport list d =
    let
        dotList = L.map (dot d) list
        decorated = (L.map2 (,)) dotList list
    in
        case L.maximum decorated of
            Nothing     -> (-100000,-100000)
            Just (m,p)  -> p


{-
 - calculate the support of the Minkowski difference
 - of two Mink's 
 -}
calcMinkSupport : Mink a -> Mink b -> (Float, Float) -> (Float,Float) 
calcMinkSupport (objA, suppA) (objB, suppB) d =
    let
        p1 = suppA objA (neg d)
        p2 = suppB objB d
     in
        sub p1 p2


-- pass bc as first parameter
getDirectionVector : Pt -> Pt -> Pt
getDirectionVector (x1, y1) (x2, y2) =
    let
        -- try the triple cross product
        d = trip (x1,y1) (x2,y2) (x1,y1)
        collinear = (d == (0,0))

        -- bc and c0 are collinear and gave us a 0 cross product.
        -- inject bc and c into 3d and get ANY perp (the algorithm should
        -- not care in this case) so now crossing bc=(x1,y1,0) with c=(-x2, -y2, 1)
        -- remember the second parameter was c0 = -c
        --
        -- (u2v3 - u3v2)i - (u1v3 - u3v1)j + (who cares)k = bc x c
        -- so...
        -- px = (y1*1 - 0*(-y2)),  py = -(x1*1 - 0*(-x2))   
        -- px = y1                 py = -x1
        -- 
        -- which is a detailed derivation of the obvious :)
    in
        if collinear then (y1, -x1) else d 


collision : Int -> Mink a -> Mink b -> Bool
collision limit minkA minkB =
    let
        d1 = (1.0, 0.0)
        d2 = neg d1
        c = calcMinkSupport minkA minkB d1 
        b = calcMinkSupport minkA minkB d2  
        -- simplex is cb and direction is (cb x c0 x cb)
        cb = from c b
        c0 = neg c
        d = getDirectionVector cb c0
        (intersects, (sim, newD)) = doSimplex limit 0 minkA minkB ([b,c], d)
    in
        intersects



doSimplex : Int -> Int -> Mink a  -> Mink b -> (List Pt, Pt) -> (Bool, (List Pt, Pt)) 
doSimplex limit depth minkA minkB (sim, d) =
    let
        a = (calcMinkSupport minkA minkB d)
        notPastOrig = ((dot a d) < 0)       -- if not past origin, there is no intersection
        b = L.head sim
        (intersects, (newSim, newDir)) = enclosesOrigin a sim
    in
        if notPastOrig then 
            (False, ([], (toFloat depth,toFloat depth)))
        else if intersects then 
            (True, (sim, a))
        else if (depth > limit) then 
            (False, (newSim, newDir)) 
        else 
            doSimplex limit (depth+1) minkA minkB (newSim, newDir)


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
 - simplex is a single point, we will be adding a to the simplex one way or another ([a,b] or [a])
 -}
handle0Simplex a b =
    let
        ab = (from a b)
        a0 = neg a
        (newSim, newDir) = if (isSameDirection ab a0) then ([a,b], (perp ab a0)) else ([a], a0)
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
        if (isSameDirection abp a0) then -- region 4 or 5
            if (isSameDirection ab a0) then (False, ([a,b], abp)) else (False, ([a], a0))
        
        else if (isSameDirection acp a0) then -- region 6 or 5
            if (isSameDirection ac a0) then (False, ([a,c], acp)) else (False, ([a], a0)) 
        
        else 
            (True, ([b,c], a0)) 




