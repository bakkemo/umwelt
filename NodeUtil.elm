module NodeUtil
    ( moveNode
    , moveNodeWD
    , moveNodeAsParentWD
    , moveNodeCase
    , getPos
    , getPosInt
    , getHOVById
    , getHOVByName
    , getNodeByIxId
    , getDroppedHovs
    , getDroppedTouches
    , getQTInfo
    , touchToHOVId
    , touchBaseToHOVId
    , hovToUpdatedIxNode 
    , calcQTPrefix
    , updateNodePrefix
    , updateNodeQTData 
    , prefixToPoint
    , dbh
    , delete
    , qtIntersections
    , qtPairs
    , getBr
    , getTl
    , getBoundingRect
    , updateNodeInfo
    , updateNodeInfoScene
    ) where



import DataTypes (..)
import Touch (Touch, touches)
import List
import Graphics.Collage (..)
import Debug
import Array as A
import IxArray as Ix
import Dict as D
import Basics as B
import String as S
import Bitwise as BW
import Color (..)
import List as L
import Maybe (..)

-- since nodes take into account touches dx,dy component
-- take them into acccount when placing forms
moveNode : Node -> Form
moveNode {x,y,dx,dy,el} = 
    let
        rx= toFloat x + dx
        ry= toFloat y - dy
    in    
        move (rx, ry) el


getPos {x,y,dx,dy,px,py} =
    let
        rx= toFloat x + dx + px
        ry= toFloat y + py - dy
    in    
        (rx, ry)

getPosInt {x,y,dx,dy,px,py} =
    let
        rx= toFloat x + dx + px
        ry= toFloat y + py - dy
    in    
        (round rx, round ry)


boundPath (x,y) (tx, ty) (bx, by) =
    let
        lx = (x + toFloat tx)
        ly = (y + toFloat ty)
        rx = (x + toFloat bx)
        ry = (y + toFloat by)
    in
       path [(lx,ly), (rx,ly), (rx,ry), (lx, ry), (lx,ly)]


getDisPrefix (x,y) (dx,dy) =
        calcQTPrefix (x+dx) (y+dy) -150 150 150 -150 3 "" 







-- this ignores child nodes, which forces parent nodes
-- to handle "drawing" child nodes (exact semantics TBD)
moveNodeCase : Ix.IxArray Node -> Node -> List Form
moveNodeCase ixNodes node =
    if | (node.id == node.parentId) && (node.childIds == []) -> moveNodeWD node -- self parent, no children
       | (node.id == node.parentId) -> moveNodeAsParentWD ixNodes node -- parent with children, so handles "draw" of children
       | otherwise -> []  -- strictly a child node...handled by previous case
{-                let
                    a = Debug.log "skip" node.id
                in
                    []      
-}



addCenter : (Float, Float) -> Node -> Node
addCenter (cx,cy) node = { node | px <- cx, py <- cy } 


moveNodeAsParentWD : Ix.IxArray Node -> Node -> List Form
moveNodeAsParentWD ixNodes node {-id,x,y,dx,dy,el,prefix,tl,br, qt_tlp, qt_trp, qt_blp, qt_brp, childIds-} = 
    let
        bbPath = traced (dotted red) (boundPath (getPos node) node.tl node.br)
        nodeCent = filled red (circle 3)
        (cx,cy) = getPos node
        a : List Form
        a = (L.concat (L.map (moveNodeWD << (addCenter (cx,cy))) (getChildNodes ixNodes node.childIds)))
    in
        [move (cx,cy) nodeCent, bbPath] ++ a

moveNodeWD : Node -> List Form
moveNodeWD node {-id,x,y,dx,dy,el,prefix,tl,br, qt_tlp, qt_trp, qt_blp, qt_brp-} = 
    let
        --a = if (id ==1) then Debug.log "draw" id else 0
        (rx, ry) = getPos node
        (nx, ny) = prefixToPoint 300 300 node.prefix
        (x1,y1,x2,y2,x3,y3,x4,y4) = prefixToBB 300 300 node.prefix
        bbPath = traced (dotted black) (boundPath (rx,ry) node.tl node.br)
        qtCenter = filled red (circle 3)
        qtVert = filled purple (circle 3)
        nodeCent = filled black (circle 3)
        (tx,ty) = node.tl
        (bx,by) = node.br
        x = round rx
        y = round ry
        pTL = getDisPrefix (x,y) (tx,ty)
        pTR = getDisPrefix (x,y) (bx,ty)
        pBL = getDisPrefix (x,y) (tx,by)
        pBR = getDisPrefix (x,y) (bx,by)
        
        mark = filled purple (circle 3)
        tlmark = move (prefixToPoint 300 300 node.qt_tlp) mark
        trmark = move (prefixToPoint 300 300 node.qt_trp) mark
        blmark = move (prefixToPoint 300 300 node.qt_blp) mark
        brmark = move (prefixToPoint 300 300 node.qt_brp) mark

        qtbbTL = alpha 0.3 (filled lightYellow (prefixToBBPoly 300 300 pTL))
        qtbbTR = alpha 0.3 (filled lightYellow (prefixToBBPoly 300 300 pTR))
        qtbbBL = alpha 0.3 (filled lightYellow (prefixToBBPoly 300 300 pBL))
        qtbbBR = alpha 0.3 (filled lightYellow (prefixToBBPoly 300 300 pBR))

    in    
        [ move (rx, ry) node.el, move (rx,ry) nodeCent, bbPath, qtbbTL, qtbbTR, qtbbBL, qtbbBR
        {-, move (nx, ny) qtCenter, move (x1,y1) qtVert, move (x2,y2) qtVert, move (x3,y3) qtVert, move (x4,y4) qtVert-}]


minkowski verts1 verts2 =
    let
        a = case verts1 of
            h :: rest -> (L.map (sub h) verts2) ++ minkowski rest verts2
            [] -> []
    in
        a

dist (x1,y1) (x2,y2) = 
    let
        xsq = (x1-x2)
        ysq = (y1-y2)
    in
       sqrt ((xsq*xsq)+(ysq*ysq))



{-
fmove thing pt = move pt thing 


drawMink depth =
    let
        cent = filled red (circle 2)
        vert = filled purple (circle 3)
        --hull = minkowski testMink.bp1 testMink.bp2
        hull = [(-2.0,-11.0),(18,13),(45,18),(58,-12),(43,-37),(10,-31)]
        pts = map (fmove vert) testMink.hull 
        mink = alpha 0.3 (filled lightBlue (polygon hull)) 
        fee = alpha 0.3 (filled lightBlue (polygon [(-50,-50)])) 
        (f,(sim,d)) = doSimplexD depth



    in
        [move (0,0) mink, move (0,0) cent] ++ pts ++ fee
-}



-- very coarse touch/mouse position to node mapping
-- TODO need to be able to specify secondary fine grained
-- hit test
hitCheck : (Int, Int) -> Node{-Rect a-} -> Bool
hitCheck (ptx,pty) node {-x,y,w,h-} =
    let 
        (tlx, tly) = node.tl
        (brx, bry) = node.br
        dx = (brx - tlx) // 2
        dy = (tly - bry) // 2
       -- dx = node.w
       -- dy = node.h
{-
        a = if (node.id == 0)
            then
                let
                    b = Debug.log "w" (dx,dx')
                    c = Debug.log "h" (dy, dy')
                in (-1,-1)
            else
                (-1,-1)   
-}
    in
        if (ptx >= node.x - dx) 
        && (ptx <= node.x + dx)   
        && (pty >= node.y - dy) 
        && (pty <= node.y + dy)
        && (node.id == node.parentId)
    then 
        True
    else 
        False


-- really just useful when the nodeset is implemented
-- as a list
--TODO:LIST:TODO
filterNodes : (Int,Int) -> List Node -> List Node--[Rect a] -> [Rect a]
filterNodes (x,y) nodes = 
    (L.filter (hitCheck (x,y)) nodes)

-- given x and y coords (and a touch Id) from a Touch,
-- return the first correstponding Hov element
nodeCheck : NodeSet -> (Int, Int, Int) -> List HOV  
nodeCheck nodes (x,y,tid) =
    let
        hitNodes = (filterNodes ((x,y)) nodes)  
    in    
        case hitNodes of   
        []   -> [None]
        _    -> L.map (nodeToHOV tid) hitNodes




nodeToHOV : Int -> Node -> HOV
nodeToHOV tid node = Hov (node.id, node.name, (node.x, node.y)) tid



getHOVById : Int -> List HOV -> HOV
getHOVById id hovs =
    case hovs of
        [] -> None
        None :: t -> getHOVById id t
        (Hov (nid,name,(x,y)) tid) :: t -> if (id == nid) then Hov (nid, name, (x,y)) tid else getHOVById id t


getHOVByName : String -> List HOV -> HOV
getHOVByName hname hovs =
    case hovs of
        [] -> None
        None :: t -> getHOVByName hname t
        (Hov (nid,name,(x,y)) tid) :: t -> if (hname == name) then Hov (nid, name, (x,y)) tid else getHOVByName name t


-- by construction, touches MUST contain the 
-- id we're looking for
getTouchById : Int -> List Touch -> Touch
getTouchById tid touches =
    case touches of
        [] -> Debug.log "WTF!!!!!!" {id=0,x=0,y=0,x0=0,y0=0,t0=0}
        (h::rest)  ->  if (h.id == tid) then h else getTouchById tid rest


-- again, only useful when NodeSet is a list
--TODO:LIST:TODO
isNode : NodeId -> Node -> Bool
isNode nid node = (node.id == nid)


-- currently total, since only called with
-- a valid id
--TODO:LIST:TODO
getNodeById : NodeId -> List Node -> Node
getNodeById nid nodes =
    List.head (L.filter (isNode nid) nodes)


getNodeByIxId : Ix.IxArray Node -> NodeId -> Maybe Node
getNodeByIxId ixnodes nid =
   Ix.get nid ixnodes 


--TODO:LIST:TODO
dropNodeById : NodeId -> List Node -> List Node
dropNodeById nid nodes = L.filter (not <<  (isNode nid)) nodes

-- a foldable dropNode
--TODO:LIST:TODO
dropNode : List Node -> Node -> List Node
dropNode nodes n = L.filter (\nd -> not (n.id == nd.id)) nodes



-- a set of NodeSet-as-list utility functions
--TODO:LIST:TODO
delete = deleteBy heq

--TODO:LIST:TODO
diff : List HOV -> List HOV -> List HOV
diff = L.foldl (B.flip delete)

--TODO:LIST:TODO
deleteBy f ns n =
    case ns of
        []    -> []
        t::ts -> if (f n t) then ts else t :: deleteBy f ts n

--TODO:LIST:TODO
deleteFirstBy f ns ds  = List.foldl (B.flip (deleteBy f)) ns ds

eq = \x y -> (x.id == y.id)
heq = \x y -> (x==y)


-- a list is not a set :(
--TODO:LIST:TODO used to get the untouched nodes
dropNodes : List Node -> List Node -> List Node
dropNodes nodes touched = deleteFirstBy eq nodes touched

-- sic
--TODO:LIST:TODO
inList : (a->a->Bool) -> a -> List a -> Bool
inList eq el lst =
    case lst of
        []   -> False
        h::t -> if (eq h el) then True else inList eq el t

-- builds up a list of dropped elements for a fold
notInList : (a -> a -> Bool) -> List a -> a -> List a -> List a
notInList eq list el acc =
    case list of
        [] -> el :: acc
        h :: t ->
            if (eq h el) then acc else notInList eq t el acc


-- needed regardless....HOVS should always be a list
-- at most like 5?
getDroppedHovs old new = List.foldl (notInList heq new) [] old
getDroppedTouches old new =  List.foldl (notInList eq new) [] old 


dbg : List Touch -> List Touch
dbg touches =
    case touches of
        [] -> touches
        _  -> Debug.log "dts" touches


dbh : HOV -> HOV
dbh hov =
    case hov of
        None -> hov
        _  -> Debug.log "nu_hdb" hov


dbgh : List HOV -> List HOV
dbgh hovs =
    case hovs of
        [] -> hovs
        _  -> Debug.log "dtargs" hovs



getNodeIds : EventSet -> List NodeId
getNodeIds (ES eventSet) = eventSet.nodeIds


collectEventIds : List EventSet -> List NodeId
collectEventIds es = 
        L.concat (L.map getNodeIds es) 
    |>  L.map (\n -> (n,n))
    |>  D.fromList
    |>  D.keys




-- given a list of nodes, and a list of selector Ids (guaranteed to be in the
-- list of nodes), create an IxArray of nodes with those Ids
getEventNodeArray : List Node -> List NodeId -> Ix.IxArray Node
getEventNodeArray nodes nodeIds =
    let
        --foldfunc : NodeId -> IxArray Node -> IxArray Node
        foldfunc id ixa = Ix.set id (getNodeById id nodes) ixa
    in
        List.foldl foldfunc (Ix.new 1) nodeIds


-- Stupid to care about cycles at this point, but leaving
-- the two different "relativeTouchxxxInfo" and "touchxxxHovId"
-- since I don't know what the compiler will do to
-- something unified that has to tear apart and reconstrct tuples
-- all over...especially since I don't know what
-- it's doing with the polymorphic records in the first place
--
-- bad name for thing that turns a Touch into (x,y,id)
relativeTouchBaseInfo : MouseMap -> BasePositionId a -> (Int, Int, Int)
relativeTouchBaseInfo rmp t =
    let
        (x1,y1) = rmp (t.x0,t.y0) 
    in
        (x1, y1, t.id)

-- touch -> relative mouse position of a collage -> [hover Id]
touchBaseToHOVId : Scene -> BasePositionId a -> List HOV
touchBaseToHOVId scene touch =
    nodeCheck  (Ix.getArrayList scene.ixNodes) ( (relativeTouchBaseInfo scene.rmp touch))  


-- bad name for thing that turns a Touch into (x,y,id)
relativeTouchInfo : MouseMap -> PositionId a -> (Int, Int, Int)
relativeTouchInfo rmp t =
    let
        (x1,y1) = rmp (t.x,t.y) 
    in
       (x1, y1, t.id)

-- touch -> relative mouse position of a collage -> [hover Id]
touchToHOVId : Scene -> PositionId a -> List HOV
touchToHOVId scene touch =
    nodeCheck (Ix.getArrayList scene.ixNodes) ( (relativeTouchInfo scene.rmp touch))  



-- IxArray version of hovToUpdatednode
hovToUpdatedIxNode : MouseMap -> Ix.IxArray Node -> List HOV -> List Touch -> List Touch -> HOV -> Maybe (List Node)
hovToUpdatedIxNode rmp nodes dropped touches droppedTouches hov =
    case hov of
        None -> Nothing 
        (Hov (nid, name, (x,y)) tid) ->
            let
                (Just node) = Ix.get nid nodes
                (DH dragFunc) = node.dh
                childNodes = getChildNodes nodes node.childIds
            in
                case inList heq hov dropped of 
                    False ->    
                        let
                            touch = getTouchById tid touches
                        in      
                            Just (dragFunc node childNodes touch False)
             
                    True ->
                        let
                            touch = getTouchById tid droppedTouches
                        in      
                            Just (dragFunc node childNodes touch True)

{-
-}
        --z = Debug.log "param" (x,y,lx,ly,rx,ry, curDepth, prefix)\
        --a = Debug.log "w" newx\
        --b = Debug.log "h" newy\
calcQTPrefix : Int -> Int -> Int -> Int -> Int -> Int -> Int -> String -> String
calcQTPrefix x y lx ly rx ry curDepth prefix =
    let
        newx = lx + ((rx - lx) // 2)
        newy = ry + ((ly - ry) // 2)
    in
        if | (curDepth == 0) -> prefix
           | otherwise ->
                if  | ((x >= newx) && (y >= newy)) -> calcQTPrefix x y newx ly rx newy (curDepth-1) ("1" ++ prefix)
                    | ((x >= newx) && (y < newy)) ->  calcQTPrefix x y newx newy rx ry (curDepth-1) ("4" ++ prefix)
                    | ((x < newx) && (y >= newy)) ->  calcQTPrefix x y lx ly newx newy (curDepth-1) ("2" ++ prefix)
                    | ((x < newx) && (y < newy)) ->   calcQTPrefix x y lx newy newx ry (curDepth-1) ("3" ++ prefix)



updateNodePrefix scenew sceneh depth node =
    let
        x = scenew // 2
        y = sceneh // 2
    in
        { node | prefix <- calcQTPrefix node.x node.y -x y x -y depth "" }

updateNodeQTData scene_width scene_height depth node =
    let
        sx = scene_width // 2
        sy = scene_height // 2
        (tx,ty) = node.tl
        (bx,by) = node.br
        (curx, cury) = getPosInt node

        tlp = calcQTPrefix (curx+tx) (cury+ty) -sx sy sx -sy depth "" 
        trp = calcQTPrefix (curx+bx) (cury+ty) -sx sy sx -sy depth "" 
        blp = calcQTPrefix (curx+tx) (cury+by) -sx sy sx -sy depth "" 
        brp = calcQTPrefix (curx+bx) (cury+by) -sx sy sx -sy depth "" 
        --qtd = node.qtData 
        --nd = { node | qtData <- { qtd |  tr <- trp, tl <- trp }} 
        --qqtd = nd.qtData 
    in
        --nd
        { node | qt_trp <- trp, qt_tlp <- tlp, qt_blp <- blp, qt_brp <- brp  }

adjustValue : Float -> Float -> Float -> Float -> Char -> (Float, Float)
adjustValue w h x y c =
   case c of
       '1' -> (x+w, y+h)
       '2' -> (x-w, y+h)
       '3' -> (x-w, y-h)
       '4' -> (x+w, y-h)
    
walkPrefix : Float -> Float -> Float -> Float -> String -> (Float, Float)
walkPrefix w h x y p =
    case (S.uncons p) of
        Nothing -> (x, y)
        Just (c, rest) ->
           let
               nw = w / 2
               nh = h / 2
               (nx, ny) = adjustValue nw nh x y c
           in
               walkPrefix nw nh nx ny rest



prefixToPoint : Int -> Int -> String -> (Float, Float)
prefixToPoint w h prefix =
    let
        s = S.reverse prefix
        nw = (toFloat w) / 2
        nh = (toFloat h) / 2
    in
       walkPrefix nw nh 0 0 s



walkPrefixBB : Float -> Float -> Float -> Float -> String -> (Float, Float, Float, Float)
walkPrefixBB w h x y p =
    case (S.uncons p) of
        Nothing -> (x, y, w, h)
        Just (c, rest) ->
           let
               nw = w / 2
               nh = h / 2
               (nx, ny) = adjustValue nw nh x y c
           in
               walkPrefixBB nw nh nx ny rest



prefixToBB : Int -> Int -> String -> (Float, Float, Float, Float, Float, Float, Float, Float)
prefixToBB w h prefix =
    let
        s = S.reverse prefix
        nw = (toFloat w) / 2
        nh = (toFloat h) / 2
        (x1,y1,w1,h1) = walkPrefixBB nw nh 0 0 s
    in
        (x1-w1, y1+h1, x1+w1, y1+h1, x1+w1, y1-h1, x1-w1, y1-h1) 


prefixToBBPoly w h prefix =
    let
        (x1,y1,x2,y2,x3,y3,x4,y4) = prefixToBB w h prefix
    in
       polygon [(x1,y1),(x2,y2),(x3,y3),(x4,y4)]



prefixToHash prefix =
    prefixToHash' prefix 0 0

prefixToHash' : String -> Int -> Int -> Int
prefixToHash' prefix ix val =
    case S.uncons prefix of
        Nothing -> val
        Just (digit, rest) ->
            prefixToHash' rest (ix+1) (val+(ixval digit ix))



ixval digit ix =
    let
        lshift = 2*ix
    in
        case digit of
            '1' -> 0
            '2' -> BW.shiftLeft 1 lshift
            '3' -> BW.shiftLeft 2 lshift
            '4' -> BW.shiftLeft 3 lshift
            otherwise -> 0



qtFold : Node -> D.Dict Int (List Int) -> D.Dict Int (List Int)
qtFold node qtInfo =
    let
        tl_hash = prefixToHash node.qt_tlp
        tr_hash = prefixToHash node.qt_trp
        bl_hash = prefixToHash node.qt_blp
        br_hash = prefixToHash node.qt_brp

        tl_data = withDefault [] (D.get tl_hash qtInfo)
        tr_data = withDefault [] (D.get tr_hash qtInfo)
        bl_data = withDefault [] (D.get bl_hash qtInfo)
        br_data = withDefault [] (D.get br_hash qtInfo)

        d1 = D.insert tl_hash (node.id :: tl_data) qtInfo
        d2 = D.insert tr_hash (node.id :: tr_data) d1
        d3 = D.insert bl_hash (node.id :: bl_data) d2
        d4 = D.insert br_hash (node.id :: br_data) d3
        --a = Debug.log "d" d4
    in
        d4
        

getQTInfo : List Node -> D.Dict Int (List Int)
getQTInfo nodes = L.foldr qtFold D.empty nodes

qtFilter : (Int, (List Int)) -> Bool
qtFilter (a, l) = ((L.length l) > 1)

qtMap : (Int, (List Int)) -> List Int
qtMap (a, l) = l 

qtIntersections : D.Dict Int (List Int) -> List (List Int)
qtIntersections dict = 
    let
        list = D.toList dict
    in
        L.map qtMap (L.filter qtFilter list)


-- enforce the invarient (a < b) in (a,b)
qtOrderedTuple : Int -> Int -> (Int, Int)
qtOrderedTuple a b =
    if | (a < b)    -> (a,b)
       | otherwise  -> (b,a)



qtPairs : List Int -> List (Int, Int)
qtPairs list = 
    case list of
        (h :: rest) -> (L.map2 qtOrderedTuple (L.repeat (L.length rest) h) rest) ++ qtPairs rest
        [] -> []




updateNodeInfoScene scene node =
    updateNodePrefix scene.w scene.h scene.qtDepth (updateNodeQTData scene.w scene.h scene.qtDepth (updateNodeBoundInfo node))

updateNodeInfo w h d node =
    updateNodePrefix w h d (updateNodeQTData w h d (updateNodeBoundInfo node))


updateNodeBoundInfo node =
    if | (node.bound == Circ) -> node
       | otherwise -> updatePolyInfo node 
       
updatePolyInfo : Node -> Node
updatePolyInfo node =
    let
        (tlx,tly) = getTl (A.toList node.boundPoly) 
        (brx,bry) = getBr (A.toList node.boundPoly) 
        w = round ((brx - tlx) /2)
        h = round ((tly - bry) /2)
    in
        { node  | tl <- (round tlx, round tly)
                , br <- (round brx, round bry)
                , w <- w
                , h <- h }  
                

tlFold : (Float, Float) -> (Float, Float) -> (Float, Float)
tlFold (x,y) (tlx, tly) =
    let
        nx = if (x < tlx) then x else tlx
        ny = if (y > tly) then y else tly
    in
        (nx, ny)

brFold : (Float, Float) -> (Float, Float) -> (Float, Float)
brFold (x,y) (brx, bry) =
    let
        nx = if (x > brx) then x else brx
        ny = if (y < bry) then y else bry
    in
        (nx, ny)

{-
tlF : (Float, Float) -> (Float, Float) -> (Float, Float)
tlF (x,y) (tlx, tly) =\
    let\
        nx = if (x < tlx) then x else tlx\
        ny = if (y > tly) then y else tly\
    in\
        (nx, ny)

brF : (Float, Float) -> (Float, Float) -> (Float, Float)
brF (x,y) (brx, bry) =\
    let\
        nx = if (x > brx) then x else brx\
        ny = if (y < bry) then y else bry\
    in\
        (nx, ny)

-}


getTl : List (Float, Float) -> (Float, Float)
getTl hull = L.foldl tlFold (10000,-10000) hull


getBr : List (Float, Float) -> (Float, Float)
getBr hull = L.foldl brFold (-10000,10000) hull




add (ax, ay) (bx,by) = (bx+ax, by+ay)
sub (ax, ay) (bx,by) = (bx-ax, by-ay)
{-
 - to calculate a bounding box for a composite node
 - the individual bounding boxes for the component bounding
 - boxes must be absolutly positioned. thus we need to 
 - add the center to the component hull to move to absolute
 - positioning
 -}
translateBB : Node -> List (Float,Float)
translateBB node =
    let
        bb = A.toList node.boundPoly
        center = (toFloat node.x, toFloat node.y)
        --a = Debug.log "t" (map (add center) bb)
        --b = Debug.log "c" center
    in
        L.map (add center) bb



getChildNodes : Ix.IxArray Node -> List Int -> List Node
getChildNodes ixNodes children = L.filterMap ((flip Ix.get) ixNodes) children


-- given a list of child IDs, compute the bounding rect of their union
getBoundingRect : Ix.IxArray Node -> List Int -> ((Int, Int), (Int, Int))
getBoundingRect ixNodes children =
    let
        hulls = L.concat (L.map translateBB (L.filterMap ((flip Ix.get) ixNodes) children))  
        (tlx,tly) = getTl hulls
        --a = Debug.log "tl" (tlx,tly)
        (brx,bry) = getBr hulls
        --b = Debug.log "br" (brx,bry)
        --c = Debug.log "cids" children
    in
        ((round tlx, round tly), (round brx, round bry)) 





