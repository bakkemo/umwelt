module Setup
    ( updateTags
    ) where

import Debug
import DataTypes (..)
import Dict as D
import List as L
import IxArray as Ix
import NodeUtil (getPos, getNodeByIxId, updateNodePrefix, getBoundingRect)
--import EventUtils (activate)
import Maybe (..)
import Signal ((<~), constant)
import Time (timestamp)

{-
 |
 | get and set the id field of Nodes and EventSets
 | used to update node/event/trigger relationships
 |
 -}
getNodeId : Node -> Int
getNodeId node = node.id

setNodeIx : Int -> Node -> Node
setNodeIx n node =
    { node | id <- n }

getEventSetIx : EventSet -> Int
getEventSetIx (ES rec) = rec.id

setEventSetIx : Int -> EventSet -> EventSet
setEventSetIx n (ES rec) =
    ES { rec | id <- n }



{-
 |
 | get the name / id association for nodes and eventsets
 | you don't pick the IDs, but you do pick the names
 | a name -> id map gets built out for triggers and EventsSets
 | to use
 |
 -}

assocNode node = (node.name, node.id)
invAssocNode node = (node.id, node.name)
assocEventSet (ES rec) = (rec.name, rec.id)
invAssocEventSet (ES rec) = (rec.id, rec.name)

assocFoldFunc : (String, Int) -> D.Dict String Int -> D.Dict String Int
assocFoldFunc (name, id) dict =
    let
        val = D.get name dict
    in
        case val of
            Nothing     -> D.insert name id dict
            (Just old)  ->
                let
                    msg = "replacing assoc dict key " ++ (toString name)  ++ ": " ++ (toString old) ++ " -> " ++ (toString id)
                    a = Debug.log "WTF" msg
                in
                    D.insert name id dict


getNameDict : List (String, Int) -> D.Dict String Int
getNameDict assoc =
    L.foldl assocFoldFunc D.empty assoc    


{-
 |
 | pull the (name, Id) tuples from the nodes and EventSets
 | and put them in a Dict
 |
 -}
setNamesToIds : Ix.IxArray Node -> Ix.IxArray EventSet -> (D.Dict String Int, D.Dict Int String, D.Dict Int String)
setNamesToIds ixNodes ixEventSets =
    let
        nodes = Ix.getArrayList ixNodes
        events = Ix.getArrayList ixEventSets
        namesAndIds = getNameDict ((L.map assocNode nodes) ++ (L.map assocEventSet events))
        nodeIdsToNames = D.fromList (L.map invAssocNode nodes)
        eventIdsToNames = D.fromList (L.map invAssocEventSet events)
    in
        (namesAndIds, nodeIdsToNames, eventIdsToNames)    


-- flip the dict lookup so we can map over keys
keyToVal : D.Dict String Int -> String -> Int
keyToVal dict key =
    let
        z = case D.get key dict of
        Nothing -> 
            let
                a = Debug.log "key" key
            in
                1
        (Just x) -> x
    in
        z 





{-
 | once Ids have been assigned and the name/id
 | mapping has been created, we need to go back and
 | provide Ids where events and triggers provided
 | names
 -}
setNodeIdsOnEvents : D.Dict String Int -> EventSet -> EventSet
setNodeIdsOnEvents nameDict (ES rec) =
    let
        nodeIds = L.map (keyToVal nameDict) rec.tags
    in
        ES { rec | nodeIds <- nodeIds }


setNodeIdsOnTriggers : D.Dict String Int -> EventTrigger -> EventTrigger
setNodeIdsOnTriggers nameDict (ET trig) =
    ET { trig | id <- keyToVal nameDict trig.eventName }


setChildNodeIdsOnNodes : D.Dict String Int -> Node -> Node
setChildNodeIdsOnNodes nameDict node =
    let
        childrenIds = L.map (keyToVal nameDict) node.childNames
        parentId = withDefault -1 (D.get node.parentName nameDict)
    in
        { node | childIds <- childrenIds, parentId <- parentId }


nodeToChildParentMapping : Node -> (Int, List Int)
nodeToChildParentMapping  node = (node.id, node.childIds)

filterEmptyChildren : (Int,List Int) -> Bool
filterEmptyChildren (pid,cids) = (cids /= []) 

setNodeParentId : Int -> Node -> Node
setNodeParentId pid node = {node | parentId <- pid}

nodesToParentChildMapping : List Node -> List (Int, (List Int))
nodesToParentChildMapping nodes =
    let
        list = L.map nodeToChildParentMapping nodes
    in
        L.filter filterEmptyChildren list

parentChildFold : Ix.IxArray Node -> (Int,(List Int)) -> List Node -> List Node
parentChildFold ixNodes (pid, cids) accum =
    let
        nodes = L.filterMap (getNodeByIxId ixNodes) cids -- : [Node] -- the Ints are in ixNodes by construction 
    in
        (L.map (setNodeParentId pid) nodes) ++ accum


setParentChildRelation : Ix.IxArray Node -> List (Int,(List Int)) -> List Node
setParentChildRelation ixNodes maps =
    L.foldl (parentChildFold ixNodes) [] maps



npid : String -> Int -> String
npid name id =
    name ++ "(" ++ (toString id) ++ ") "

alertFold : D.Dict Int String -> Node -> D.Dict Int Int -> D.Dict Int Int
alertFold idToName {id, parentId} dict =
    let
        val = D.get id dict
        nodeName = withDefault "noNodeName" (D.get id idToName)
        newParentName = withDefault "NoNewParentName" (D.get parentId idToName)
    in
        case val of
            Nothing     -> D.insert id parentId dict
            (Just old)  ->
                let
                    oldParentName = withDefault "noOldParentName" (D.get old idToName)
                    msg = (npid nodeName id) ++ ": replacing parent dict value " ++ (npid oldParentName old) ++ "-> " ++ (npid newParentName parentId) 
                    a = Debug.log "WTF" msg
                in
                    D.insert id parentId dict


insertFold : Node -> D.Dict Int Node -> D.Dict Int Node
insertFold node dict =
    D.insert node.id node dict

{-
 - id's have been replaced, but what if there seem to be multiple parents
 _ for a node? We'd like it to be reported...
 -}

validateParentChildRelation : D.Dict Int String -> Ix.IxArray Node -> List Node
validateParentChildRelation idsToNames ixNodes =
    let
        mappings = nodesToParentChildMapping (Ix.getArrayList ixNodes) -- : [(Int, [Int])] 
        assignedParentList = setParentChildRelation ixNodes mappings -- : [mentioned as a child Node]. A node appears as many times as it was mentioned as a child. 
        alertDict = L.foldl (alertFold idsToNames) D.empty assignedParentList
        parentedDict = L.foldl insertFold D.empty assignedParentList --last one wins, obviously
    in
        D.values parentedDict





{-
 - set a parent node parent ID to itself. Also
 - set the bounding box to the cover all subnodes
 _ 
 -}
ensureParentIdMap : Node -> Node
ensureParentIdMap  node =
        if  | (node.parentId >= 0) -> node
            | otherwise ->  { node  | parentId <- node.id
                            }       

sub (ax, ay) (bx,by) = (bx-ax, by-ay)

{-
 - the parent nodes bounding box needs to be adjusted to cover
 - all claimed child nodes (calculated in absolute terms).
 _ once that is done, then the child node centers should be
 - converted from absolute positioning elements for the subnodes
 - to relative (to parent center) positioning elements
 - i.e parameterized off of parent center
 -}
updateBoundingRectMap : Ix.IxArray Node -> Node -> Node
updateBoundingRectMap ixNodes node =
    let
        ((tlx,tly), (brx,bry)) = getBoundingRect ixNodes node.childIds
        w =  (cx - tlx)
        h =  (tly - cy)
        cx = (brx + tlx) // 2 --calculate center relative to bounding box
        cy = (bry + tly) // 2
        rtl = sub (cx,cy) (tlx,tly) -- make bounding box relative to that relative center
        rbr = sub (cx,cy) (brx,bry)
    in
        if  | (L.length node.childIds == 0) -> node
            | otherwise ->  { node  | tl <- rtl--(tlx,tly)
                                    , br <- rbr--(brx,bry)
                                    , w <- w
                                    , h <- h
                                    , x <- cx
                                    , y <- cy
                            }       



relativiseChildCentersMap : Ix.IxArray Node -> Node -> Node
relativiseChildCentersMap ixNodes node =
    let
        (Just pnode) = Ix.get node.parentId ixNodes -- parent ID has been properly set
        (cx,cy) = getPos pnode
        relx = toFloat node.x - cx
        rely = toFloat node.y - cy      -- (relx,rely) = (node.x,node.y) - (pnode.x,pnode.y)
                                -- => (node.x,node.y) = (relx,rely) + (pnode.x, pnode.y)
                                -- => relatve center of child node = parent center + node center
                                -- the plan is to update dx with parents position, so child nodes
                                -- get delegated individual collision detection
    in
        if | (node.id == node.parentId) -> node -- leave self parents alone
           | otherwise -> { node | x  <- round relx
                                 , y  <- round rely
                                 , px <- cx
                                 , py <- cy
                          }  



tup node = (node.id, node.parentId, node.childIds)




    
{-
 | put it all together and update the scene
 |
 | At the end of the day, we will be using arrays to access and update
 | nodes, rather than dicts (for the better O() factor.) This means we would like 
 | a mapping from "names" to ids, so that we can declare events with names of nodes
 | rather than ids (which we would not know in advance unless we did it by hand - in
 | which case you might just use numbers all around.) that would be error prone; lets
 | automate it. Generate an id upon entry into the indexed array, map out a (name, id)
 | pair, and insert into nametoids dict. We use this to set the "list of names as a group"
 | element in the events and triggers (nodes will need this too when composites are introduced)
 | via setNodeIdsOnTriggers and setNodeIdsOnEvents. we could create a polymorphic function to do this
 | if the didn't need need a data type rather than a record (we commited to an act function
 | and cannot have a F : {rec} -> {rec} in a {rec}.).
 -}
updateTags : Scene -> Scene
updateTags scene =
    let
        nodes = Ix.getArrayList scene.ixNodes
        events = Ix.getArrayList scene.ixEventSets
        reindexedNodes =  Ix.ixSetElem_ArrayFromList setNodeIx nodes -- use setNodeIx to set the id on the nodes inserted. is this faster than (Ix.new 1)?
        reindexedES =  Ix.ixSetElem_ArrayFromList setEventSetIx events -- same idea.  is this faster than (Ix.new 1)?
        (nametoids, nodeIdsToNames, eventIdsToNames) = setNamesToIds reindexedNodes reindexedES           -- get mapping dict
        nodesWithChildIds = L.map (setChildNodeIdsOnNodes nametoids) (Ix.getArrayList reindexedNodes) -- fill out the id fields
        updatedIxNodes = Ix.updateIxArrayFromProvidedIxList getNodeId reindexedNodes nodesWithChildIds -- reIxify
        validatedNodes = validateParentChildRelation nodeIdsToNames updatedIxNodes  -- : [mentioned as child: Node]. will alert on dup parents 
        parentedIxNodes = Ix.updateIxArrayFromProvidedIxList getNodeId updatedIxNodes validatedNodes -- mentioned children now have parent ID
        parentedNodes = Ix.getArrayList parentedIxNodes
        adjustedPBBnodes = (L.map (updateBoundingRectMap parentedIxNodes) (L.map ensureParentIdMap parentedNodes))  -- [Node]. Added self parent ID
        adjustedPBBIxNodes = Ix.updateIxArrayFromProvidedIxList getNodeId parentedIxNodes adjustedPBBnodes   -- reIxify
        completedNodes = L.map (relativiseChildCentersMap adjustedPBBIxNodes) (Ix.getArrayList adjustedPBBIxNodes) -- adjust component node centers
        completedIxNodes =   Ix.updateIxArrayFromProvidedIxList getNodeId adjustedPBBIxNodes completedNodes
        eventsWithNodeIds = L.map (setNodeIdsOnEvents nametoids) (Ix.getArrayList reindexedES) -- place node ids on eventSets 
        --activatedEvents = L.map activate eventsWithNodeIds
        ixEventSetsWithNodeIds = Ix.updateIxArrayFromProvidedIxList getEventSetIx reindexedES eventsWithNodeIds --update into the IxArray from updated list 
        updatedTriggers = L.map (setNodeIdsOnTriggers nametoids) scene.triggers -- place node ids on triggers
    in
        { scene | ixNodes <- completedIxNodes --updatedIxNodes--reindexedNodes
        , ixEventSets <- ixEventSetsWithNodeIds
        , triggers <- updatedTriggers -- since all triggers are walked there is no need for random access, so just use list
        , tags <- nametoids } 

