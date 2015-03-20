module IxArray
    ( new           -- : string
    , length        -- : Int -> Int
    , get
    , set 
    , getArrayList
    , fromList 
    , Indexable
    , IxArray
    , listToIxArray
    , updateIxArrayFromList
    , ixSetElem_ArrayFromList 
    , updateIxArrayFromProvidedIxList 
    ) where

import String
import List
import Array as A
import Maybe (..)
version : String
version = "0.1.0"

out_Of_Range : Int
out_Of_Range = -2

unused : Int
unused = -1

mid : Int -> Int
mid a = a
 

type alias Indexable a =
    { a | id : Int }

type IxArray a = Ix
   { ix  : A.Array Int
   , arr : A.Array a
   }

--new : Int -> IxArray a
new n =
    let
        ix = A.initialize n (always unused)
    in
        Ix { ix = ix, arr = A.empty }

--length : IxArray a -> Int
length (Ix ixa) = A.length ixa.ix

-- index was in range and that cell has already been set
-- (this assumes that you have already gone through the index)
plainUpdate n el ixa =
    let
        arr = A.set n el ixa.arr
    in
       Ix { ixa | arr <- arr }

-- index is less than index array size, but cell has never been set
-- so push the value on the end of the value array, then point
-- the ix array there.
-- N.B. This will fail miserably if n is out of range on ix,
-- and by fail I mean the ix update will fail and arr and ix will
-- be unusably out of sync
newInsert n el ixa =
    let
        arr = A.push el ixa.arr
        ix = A.set n ((A.length arr)-1) ixa.ix
    in
        Ix { ixa | ix <- ix, arr <- arr } -- does this save any allocation?



-- both ix and array need to be pushed on to
addOnEnd n el ixa =
    let
        endPos = A.length ixa.arr
        arr = A.push el ixa.arr
        ix = A.push endPos ixa.ix
    in
        Ix { ixa | arr <- arr, ix <- ix }


-- the fiddly one
extendIndex n el ixa =
    let
        diff = n - ((A.length ixa.ix) - 1)
    in
        if  | (diff == 1) -> addOnEnd n el ixa
            | otherwise ->
                let
                    ext = A.initialize (diff-1) (always unused)
                in
                    addOnEnd n el { ixa | ix <- (A.append ixa.ix ext) }



--get : Int -> IxArray a -> Maybe a
get n (Ix ixa) =
    case (A.get n ixa.ix) of
        Nothing  -> Nothing
        Just -1  -> Nothing
        Just pos -> A.get pos ixa.arr


--set : Int -> a -> IxArray a -> IxArray a
set n el (Ix ixa) =
    let
        index = withDefault out_Of_Range (A.get n ixa.ix)
    in
        if  | (index == out_Of_Range) -> extendIndex n el ixa
            | (index == unused) -> newInsert n el ixa
            | otherwise -> plainUpdate index el ixa

--map : (a -> b) -> IxArray a -> IxArray b
map f ixa = {ixa | arr <- A.map f ixa.arr}




{-
 | 
 | just creating an array from a list is probably less
 | useful than it looks, since then the index are arbitrary,
 | but c'est la vie
 | 
 -}

fromList = ixArrayFromList

-- given a list of elements
ixArrayFromList : List a -> IxArray a
ixArrayFromList elems =
    let
        index = 0
    in
        ixArrayFromList' index (new 1) elems        


{-
 | a little naked naked recursion never hurt anyone
 -}
ixArrayFromList' : Int -> IxArray a -> List a -> IxArray a
ixArrayFromList' index ixarr elems =
    case elems of
        []      -> ixarr
        h::tail -> ixArrayFromList' (index+1) (set index h ixarr) tail




-- given a list of elements and a index setting function...
ixSetElem_ArrayFromList : (Int -> a -> a) -> List a -> IxArray a
ixSetElem_ArrayFromList setIndex elems =
    let
        index = 0
    in
        ixSetElem_ArrayFromList' setIndex 0 (new 1) elems        


{-
 | a little naked naked recursion never hurt anyone
 -}
ixSetElem_ArrayFromList' : (Int -> a ->a) -> Int -> IxArray a -> List a -> IxArray a
ixSetElem_ArrayFromList' setIndex index ixarr elems =
    case elems of
        []      -> ixarr
        h::tail ->  ixSetElem_ArrayFromList' setIndex (index+1) (set index (setIndex index h) ixarr) tail






{-
 | 
 | possibly more useful, listToIxArray and updateIxArayFromList
 | work with any [(Indexable a)]
 | 
 | 
 -}
updateIxArrayFromList : IxArray (Indexable a) -> List (Indexable a) -> IxArray (Indexable a) 
updateIxArrayFromList ixarr elems =
    let
        foldfunc elm ixa = set elm.id elm ixa
    in
        List.foldl foldfunc ixarr elems


{-
 | 
 | as long as functions A -> B that are stored in an A 
 | need to be hidden behind a data constructor, this is
 | as pretty as it gets... :{
 | 
 -}
updateIxArrayFromProvidedIxList : (a -> Int) -> IxArray a -> List a -> IxArray a 
updateIxArrayFromProvidedIxList getIx ixarr elems =
    let
        foldfunc getIx elem ixa = set (getIx elem) elem ixa
    in
        List.foldl (foldfunc getIx) ixarr elems


{-
 | 
 | if the record doesn't have to be hidden behind a  
 | a constructor, this is actually pretty
 | 
 -}
listToIxArray : List (Indexable a) -> IxArray (Indexable a) 
listToIxArray nodes = updateIxArrayFromList (new 1) nodes



pushOn el (Ix ixa) = (Ix {ixa | arr <- (A.push el ixa.arr)}) 
getRec (Ix rec) = rec
getArrayList (Ix ixa) = A.toList ixa.arr

