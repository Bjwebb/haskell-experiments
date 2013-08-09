import Text.JSON
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List

import System.IO  

j a = decode a :: Result JSValue

isOk (Ok _) = True
isOk _ = False



collapsonate xs = let
    collapsonate' ((a,b):xs) m = collapsonate' xs $ Map.insertWith (++) a [b] m
    collapsonate' [] m = m
    in
    collapsonate' xs Map.empty 
invert' ((a,y:ys):xs) = (y, a) : invert' ((a,ys):xs)
invert' ((a,[]):xs) = invert' xs
invert' [] = []
invert xs = collapsonate $ invert' xs


stuff contents = let 
    (JSObject x) ! y = let Ok a = valFromObj y x in a
    (JSObject x) ? y =
        isOk (valFromObj y x :: Result JSValue)
    Ok json = j contents
    elements = json ! "elements" :: [JSValue] 

    nodes = filter (\x -> x!"type" == "node") elements
    nodesMap = Map.fromList $ map (\x -> (x!"id", x) ::(Int, JSValue)) $ nodes
    ways = filter (\x->x!"type"=="way") elements
    waysMap = Map.fromList $ map (\x -> (x!"id", x) ::(Int, JSValue)) $ ways

    in
        invert $ map (\x -> (x!"id", x!"nodes")) ways :: Map.Map Int [Int]


        --Map.lookup 26191710 ways
        --map (\x -> (x!) ) elements
        --putStrLn $ foldr (\x y -> x++"\n\n"++y) "" $ map show

  --          $ map (\x -> (x!"geometry"!"coordinates")::[[Double]])
 --           $ filter (\x -> x!"properties"?"highway")
--            $ filter (\x -> (x!"properties"!"@type") == "way") 
       
             --map (\x -> Map.fromList . fromJSObject $ x!"tags" :: Map.Map String String  ) ways -- if x?"tags"
                --then x!"tags" --::(Map.Map String String) 
                --else Map.empty::(Map.Map String String)) 



main = do
    handle <- openFile "oldham.json" ReadMode  
    contents <- hGetContents handle 
    print $ stuff contents

