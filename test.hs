import Text.JSON
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import qualified Crypto.Hash.SHA1 as SHA1

import System.IO  

j a = decode a :: Result JSValue

isOk (Ok _) = True
isOk _ = False


enumerate = zip [0..]


invert xs = let
    invert' ((a,y:ys):xs) = (y, a) : invert' ((a,ys):xs)
    invert' ((a,[]):xs) = invert' xs
    invert' [] = []
    collapsonate ((a,b):xs) m = collapsonate xs $ Map.insertWith (++) a [b] m
    collapsonate [] m = m
    in
    collapsonate (invert' xs) Map.empty

-- lookup do structure let specifics

loop contents nodeid = do
    let
        nodes = filter (\x -> x!"type" == "node") elements
        nodesMap = Map.fromList $ map (\x -> (x!"id", x) ::(Int, JSValue)) $ nodes
        ways = filter (\x->x!"type"=="way") elements
        waysMap = Map.fromList $ map (\x -> (x!"id", x) ::(Int, JSValue)) $ ways
        waysByNode = invert $ map (\x -> (x!"id", x!"nodes")) ways :: Map.Map Int [Int]
        nodesByWay = Map.fromList $ map (\x -> (x!"id", x!"nodes")) ways :: Map.Map Int [Int]
        (JSObject x) ! y = let Ok a = valFromObj y x in a
        (JSObject x) ? y =
            isOk (valFromObj y x :: Result JSValue)
        Ok json = j contents
        elements = json ! "elements" :: [JSValue] 

        nextnodes n xs = let
            nextnodes' n (x1:x2:xs) Nothing = if x1 == n then [ x2 ] else nextnodes' n (x2:xs) (Just x1)
            nextnodes' n (x1:x2:xs) (Just x0) = if x1 == n then [ x0, x2 ] else nextnodes' n (x2:xs) (Just x1)
            nextnodes' _ (x1:[]) (Just x0) = if x1 == n then [ x0 ] else []
            nextnodes' _ (x1:[]) Nothing = []
            nextnodes' n [] _ = []
            in nextnodes' n xs Nothing 
        stuff contents nodeid =
            concat $ map (nextnodes nodeid) $ map (nodesByWay Map.!) (waysByNode Map.! nodeid)
        c = stuff contents nodeid
        node = nodesMap Map.! nodeid
        direction :: Double -> Double -> String
        direction x y
            | xl && x>0 = "East"
            | xl && x<0 = "West"
            | not xl && y>0 = "North"
            | not xl && y<0 = "South"
            where xl = (abs x) > (abs y)

    --print $ enumerate c
    --print $ SHA1.hash $ encode nodeid
    print $ nodeid
    print $ enumerate $ map (\x -> direction (x!"lon"-node!"lon") (x!"lat"-node!"lat") ) $ map (nodesMap Map.!) c
    test <- getLine
    case lookup (read test) (enumerate c) of
        Just nextnode -> loop contents nextnode
        Nothing -> loop contents nodeid

main = do
    handle <- openFile "oldham.json" ReadMode  
    contents <- hGetContents handle 
    putStrLn "You are in a maze of twisty little passages, all alike"
    loop contents 784008757
