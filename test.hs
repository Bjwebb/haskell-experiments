import Text.JSON
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import qualified Data.ByteString.UTF8 as UTF8
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Base64 as Base64

import System.IO  

j a = decode a :: Result JSValue



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
        (JSObject x) !? y = let a = valFromObj y x in case a of
            Ok b -> Just b
            _ -> Nothing
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
            | a*(abs x) > (abs y) = if x>0 then "East" else "West"
            | a*(abs y) > (abs x) = if y>0 then "North" else "South"
            | x>0 && y>0 = "North East"
            | x>0 && y<0 = "South East"
            | x<0 && y>0 = "North West"
            | x<0 && y<0 = "South West"
            where a = tan(pi/8)

    -- print $ enumerate c
    putStrLn ""
    print $ Base64.encode $ SHA1.hash $ UTF8.fromString $ encode nodeid
    print $ nodeid
    mapM_ print $ enumerate $ map (
        \x -> (
            direction (x!"lon"-node!"lon") (x!"lat"-node!"lat")
            , map (\y->(waysMap Map.! y)!"tags"!?"name" :: Maybe String) $ waysByNode Map.! (x!"id")
            )
        ) $ map (nodesMap Map.!) c
    test <- getLine
    case lookup (read test) (enumerate c) of
        Just nextnode -> loop contents nextnode
        Nothing -> loop contents nodeid

main = do
    handle <- openFile "oldham.json" ReadMode  
    contents <- hGetContents handle 
    putStrLn "You are in a maze of twisty little passages, all alike"
    loop contents 784008757
