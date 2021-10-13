import Data.Bits
import Data.List
import qualified Data.IntSet as IntSet

data Graph = Graph Int Int deriving (Show)

getG :: Graph -> Int
getG (Graph d g) = g

getD :: Graph -> Int
getD (Graph d g) = d

monochromatic :: Int -> Graph
monochromatic n = Graph n
    (foldl (.|.) (0) [(bit (x*n+x)) | x <- [0..n-1]])

symmetric :: Graph -> Bool
symmetric (Graph d g) =
    foldl1 (&&) [((testBit g x) == 
        (testBit g ((x `mod` d)*d+(x `quot` d)))) 
            | x <- [0..((d^2)-1)]]

-- need to filter so that (a,b) = (b,a) for all b and a
createAllG :: Int -> [Graph]
createAllG d =
    (filter (symmetric)
        (filter ((.) ((==) m) ((.) ((.&.) m) (getG)))
            [(Graph d n) | n <- [0..(2^(d^2)-1)]]))
    where m = getG (monochromatic d)

getNeighbors :: Graph -> Int -> [Int]
getNeighbors (Graph d g) n = 
    [x | x <- [0..(d-1)], (g .&. (bit (d*x+n))) /= 0]

getUnNeighbors :: Graph -> Int -> [Int]
getUnNeighbors (Graph d g) n = ([0..(d-1)] 
    \\ ((getNeighbors (Graph d g) n))) ++ [n]

isNeighborhood :: Graph -> [Int] -> Bool
isNeighborhood g l = 
    foldl1 (&&) [((IntSet.fromList l) `IntSet.isSubsetOf`
          (IntSet.fromList (getNeighbors g x))) | x <- l]


isUnNeighborHood :: Graph -> [Int] -> Bool
isUnNeighborHood g l = 
    foldl1 (&&) [((IntSet.fromList l) `IntSet.isSubsetOf`
          (IntSet.fromList (getUnNeighbors g x))) | x <- l]

containsNeighborhoodN :: Graph -> Int -> Bool
containsNeighborhoodN (Graph d g) n =
    foldl (||) (False) [(isNeighborhood (Graph d g) x)
        | x <- (subsequences [0..(d-1)]), (length x) == n]

containsUnNeighborhoodN :: Graph -> Int -> Bool
containsUnNeighborhoodN (Graph d g) n =
    foldl (||) (False) [(isUnNeighborHood (Graph d g) x)
        | x <- (subsequences [0..(d-1)]), (length x) == n]

graphBad :: Graph -> Int -> Int -> Bool
graphBad g n m = not ((containsNeighborhoodN g n) || 
    (containsUnNeighborhoodN g m))

isLowerBound :: Int -> Int -> Int -> Bool
isLowerBound d n m = foldr (||) (False) [(graphBad x n m)
    | x <- (createAllG d) ]
