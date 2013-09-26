import Data.List

data DistancesParents = DistancesParents {distances :: [(Integer, Double)], parents :: [(Integer, Integer)]} deriving (Show)
data DistancesParentsVisitedNext = DistancesParentsVisitedNext [(Integer, Double)] [(Integer, Integer)] [(Integer, Bool)] Integer deriving (Show)

getNotVisited :: [(a, Bool)] -> [a]  
getNotVisited [] = []  
getNotVisited xs = [a | (a,b) <- xs, b == False]

getClosest :: [(a, Double)] -> [a]
getClosest [] = []
getClosest xs = [a | (a,b) <- xs, b == minDistance]
	where minDistance = minimum $ map snd xs
	
getNextVertex :: [(Integer, Bool)] -> [(Integer, Double)] -> Integer
getNextVertex [] [] = -1
getNextVertex xs ys = head (let xs' = getNotVisited xs; ys' = getClosest ys in intersect xs' ys')

getOutcomingVertices :: [(Integer, [Integer])] -> Integer => [Integer]
getOutcomingVertices xs v = head [b | (a,b) <- xs, a == v]

getEdgeWeight :: [(String, Double)] -> Integer -> Integer -> Double
getEdgeWeight w v1 v2 = head [b | (a,b) <- w, a == show v1 ++ "-" ++ show v2]

getVertexDistance :: [(Integer, Double)] -> Integer -> Double
getVertexDistance d v = head [b | (a,b) <- d, a == v]

edgeHasWeight :: [(String, Double)] -> Integer -> Integer -> Bool
edgeHasWeight w v1 v2 = length [b | (a,b) <- w, a == show v1 ++ "-" ++ show v2] > 0

getNextDistance :: [(Integer, Double)] -> [(String, Double)] -> Integer -> Integer -> Double
getNextDistance d w v_next out_v = (let edgeCost = getEdgeWeight w v_next out_v;
									prev_d = getVertexDistance d out_v;
									next_d = getVertexDistance d v_next in
									minimum [prev_d, next_d + edgeCost])
									
getNextDistances :: [(Integer, Double)] -> [(String, Double)] -> Integer -> [(Integer, [Integer])] -> [(Integer, Double)]
getNextDistances d w v_next out_vs = [(a, d_next) | (a,b) <- d, let d_next = (if a `elem` next_out then  getNextDistance d w v_next a else b)]
	where next_out = getOutcomingVertices out_vs v_next	
	
isChangedDistance :: Integer -> Double -> [(Integer, Double)] -> Bool
isChangedDistance c d xs = length [a | (a,b) <- xs, a == c, b /= d] > 0

getVerticesWithChangedDistance :: [(Integer, Double)] -> [(Integer, Double)] -> [Integer]
getVerticesWithChangedDistance xs ys = [a | (a,b) <- xs, isChangedDistance a b ys]

getUpdatedParents :: [(Integer, Integer)] -> [Integer] -> Integer -> [(Integer, Integer)]
getUpdatedParents p_old changed v_next = [(a, new_val) | (a,b) <- p_old, let new_val = (if a `elem` changed then v_next else b)]

getUpdatedDistancesParents :: [(Integer, Double)] -> [(String, Double)] -> [(Integer, [Integer])] -> [(Integer, Integer)] -> Integer -> DistancesParents
getUpdatedDistancesParents d w out_vs p v_next = (let new_d = getNextDistances d w v_next out_vs in (let changed = getVerticesWithChangedDistance new_d d; new_p = getUpdatedParents p changed v_next in DistancesParents new_d new_p))

getUpdatedVisited :: [(Integer, Bool)] -> Integer -> [(Integer, Bool)]
getUpdatedVisited v v_next = [(a,new_val) | (a,b) <- v, let new_val = (if a == v_next then True else b)]

tellDistances :: DistancesParents -> [(Integer, Double)]
tellDistances (DistancesParents distances _ ) = distances  

tellParents :: DistancesParents -> [(Integer, Integer)]
tellParents (DistancesParents _ parents) = parents 

getUpdatedDistancesParentsVisitedNext :: [(Integer, Double)] -> [(String, Double)] -> [(Integer, [Integer])] -> [(Integer, Bool)] -> [(Integer, Integer)] -> DistancesParentsVisitedNext
getUpdatedDistancesParentsVisitedNext d w out_vs v p = (let v_next = getNextVertex v d in (let new_visited = getUpdatedVisited v v_next; new_dp = getUpdatedDistancesParents d w out_vs p v_next in (let new_distances = tellDistances new_dp; new_parents = tellParents new_dp in DistancesParentsVisitedNext new_distances new_parents new_visited v_next)))