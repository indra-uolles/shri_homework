import Data.List

--http://habrahabr.ru/post/111361
--http://e-maxx.ru/algo/dijkstra

--an example of use (WinGHCi)
--let d = [(1,0.0),(2,1000.0),(3,1000.0),(4,1000.0),(5,1000.0)]
--let w = [("3-5",10.0), ("5-1",10.0), ("5-3",10.0), ("1-5",10.0), ("1-4",50.0), ("1-3",30.0), ("1-2",10.0), 
--("5-4",30.0), ("4-2",40.0), ("4-3",20.0)]
--let out_vs = [(1,[2,3,4,5]),(2,[]),(3,[5]),(4,[2,3]),(5,[1,3,4])]
--let p = [(1,1),(2,1),(3,1),(4,1),(5,1)]
--let v = [(1,False),(2,False),(3,False),(4,False),(5,False)]
--let source = 1
--let z = getPathsFromSourceToDestinations d w out_vs v p source 
--z
--["path from source to 2 goes through these vertices: [2] and costs 10.0","path from source to 3 goes through these vertices: [5,3] 
--and costs 20.0","path from source to 4 goes through these vertices: [5,4] and costs 40.0","path from source to 5 
--goes through these vertices: [5] and costs 10.0"]

getPathsFromSourceToDestinations ::  [(Integer, Double)] -> [(String, Double)] -> [(Integer, [Integer])] -> [(Integer, Bool)] -> [(Integer, Integer)] -> Integer -> [String]
getPathsFromSourceToDestinations d w out_vs v p source = let g = getCalculatedParentsCosts (d, w, out_vs, v, p, source)
														 in (let parents = ffth_of_six g;
														 	     costs   = fst_of_six g;
														 	     vertices = [a | (a,b) <- d, a /= source]
														 	 in [getPathInfo costs parents source a | a <- vertices])

getPathInfo :: [(Integer, Double)] -> [(Integer, Integer)] -> Integer -> Integer -> String
getPathInfo d p source destination = "path from source to " ++ show destination ++ " goes through these vertices: " ++ 
								show path_vertices ++ " and costs " ++ show (getVertexDistance d destination)
	where path_vertices = frth_of_four $ getPathFromSource (p, source, destination, [])
 

getPathFromSource :: ([(Integer, Integer)], Integer, Integer, [Integer]) -> ([(Integer, Integer)], Integer, Integer, [Integer])
getPathFromSource g = (if (snd_of_four g) == (thrd_of_four g) 
					   then g
					   else let new_v = getParentbyVertexNumber (first_of_four g) (thrd_of_four g)
							in (let new_path = (thrd_of_four g) : (frth_of_four g)
								in (getPathFromSource ((first_of_four g), (snd_of_four g), new_v, new_path))))


getParentbyVertexNumber :: [(Integer, Integer)] -> Integer -> Integer
getParentbyVertexNumber parents v = snd  $ parents !! v'
	where v' = fromIntegral (pred v) :: Int

--calculation of nencessary data for getting the shortest paths (distances and parents)
getCalculatedParentsCosts :: ([(Integer, Double)], [(String, Double)], [(Integer, [Integer])], [(Integer, Bool)], [(Integer, Integer)], Integer) -> ([(Integer, Double)],[(String, Double)],[(Integer, [Integer])], [(Integer, Bool)], [(Integer, Integer)], Integer)
getCalculatedParentsCosts g = (if sxth_of_six g == 0 
							   then g 
							   else (getCalculatedParentsCosts $ getUpdatedAll 
							   (fst_of_six g) (snd_of_six g) (thr_of_six g) (frth_of_six g) (ffth_of_six g) (sxth_of_six g)))

getUpdatedAll :: [(Integer, Double)] -> [(String, Double)] -> [(Integer, [Integer])] -> [(Integer, Bool)] -> [(Integer, Integer)] -> Integer -> ([(Integer, Double)],[(String, Double)],[(Integer, [Integer])],[(Integer, Bool)],[(Integer, Integer)],Integer)
getUpdatedAll d w out_vs v p v_next = let new_visited = getUpdatedVisited v v_next; 
										  new_dp = getUpdatedDistancesParents d w out_vs p v_next
										  in (let new_distances = fst new_dp; new_parents = snd new_dp 
											  in (let new_v_next = getNextVertex new_visited new_distances out_vs
												  in (new_distances, w, out_vs, new_visited, new_parents, new_v_next)))
												  
getUpdatedVisited :: [(Integer, Bool)] -> Integer -> [(Integer, Bool)]
getUpdatedVisited v v_next = [(a,new_val) | (a,b) <- v, let new_val = (if a == v_next then True else b)]
												  
--getting updated distances and parents after vertices relaxation												  
getUpdatedDistancesParents :: [(Integer, Double)] -> [(String, Double)] -> [(Integer, [Integer])] -> [(Integer, Integer)] -> Integer -> ([(Integer, Double)],[(Integer, Integer)])
getUpdatedDistancesParents d w out_vs p v_next = (let new_d = getNextDistances d w v_next out_vs 
												  in (let changed = getVerticesWithChangedDistance new_d d; 
												          new_p = getUpdatedParents p changed v_next 
												      in (new_d, new_p)))

getNextDistances :: [(Integer, Double)] -> [(String, Double)] -> Integer -> [(Integer, [Integer])] -> [(Integer, Double)]
getNextDistances d w v_next out_vs = [(a, d_next) | (a,b) <- d, let d_next = (if a `elem` next_out 
																			  then  getNextDistance d w v_next a 
																			  else b)]
	where next_out = getOutcomingVertices out_vs v_next		
	
getVerticesWithChangedDistance :: [(Integer, Double)] -> [(Integer, Double)] -> [Integer]
getVerticesWithChangedDistance xs ys = [a | (a,b) <- xs, isChangedDistance a b ys]

isChangedDistance :: Integer -> Double -> [(Integer, Double)] -> Bool
isChangedDistance c d xs = length [a | (a,b) <- xs, a == c, b /= d] > 0

--calculation of the next vertex belonging to the shortest path between source and every other vertex
getNextVertex :: [(Integer, Bool)] -> [(Integer, Double)] -> [(Integer, [Integer])] -> Integer
getNextVertex v d out_vs = let not_visited = [a | (a,b) <- v, b == False];
                               having_outcoming_vertices = [a | (a,b) <- d, length (getOutcomingVertices out_vs a) > 0]
							   in (let candidates = intersect not_visited having_outcoming_vertices 
								   in (let next_vertices = getClosest d candidates
								   in (if length next_vertices > 0 then head next_vertices else 0)))
								   
getOutcomingVertices :: [(Integer, [Integer])] -> Integer => [Integer]
getOutcomingVertices xs v = head [b | (a,b) <- xs, a == v]

getClosest :: [(Integer, Double)] -> [Integer] -> [Integer]
getClosest [] [] = []
getClosest distances candidates = let filteredDistances = [(a,b) | (a,b) <- distances, a `elem` candidates]
								  in (let minDistance = minimum $ map snd filteredDistances
								  in  [a | (a,b) <- filteredDistances, b == minDistance])

--extracting data from the non-trivial data structures that I've invented for this problem
-- e.g. visited = [(1,False),(2,False),(3,False),(4,False),(5,False)] - tells us whether the vertex was
-- already considered to belong to the shortest route or not. e.g., here we can see that none of the vertices
-- hasn't been considered yet
-- outcoming_vertices = [(1,[2,3,4,5]),(2,[]),(3,[5]),(4,[2,3]),(5,[1,3,4])] tell us what outcoming vertices
-- has each vertex in the graph, e.g. vertex 2 doesn't have any outcoming_vertices, vertex 1 has 4 outcoming
--vertices.
-- weights = [("3-5",10), ("5-1",10), ("5-3",10), ("1-5",10), ("1-4",50), ("1-3",30), ("1-2",10), ("5-4",30),
-- ("4-2",40), ("4-3",20)] this structure consists of tuples ("a-b",c) where "c" is the price/cost of the 
--part of the path that we follow when we go from vertex a to vertex b
getEdgeWeight :: [(String, Double)] -> Integer -> Integer -> Double
getEdgeWeight w v1 v2 = head [b | (a,b) <- w, a == show v1 ++ "-" ++ show v2]

getVertexDistance :: [(Integer, Double)] -> Integer -> Double
getVertexDistance d v = head [b | (a,b) <- d, a == v]

getNextDistance :: [(Integer, Double)] -> [(String, Double)] -> Integer -> Integer -> Double
getNextDistance d w v_next out_v = (let edgeCost = getEdgeWeight w v_next out_v; 
									prev_d = getVertexDistance d out_v; next_d = getVertexDistance d v_next 
									in minimum [prev_d, next_d + edgeCost])
									
getUpdatedParents :: [(Integer, Integer)] -> [Integer] -> Integer -> [(Integer, Integer)]
getUpdatedParents p_old changed v_next = [(a, new_val) | (a,b) <- p_old, let new_val = (if a `elem` changed 
																						then v_next 
																						else b)]

--extracting elements from tuples
fst_of_six :: (a, b, c, d, e, f) -> a  
fst_of_six (x, _, _, _, _, _) = x  

snd_of_six :: (a, b, c, d, e, f) -> b  
snd_of_six (_, x, _, _, _, _) = x 

thr_of_six :: (a, b, c, d, e, f) -> c  
thr_of_six (_, _, x, _, _, _) = x  

frth_of_six :: (a, b, c, d, e, f) -> d  
frth_of_six (_, _, _, x, _, _) = x 

ffth_of_six :: (a, b, c, d, e, f) -> e  
ffth_of_six (_, _, _, _, x, _) = x  

sxth_of_six :: (a, b, c, d, e, f) -> f  
sxth_of_six (_, _, _, _, _, x) = x

first_of_four :: (a, b, c, d) -> a
first_of_four (y, _, _, _) = y

snd_of_four :: (a, b, c, d) -> b
snd_of_four (_, y, _, _) = y

thrd_of_four :: (a, b, c, d) -> c
thrd_of_four (_, _, y, _) = y

frth_of_four :: (a, b, c, d) -> d
frth_of_four (_, _, _, y) = y