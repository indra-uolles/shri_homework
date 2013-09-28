import Data.List

--вычисление необходимых данных для расчета кратчайших маршрутов из вершины-источника в любую другую вершину----------
getCalculatedParentsCosts :: ([(Integer, Double)], [(String, Double)], [(Integer, [Integer])], [(Integer, Bool)], [(Integer, Integer)], Integer) -> ([(Integer, Double)],[(String, Double)],[(Integer, [Integer])], [(Integer, Bool)], [(Integer, Integer)], Integer)
getCalculatedParentsCosts g = (if sxth_of_six g == 0 then g else (getCalculatedParentsCosts $ getUpdatedAll (fst_of_six g) (snd_of_six g) (thr_of_six g) (frth_of_six g) (ffth_of_six g) (sxth_of_six g)))

getUpdatedAll :: [(Integer, Double)] -> [(String, Double)] -> [(Integer, [Integer])] -> [(Integer, Bool)] -> [(Integer, Integer)] -> Integer -> ([(Integer, Double)],[(String, Double)],[(Integer, [Integer])],[(Integer, Bool)],[(Integer, Integer)],Integer)
getUpdatedAll d w out_vs v p v_next = let new_visited = getUpdatedVisited v v_next; new_dp = getUpdatedDistancesParents d w out_vs p v_next
										  in (let new_distances = fst new_dp; new_parents = snd new_dp 
											  in (let new_v_next = getNextVertex new_visited new_distances out_vs
												  in (new_distances, w, out_vs, new_visited, new_parents, new_v_next)))
												  
getUpdatedVisited :: [(Integer, Bool)] -> Integer -> [(Integer, Bool)]
getUpdatedVisited v v_next = [(a,new_val) | (a,b) <- v, let new_val = (if a == v_next then True else b)]
												  
--получение массива обновленных расстояний и родителей (после релаксации вершин)-------------------------------------												  
getUpdatedDistancesParents :: [(Integer, Double)] -> [(String, Double)] -> [(Integer, [Integer])] -> [(Integer, Integer)] -> Integer -> ([(Integer, Double)],[(Integer, Integer)])
getUpdatedDistancesParents d w out_vs p v_next = (let new_d = getNextDistances d w v_next out_vs 
												  in (let changed = getVerticesWithChangedDistance new_d d; new_p = getUpdatedParents p changed v_next 
												      in (new_d, new_p)))

getNextDistances :: [(Integer, Double)] -> [(String, Double)] -> Integer -> [(Integer, [Integer])] -> [(Integer, Double)]
getNextDistances d w v_next out_vs = [(a, d_next) | (a,b) <- d, let d_next = (if a `elem` next_out then  getNextDistance d w v_next a else b)]
	where next_out = getOutcomingVertices out_vs v_next		
	
getVerticesWithChangedDistance :: [(Integer, Double)] -> [(Integer, Double)] -> [Integer]
getVerticesWithChangedDistance xs ys = [a | (a,b) <- xs, isChangedDistance a b ys]

isChangedDistance :: Integer -> Double -> [(Integer, Double)] -> Bool
isChangedDistance c d xs = length [a | (a,b) <- xs, a == c, b /= d] > 0

--вычисление новой промежуточной вершины кратчайшего маршрута---------------------------------------------------------
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

--функции для преобразования данных задачи и извлечения данных задачи из структур типа [(...,...)]-------------------------						   
getEdgeWeight :: [(String, Double)] -> Integer -> Integer -> Double
getEdgeWeight w v1 v2 = head [b | (a,b) <- w, a == show v1 ++ "-" ++ show v2]

getVertexDistance :: [(Integer, Double)] -> Integer -> Double
getVertexDistance d v = head [b | (a,b) <- d, a == v]

getNextDistance :: [(Integer, Double)] -> [(String, Double)] -> Integer -> Integer -> Double
getNextDistance d w v_next out_v = (let edgeCost = getEdgeWeight w v_next out_v; prev_d = getVertexDistance d out_v; next_d = getVertexDistance d v_next 
									in minimum [prev_d, next_d + edgeCost])
									
getUpdatedParents :: [(Integer, Integer)] -> [Integer] -> Integer -> [(Integer, Integer)]
getUpdatedParents p_old changed v_next = [(a, new_val) | (a,b) <- p_old, let new_val = (if a `elem` changed then v_next else b)]

-- вспомогательные функции для извлечения элементов по номеру из кортежей----------------------------------------------
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