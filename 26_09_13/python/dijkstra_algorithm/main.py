'''
Created on 22.09.2013

@author: natalia
http://habrahabr.ru/post/111361/
http://e-maxx.ru/algo/dijkstra
'''
from Dijkstra import PathCalculator

arr = []
arr.append([0,10,30,50,10])
arr.append([0,0,0,0,0])
arr.append([0,0,0,0,10])
arr.append([0,40,20,0,0])
arr.append([10,0,10,30,0])
pcalc = PathCalculator(arr, 1)
print pcalc.get_shortest_path(1, 2)
print pcalc.get_shortest_path(1, 3)
print pcalc.get_shortest_path(1, 4)
print pcalc.get_shortest_path(1, 5)