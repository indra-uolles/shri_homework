'''
Created on 23.09.2013

@author: natalia
'''

class PathCalculator(object):
    '''
    classdocs
    '''

    def __init__(self, adjacency_matrix, source):
        '''
        Constructor
        '''
        #check the adjacency matrix
        if len(adjacency_matrix) != len(adjacency_matrix[0]):
            raise Exception("adjacency matrix should be square")
        columns_number = len(adjacency_matrix[0])       
        for i in range(len(adjacency_matrix)):
            matrix_row = adjacency_matrix[i]
            if len(matrix_row) != columns_number:
                raise Exception("adjacency matrix rows should be of the same size")
            for j in range(len(matrix_row)):
                try:
                    val = int(adjacency_matrix[i][j])
                except ValueError:
                    raise Exception("this is not a digit: ", matrix_row[j])
            
            
        #put the data into the comfy data structures
        n, m = len(adjacency_matrix), len(adjacency_matrix[0])
        distances, visited, outcoming_vertices, weights, parents = {}, {}, {}, {}, {}
        
        for i in range(n):
            distances[i+1] = float("inf")
        distances[source] = 0
        
        for i in range(n):
            visited[i+1] = False
            parents[i+1] = source

        for i in range(n):
            outcoming_vertices[i+1] = []
            for j in range(m):
                if adjacency_matrix[i][j] != 0:
                    outcoming_vertices[i+1].append(j+1)
                    weights[str(i+1) + '_' + str(j+1)] = adjacency_matrix[i][j]
                    
        self.distances          = distances
        self.visited            = visited
        self.outcoming_vertices = outcoming_vertices
        self.weights            = weights
        self.parents            = parents
        
        #calculate the shortest routes between source and every other vertex
        v_next = self.get_next_vertex(distances,visited)
        while v_next != -1:
            if v_next in outcoming_vertices:
                visited[v_next] = True
            else:
                while not v_next in outcoming_vertices and v_next != -1:
                    self.visited[v_next] = True
                    v_next = self.get_next_vertex(distances,visited)
            if v_next != -1:
                self.distances, self.parents = self.relax_vertices(outcoming_vertices, weights, distances, v_next, parents)
                v_next = self.get_next_vertex(distances,visited)

    def get_next_vertex(self, distances, visited):
        next_vertex= -1
        not_visited = {k:v for k, v in visited.iteritems() if v != True}
        if (len(not_visited) > 0):
            min_distance = min([v for k, v in distances.iteritems() if k in not_visited])
            next_vertices = [k for k, v in distances.iteritems() if k in not_visited and v == min_distance]
            if (len(next_vertices) > 0):
                next_vertex = next_vertices[0]
        return next_vertex

    def relax_vertices(self, outcoming_vertices, weights, distances, v_next, parents):
        for outcoming_vertex in outcoming_vertices[v_next]:
            if str(v_next) + '_' + str(outcoming_vertex) in weights:
                edge_cost = weights[str(v_next) + '_' + str(outcoming_vertex)]
                prev_distance = distances[outcoming_vertex]
                distances[outcoming_vertex] = min(distances[outcoming_vertex], distances[v_next] + edge_cost)
                if distances[outcoming_vertex] < prev_distance:
                    parents[outcoming_vertex] = v_next
        return distances,parents
    
    def get_shortest_path(self, source, destination):
        result = []
        current_vertex = destination
        while current_vertex != source:
            result.append(self.parents[current_vertex])
            current_vertex = self.parents[current_vertex]
        return "the shortest path from " + str(source) + " to " + str(destination) + " goes through these vertices: "  + ', '.join(map(str,result)) + "; path cost is " + str(self.distances[destination])