#!/usr/bin/env python

import numpy as np
import sys
import queue


#Globals
airlist = []
start = (0,0)
home = (0,0)
virus = (0,0)
Q = queue.Queue(maxsize= sys.maxsize)
neighbors = [(1,0), (0,-1), (0,1), (-1,0)]

def split(line, linec):
    global start,home,virus
    L = []
    charC = 0
    for char in line:
        if char == 'X':
            L.append((-15))
        elif char == 'A':
            L.append(sys.maxsize - 1)
            airlist.append((linec, charC))
        elif char =='W':
            L.append(0)
            virus = (linec, charC)
        elif char == '\n':
            continue
        else:
            if char == 'T':
                home = (linec, charC)
            elif char == 'S':
                start = (linec, charC)
            L.append(sys.maxsize)
        charC +=1 
    return L                     

def read():  # https://stackoverflow.com/questions/51161670/reading-matrix-from-a-text-file-and-storing-it-in-an-array
    l = []
    linec = 0
    with open(sys.argv[1], 'r') as f:
        for line in f:
            l.append(list(split(line, linec)))
            linec = linec + 1
    matrix = np.asarray(l)
    bfs(matrix)



#https://en.wikipedia.org/wiki/Flood_fill
def bfs(matrix):
    add_to_q(virus, 0, matrix)
    while Q.queue:
        (root, t) = Q.get()
        i, j = root[0], root[1]
        if i < 0 or i >= matrix.shape[0] or j <0 or j>=matrix.shape[1]:
            continue

        if matrix[i][j] == -15:
            continue

        if matrix[i][j] == sys.maxsize-1: #aerodromio
            matrix[i][j] = t+2 
            add_to_q((i,j), matrix[i][j], matrix)
            for airport in airlist:
                    if matrix[airport[0]][airport[1]] > t + 7:  
                        Q.put(((airport[0],airport[1]),t + 5))
        
        elif matrix[i][j] > t + 2:
            matrix[i][j]= t + 2   
            add_to_q((i,j), t+2, matrix)
    N, path = astar(matrix)
    if path == False or N ==0 :
        print("IMPOSSIBLE")
    else:
        print(N, path, sep='\n')          

    
def add_to_q(root, t, matrix):
    for (i,j) in neighbors:
        neighbor = (root[0] + i, root[1] + j)
        if 0<=neighbor[0] < matrix.shape[0] and 0 <= neighbor[1] < matrix.shape[1]:
            if matrix[neighbor[0]][neighbor[1]] != -15 and matrix[neighbor[0]][neighbor[1]] > t + 2:
                Q.put((neighbor,t))
    return


#https://www.analytics-link.com/post/2018/09/14/applying-the-a-path-finding-algorithm-in-python-part-1-2d-square-grid          
def astar(matrix):  #gscore = t, fscore = gscore
#telika egine aplo bfs ki oxi a*
    close_set = set() #keep visited nodes
    came_from = {} #keep parents of nodes
    Q.put((0, start ))
    close_set.add(start)
    while Q.queue:
        (t,current) = Q.get()
        
        while ((current[0] <0 or current[1] < 0 or current[0] >= matrix.shape[0] or current[1] >=matrix.shape[1])
         or (matrix[current[0]][current[1]] == -15)):
            (t,current) = Q.get()

        if current == home:
            answer = []
            while current in came_from:

                prev = current
                current = came_from[current]

                if current == (prev[0] + 1, prev[1]):
                    answer.append('U')
                elif current == (prev[0] - 1, prev[1]):
                    answer.append('D')
                elif current == (prev[0], prev[1] +1):
                    answer.append('L')    
                elif current == (prev[0], prev[1]-1):
                    answer.append('R')

            answer.reverse()
            return (len(answer), ("".join(answer)))


        for (i,j) in neighbors:
            neighbor = (current[0] + i , current[1] + j)
            if 0 <= neighbor[0] < matrix.shape[0] and 0<= neighbor[1] < matrix.shape[1]:
                if matrix[neighbor[0]][neighbor[1]] > t + 1 and neighbor not in close_set:
                    came_from[neighbor] = current
                    close_set.add(neighbor)
                    Q.put((t+1, neighbor))
    
    return (0, False)
       

read() 