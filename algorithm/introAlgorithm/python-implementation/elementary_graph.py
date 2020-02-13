#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import enum

class GraphVertexColor(enum.Enum):
    WHITE = 1
    GRAY = 2
    BLACK = 3

class GraphVertex:
    def __init__(self, value):
        self.value = value
        self.num = 0

        self.color = GraphVertexColor.WHITE
        self.dist = 0
        self.pre = None

    def __str__(self):
        return 'GraphVertex num ' + str(self.num) + \
            "," + "value: " + str(self.value)

    def __repr__(self):
        return self.__str__()

class Graph:
    def __init__(self, vertices=None, edges=None):
        if not (vertices and edges):
            self.adj_lists = dict()
            self.size = 0
            self.vertices = []
        else:
            self.adj_lists = edges
            self.size = len(vertices)
            self.vertices = vertices

    def insert(self, vertex, adjacencies):
        vertex.num = self.size
        self.size += 1
        self.vertices.append(vertex)
        self.adj_lists[vertex.num] = adjacencies

    def bfs(self):
        from queue import SimpleQueue
        for v in self.vertices:
            v.color = GraphVertexColor.WHITE
            v.dist = -1
            v.pre = None
        s = self.vertices[0]
        s.dist = 0
        s.pre = None
        Q = SimpleQueue()
        Q.put(s)
        while not Q.empty():
            u = Q.get()
            for v in self.adj_lists[u.num]:
                if v.color == GraphVertexColor.WHITE:
                    v.color = GraphVertexColor.GRAY
                    v.dist = u.dist + 1
                    v.pre = u
                    Q.put(v)
                    print(v)
            u.color = GraphVertexColor.BLACK


g = Graph()
r = GraphVertex(1)
s = GraphVertex(2)
t = GraphVertex(3)
u = GraphVertex(4)
v = GraphVertex(5)
w = GraphVertex(6)
x = GraphVertex(7)
y = GraphVertex(8)
radj = [s, v]
sadj = [r, w]
tadj = [w, x, u]
uadj = [t, x, y]
vadj = [r]
wadj = [s, t, x]
xadj = [w, t, u, y]
yadj = [x, u]

vertices = [r, s, t, u, v, w, x, y]
lists = [radj, sadj, tadj, uadj, vadj, wadj, xadj, yadj]

for v, adj in zip(vertices, lists):
    g.insert(v, adj)

