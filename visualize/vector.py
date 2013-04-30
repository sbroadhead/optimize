#
# vector.py
# 
# 2-d vectors and vector operations
#

import math

class Vector(object):
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __add__(self, other):
        return Vector(self.x+other[0], self.y+other[1])

    def __sub__(self, other):
        return Vector(self.x-other[0], self.y-other[1])

    def __mul__(self, k):
        return Vector(self.x*k, self.y*k)

    def __div__(self, k):
        return Vector(self.x/k, self.y/k)

    def __rmul__(self, k):
        return Vector(self.x*k, self.y*k)

    def __repr__(self):
        return 'Vector(%f, %f)' % (self.x, self.y)

    def __getitem__(self, i):
        return (self.x, self.y)[i]

    def __iter__(self):
        for i in (self.x, self.y):
            yield i
        raise StopIteration

def dot(v1, v2):
    return v1.x*v2.x + v1.y*v2.y

def squared_norm(v):
    return dot(v, v)

def norm(v):
    return math.sqrt(squared_norm(v))

def normalized(v):
    if norm(v) == 0:
        return v
    return v/norm(v)
