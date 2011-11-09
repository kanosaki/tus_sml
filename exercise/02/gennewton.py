#!/usr/bin/env python3
#-*- coding: utf-8 -*-

def drange(begin, end, delta):
    cur = begin
    while cur <= end:
        yield cur
        cur += delta

f = open("data", 'w')

for v in drange(-3, 3, 0.01):
    value = "{:f}\n".format(v)
    f.write(value)

f.close()
