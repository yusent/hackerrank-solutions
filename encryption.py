# Challenge url: https://www.hackerrank.com/challenges/encryption/problem

import math

s = input().strip()
l = len(s)
r = math.floor(math.sqrt(l))
c = math.ceil(math.sqrt(l))

for i in range(c):
  j = i

  while j < l:
    print(s[j], end="")
    j += r + (0 if r == c else 1)

  print(" ", end="")
