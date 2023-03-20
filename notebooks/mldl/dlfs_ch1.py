import numpy as np


a = np.array([1, 2, 3])
b = np.array([4, 5, 6])

print(a + b)
print(a - b)
print(a * b)
print(a / b)

a = np.array([[1, 2], [3, 4]])

print(a.sum(axis=0))
print(a.sum(axis=1))
