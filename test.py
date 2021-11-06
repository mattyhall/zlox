import time

def fib(n):
    if n < 2: return n
    return fib(n-1) + fib(n-2)

start = time.time()
fib(35)
print(time.time() - start)