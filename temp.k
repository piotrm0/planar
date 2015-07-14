# Define ':' for sequencing:

def binary : 1 (x y) y;

def fib(x)
    if (x < 3) then
       1
    else
       fib(x-1) + fib(x-2);