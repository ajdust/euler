
def fib(n: int, k: int):
    previous = 0
    current = 1
    for i in range(0, n - 1):
        temp = current
        current += previous * k
        previous = temp

    return current
