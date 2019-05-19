
def isPal(n):
    s = str(n)
    c = len(s) // 2
    return s[:c] == s[-c:][::-1]

def problem04():
    pals = [v1*v2 for v1 in range(101,999) for v2 in range(v1,999) if isPal(v1*v2)]
    return max(pals)

print(problem04())
