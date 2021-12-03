D = {
    "f": 0,
    "u": 0,
    "d": 0,
    "a": 0
}

A = D.copy()

while True:
    try:
        a, b = input().split()
    except EOFError:
        break
    D[a[0]] += int(b)
    A[a[0]] += int(b)
    if A['f']:
        A['f'] = 0
        D['a'] += int(b) * (A['d'] - A['u'])
print(D['a']*D['f'])
