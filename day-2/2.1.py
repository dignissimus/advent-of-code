D = {
    "f": 0,
    "u": 0,
    "d": 0,
}

while True:
    try:
        a, b = input().split()
    except EOFError:
        break
    D[a[0]] += int(b)

print((D['d']-D['u'])*D['f'])
