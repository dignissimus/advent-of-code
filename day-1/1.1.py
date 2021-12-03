p = -100
inc = 0
while True:
    try:
        a = int(input())
    except ValueError:
        break
    if p < 0:
        p = a
    if a > p:
        inc += 1
    p = a

print(inc)
