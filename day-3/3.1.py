import sys
x = sys.stdin.read().split("\n")[:-1] # Final line is empty
print('\n'.join([''.join(map(lambda s: s[i], x)) for i in range(12)]))
