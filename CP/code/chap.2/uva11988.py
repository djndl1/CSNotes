import sys
from collections import deque


if __name__ == '__main__':
    file = sys.stdin
    while True:
        line = sys.stdin.readline()
        line = line.rstrip()
        if not line:
            break

        beiju = deque()
        temp = deque()

        appending = True
        for c in line:
            if c == "[" and  appending:
                appending = False
                continue
            elif c == "[" and not appending:
                appending = False
                beiju.extendleft(temp)
                temp.clear()
                continue
            elif c == "]" and not appending:
                appending = True
                beiju.extendleft(temp)
                temp.clear()
                continue
            elif c == "]" and appending:
                continue

            if appending:
                beiju.append(c)
            else:
                temp.appendleft(c)

        if not appending:
            beiju.extendleft(temp)

        s = "".join(beiju)
        print(s)

