#!/usr/bin/env python3

import sys, io, time

def main():
    case_cnt = int(sys.stdin.readline())
    for i in range(1, case_cnt+1):
        wall_cnt = int(sys.stdin.readline())

        if wall_cnt <= 1:
            sys.stdout.write("Case {}: 0 0\n".format(i))
            sys.stdin.readline()
            continue

        walls = sys.stdin.readline().split(' ')
        prior = int(walls[0])

        high_cnt = 0
        low_cnt = 0
        for post in [int(w) for w in walls[1:]]:
            if post < prior:
                low_cnt += 1
            elif post > prior:
                high_cnt += 1

            prior = post

        sys.stdout.write("Case {}: {} {}\n".format(i, high_cnt, low_cnt))

if __name__ == '__main__':
    main()
