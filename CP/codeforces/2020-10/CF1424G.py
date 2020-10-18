import sys
import collections

if __name__ == '__main__':
    cin = sys.stdin
    cout = sys.stdout

    n = int(cin.readline().strip())
    increments = collections.OrderedDict()
    for p in range(0, n):
        line = cin.readline().strip().split(' ')
        by = int(line[0])
        dy = int(line[1])

        increments[by] = increments.get(by, 0) + 1
        increments[dy] = increments.get(dy, 0) - 1

    increments = {k: v for k, v in sorted(increments.items(), key=lambda pair: pair[0])}
    population_point = collections.OrderedDict()

    max_year = 0
    max_population = 0;
    running_sum = 0;
    for cur_year, cur_inc in increments.items():
        running_sum += cur_inc
        if running_sum > max_population:
            max_year = cur_year
            max_population = running_sum

    result = f"{max_year} {max_population}\n"
    cout.write(result)
