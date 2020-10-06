import sys
import io

def get_problems_for_one(fin: io.TextIOWrapper):
    line = fin.readline().strip()
    while not line:
        line = fin.readline().strip()
    prob_list = line.split(' ')
    prob_list.pop(0)

    return set([int(c) for c in prob_list])

def get_unique_problems(bloated: set, *other_problem_sets: set):
    temp = set(bloated)
    for prblms in other_problem_sets:
        temp.difference_update(prblms)

    return list(temp)

if __name__ == '__main__':
    fin = sys.stdin

    T = int(fin.readline())

    for t in range(1, T+1):
        prima = get_problems_for_one(fin)
        secunda = get_problems_for_one(fin)
        tertia = get_problems_for_one(fin)

        first_unique = get_unique_problems(prima, secunda, tertia)
        second_unique = get_unique_problems(secunda, prima, tertia)
        third_unique = get_unique_problems(tertia, secunda, prima)

        uniques = (first_unique, second_unique, third_unique)
        greatest = max([len(s) for s in uniques])
        print("Case #{}:".format(t))
        for i in range(1, len(uniques)+1):
            if len(uniques[i-1]) == greatest:
                sys.stdout.write(str(i) + " " + str(greatest))
                cur_set = list(uniques[i-1])
                cur_set.sort()
                for p in cur_set:
                    sys.stdout.write(" " + str(p))
                sys.stdout.write("\n")
