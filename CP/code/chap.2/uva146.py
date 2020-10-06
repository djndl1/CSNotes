import itertools
import sys


def read_code(fin):
    temp = fin.readline().strip()
    while not temp:
        temp = fin.readline().strip()

    return temp

class lexicographic_order_permutation:
    def __init__(self, iterable):
        self.iter_len = len(iterable)
        self.cur_permute = list(iterable)

    def __iter__(self):
        return self

    def __next__(self):
        k = self.iter_len - 2
        while k >= 0:
            if self.cur_permute[k] < self.cur_permute[k+1]:
                break;
            k -= 1
        if k < 0:
            raise StopIteration()

        l = self.iter_len - 1
        while l > k:
            if self.cur_permute[l] > self.cur_permute[k]:
                break
            l -= 1

        self.cur_permute[k], self.cur_permute[l] = self.cur_permute[l], self.cur_permute[k]
        self.cur_permute[k+1:] = list(reversed(self.cur_permute[k+1:]))

        return tuple(self.cur_permute)


if __name__ == '__main__':
    fin = sys.stdin
    fout = sys.stdout

    while True:
        code = read_code(fin)
        if code == "#":
            break

        try:
            it = lexicographic_order_permutation(code)
            successor = next(it)
            fout.write("".join(successor) + "\n")
        except StopIteration as e:
            fout.write("No Successor\n")
