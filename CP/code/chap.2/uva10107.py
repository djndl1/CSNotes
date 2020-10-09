import sys
import statistics

def read_input(fin):
    line = fin.readline().strip()
    if line:
        return int(line)

if __name__ == '__main__':
    cin = sys.stdin
    cout = sys.stdout

    nums = []
    while True:
        X = read_input(cin)
        if not X:
            break
        nums.append(X)
        cout.write(str(int(statistics.median(nums))) + "\n" )
