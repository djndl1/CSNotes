#include <cstdio>

enum class Robot_Move {
    Left,
    Right,
};

int main()
{
    int T;
    scanf("%d\n", &T);
    while (T--) {
        int n;
        scanf("%d\n", &n);

        Robot_Move moves[100];
        for (int i = 0; i < n; i++) {
            char instruction[50];
            fgets(instruction, 50, stdin);

            if (instruction[0] == 'L') {
                moves[i] = Robot_Move::Left;
            }
            else if (instruction[0] == 'R') {
                moves[i] = Robot_Move::Right;
            } else {
                int cmd_idx;
                sscanf(instruction, "SAME AS %d", &cmd_idx);
                moves[i] = moves[cmd_idx - 1];
            }
        }

        int coord = 0;
        for (int i = 0; i < n; i++) {
            auto mov = moves[i];
            if (mov == Robot_Move::Left) {
                coord -= 1;
            } else {
                coord += 1;
            }
        }

        printf("%d\n", coord);
    }
}
