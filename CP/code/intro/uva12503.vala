using GLib;

enum Robot_Move {
	Left,
	Right
}

int main() {
	int T = 0;
	stdin.scanf("%d\n", &T);

	while ((T--) > 0) {
		int n = 0;
		stdin.scanf("%d\n", &n);

		Robot_Move[] moves = new Robot_Move[n];
		for (int i = 0; i < n; i++) {
			string[] s = stdin.read_line().split(" ");

			if (s[0][0] == 'L') {
				moves[i] = Robot_Move.Left;
			} else if (s[0][0] == 'R')
				moves[i] = Robot_Move.Right;
			else {
				int idx = int.parse(s[2]);
				moves[i] = moves[idx - 1];
			}
		}

		int coord = 0;
		foreach (var mv in moves) {
			coord += (mv == Robot_Move.Left ? -1 : 1);
		}
		stdout.printf(@"$coord\n");
	}

	return 0;
}
