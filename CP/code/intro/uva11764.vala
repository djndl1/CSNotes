using GLib;

int main() {
	int cases = 0;
	stdin.scanf("%d\n", &cases);

	for (int i = 1; i <= cases; i++) {
		int wall_cnt = 0;
		stdin.scanf("%d\n", &wall_cnt);
		var wall_line = stdin.read_line();
		if (wall_cnt <= 1) {
			stdout.printf(@"Case $i: 0 0\n");
			continue;
		}
		string[] wall_heights = wall_line.split(" ");

		int prior = int.parse(wall_heights[0]);

		int high_cnt = 0;
		int low_cnt = 0;
		foreach (unowned string wall in wall_heights[1:wall_heights.length]) {
			int post = int.parse(wall);
			if (post > prior) {
				high_cnt++;
			} else if (post < prior) {
				low_cnt++;
			}

			prior = post;
		}
		stdout.printf(@"Case $i: $high_cnt $low_cnt\n");

	}
	return 0;
}
