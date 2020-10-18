import java.util.*;

fun main() {
    val n: Int = readLine()?.trim()?.toInt() ?: 0;

    var incrementOfYear = TreeMap<Int, Int>();
    for (p in 1..n) {
        var line = readLine()?.trim()?.split(" ");
        var by = line?.get(0)?.toInt() ?: 0;
        var dy = line?.get(1)?.toInt() ?: 0;

        incrementOfYear[by] = incrementOfYear.getOrDefault(by, 0) + 1;
        incrementOfYear[dy] = incrementOfYear.getOrDefault(dy, 0) - 1;
    }
    var runningSum: Int = 0;
    var maxYear: Int = 0;
    var maxPopulation: Int = 0;
    for ((curYear, curPopulation) in incrementOfYear) {
        runningSum += curPopulation
                if (runningSum > maxPopulation) {
                    maxPopulation = runningSum;
                    maxYear = curYear;
                }
    }
    println("${maxYear} ${maxPopulation}");
}
