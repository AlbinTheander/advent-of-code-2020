package days

import common.Day

class Day3 : Day() {

    override fun solve() {
        val slope = Slope(getFileData("day03.txt"))
        val result1 = part1(slope)
        val result2 = part2(slope);
        println("\n===== Day 3  =====")
        println("The first tobbogan hits $result1 trees")
        println("The second tobbogans hits $result2 trees")
    }

    private fun part1(slope: Slope): Long {
        return countTrees(slope, 3, 1)
    }

    private fun part2(slope: Slope): Long {
        val directions = arrayOf(1 to 1, 3 to 1, 5 to 1, 7 to 1, 1 to 2)

        val trees = directions.map { (dx, dy) -> countTrees(slope, dx, dy) }

        return trees.reduce {a, b -> a * b}
    }

    private fun countTrees(slope: Slope, dx: Int, dy: Int): Long {
        var x = 0
        var y = 0
        var trees: Long = 0
        while (y < slope.length) {
            if (slope.isTree(x, y)) trees++
            x += dx
            y += dy
        }
        return trees
    }
}

class Slope(val map: List<String>) {
    val length = map.size

    fun isTree(x: Int, y: Int): Boolean {
        val line = map.get(y)
        return line[x % line.length] == '#';
    }
}