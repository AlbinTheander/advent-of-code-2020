package days

import java.io.File

class Day1 {

    fun solve() {
        val data = getData()
        val result1 = part1(data)
        val result2 = part2(data)
        println("===== Day 1  =====")
        println("The double product is $result1")
        println("The triple product is $result2")
    }

    private fun getData() = File("../data/day01.txt").readLines().map { it.toInt() }

    private fun part1(data: List<Int>): Int {
        for (i in data.indices)
            for (j in data.indices)
                if (i != j && data[i] + data[j] == 2020) return data[i] * data[j];

        return -1;
    }

    private fun part2(data: List<Int>): Int {
        val last = data.size - 1
        for (i in 0..last-2)
            for(j in i+1..last-1)
                for(k in j+1..last)
                    if (data[i] + data[j] + data[k] == 2020)
                        return data[i] * data[j] * data[k]

        return -1;
    }

}