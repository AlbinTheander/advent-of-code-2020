package days

import common.Day
import java.io.File

class Day2 : Day() {

    override fun solve() {
        val data = getData()
        val result1 = data.count(this::valid1)
        val result2 = data.count(this::valid2)
        println("\n===== Day 2  =====")
        println("The old policy has $result1 valid passwords")
        println("The new policy has $result2 valid passwords")
    }

    private fun getData(): List<Entry> {
        val lines = getFileData("day02.txt")
            .map { it.split(Regex("-|:? ")) }
            .map { Entry(it[0].toInt(), it[1].toInt(), it[2][0], it[3])}

        return lines
}

    private fun valid1(entry: Entry): Boolean {
        val count = entry.password.count { it == entry.letter }
        return count >= entry.first && count <= entry.last
    }

    private fun valid2(entry: Entry): Boolean {
        return (entry.password[entry.first-1] == entry.letter) xor
                (entry.password[entry.last-1] == entry.letter)

    }
}

data class Entry(val first: Int, val last: Int, val letter: Char, val password: String)