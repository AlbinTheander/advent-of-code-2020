package days

import common.Day

class Day5 : Day() {
    override fun solve() {
        val seats = getFileData("day05.txt")
        val seatNrs = seats.map(this::seatToInt)
        val highestSeat = part1(seatNrs)
        val mySeat = part2(seatNrs)

        println("\n===== Day 5  =====")
        println("The highest seat nr is $highestSeat")
        println("My seat is $mySeat")
    }

    private fun seatToInt(seat: String): Int {
        val bin = seat
            .replace(Regex("[BR]"), "1")
            .replace(Regex("[FL]"), "0")

        return bin.toInt(2)
    }
    private fun part1(seats: List<Int>): Int {
        return seats.maxOrNull() ?: -1
    }

    private fun part2(seats: List<Int>): Int {
        val sorted = seats.sorted()
        val myIndex = (1..sorted.size).find { sorted[it] == sorted[it-1]+2 }

        return sorted[myIndex!!] - 1
    }
}