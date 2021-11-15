package days

import common.Day

class Day9: Day() {
    override fun solve() {
        val nums = getFileData("day09.txt").map { it.toLong() }
        val firstInvalid = part1(nums)
        val weakness = part2(nums, firstInvalid)
        println("\n===== Day 9  =====")
        println("The first invalid number is $firstInvalid")
        println("The weakness is $weakness")
    }

    private fun part1(nums: List<Long>): Long {
        var current = nums.subList(0, 25).toMutableList()
        for (n in nums.subList(25, nums.size)) {
            if (!hasSum(current, n)) return n
            current.removeAt(0)
            current.add(n)
        }

        return -1
    }

    private fun part2(nums: List<Long>, target: Long): Long {
        var sum:Long = 0
        var low = 0
        var high = 0
        while(high < nums.size) {
            while (sum < target) { sum += nums[high++]; }
            if (sum == target) {
                val range = nums.subList(low, high)
                return range.minOrNull()!! + range.maxOrNull()!!
            }
            while (sum > target && low < high-2) { sum -= nums[low++] }
        }
        return -1
    }

    private fun hasSum(ns: List<Long>, n: Long): Boolean {
        for (i in 0..ns.size-2)
            for (j in i+1..ns.size-1)
                if (ns[i] + ns[j] == n) return true
        return false
    }
}