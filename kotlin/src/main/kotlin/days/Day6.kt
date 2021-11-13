package days

import common.Day

class Day6: Day() {
    override fun solve() {
        val groups = getData()
        val anyAnswered = part1(groups)
        val allAnswered = part2(groups)
        println("\n===== Day 6  =====")
        println("The sum of questions anyone answered yes to is $anyAnswered")
        println("The sum of questions everyone answered yes to is $allAnswered")

    }

    fun part1(groups: List<Group>): Int {
        return groups
            .map { it.anyAnsweredCount }
            .sum()
    }

    fun part2(groups: List<Group>): Int {
        return groups
            .map { it.allAnsweredCount }
            .sum()
    }

    private fun getData(): List<Group> {
        val lines = getFileData("day06.txt")
        val groups = mutableListOf<Group>()
        var group: Group? = null
        lines.forEach {
            if (it.isEmpty()) {
                group?.also { groups.add(it) }
                group = null
            } else {
                val g = group ?: Group()
                g.addAnswer(it)
                group = g
            }
        }
        group?.also { groups.add(it) }
        return groups
    }
}

class Group() {
    val answers = mutableListOf<String>()

    fun addAnswer(answer: String) = answers.add(answer)

    val anyAnsweredCount: Int
        get() {
            return answers
                .map { it.toSet() }
                .reduce { s1, s2 -> s1.union(s2) }
                .size
        }
    val allAnsweredCount: Int
        get() {
            return answers
                .map { it.toSet() }
                .reduce { s1, s2 -> s1.intersect(s2) }
                .size
        }
}