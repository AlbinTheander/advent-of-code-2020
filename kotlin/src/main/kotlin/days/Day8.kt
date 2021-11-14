package days

import common.Day

class Day8: Day() {
    override fun solve() {
        val program = getProgram()
        val acc1 = part1(program)
        val acc2 = part2(program)
        println("\n===== Day 6  =====")
        println("The accumulator is $acc1 when the program stops")
        println("The accumulator is $acc2 when the program stops correctly")
    }

    fun part1(program: List<Instruction>): Int {
        val cpu = CPU(program)
        cpu.run()
        return cpu.acc
    }

    fun part2(original: List<Instruction>): Int {
        var program = original.toMutableList()
        for(line in original.indices) {
            when (original[line].op) {
                "jmp" -> program[line] = Instruction("nop", original[line].arg)
                "nop" -> program[line] = Instruction("jmp", original[line].arg)
            }
            val cpu = CPU(program)
            cpu.run()
            if (!cpu.crashed) return cpu.acc
            program[line] = original[line]
        }
        return -1
    }

    private fun getProgram(): List<Instruction> {
        return getFileData("day08.txt")
            .map {
                val parts = it.split(" ")
                Instruction(parts[0], parts[1].toInt())
            }
    }
}

data class Instruction(val op: String, val arg: Int)

class CPU(val program: List<Instruction>) {

    var crashed: Boolean = false

    var ip: Int = 0
    var acc: Int = 0
    private val usedLines: MutableSet<Int> = mutableSetOf()

    fun run() {
        usedLines.removeAll { true }
        while(ip >= 0 && ip < program.size && !crashed) {
            step()
            ip++
        }
    }

    fun step() {
        if (usedLines.contains(ip)) {
            crashed = true
            return
        }
        usedLines.add(ip)
        val instr = program[ip]
        when(instr.op) {
            "acc" -> acc += instr.arg
            "jmp" -> ip += instr.arg - 1
        }
    }
}