package common;

import java.io.File

abstract class Day() {

    fun getFileData(filename: String): List<String> {
        return File("../data/$filename").readLines()
    }

    abstract fun solve(): Unit
}