package com.github.natanbc.aoc

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day6 {
    def main(args: Array[String]): Unit = {
        val string = """4	10	4	1	8	4	9	14	5	1	14	15	0	15	3	5"""

        val v = Seq(string.split("\\s").map(_.toInt).toSeq).map(new ArrayBuffer()++_).map(seq=>{
            val set = new mutable.HashSet[Seq[Int]]()
            var count = 0
            while(!set.contains(seq)) {
                var max = seq.max
                val idx = seq.indexOf(max)
                set += seq.clone()
                seq(idx) = 0
                var i = (idx + 1) % seq.size
                while(max > 0) {
                    seq(i) += 1
                    i = (i + 1) % seq.size
                    max -= 1
                }
                count += 1
            }
            count
        }).head

        println(v)

        val v2 = Seq(string.split("\\s").map(_.toInt).toSeq).map(new ArrayBuffer()++_).map(seq=>{
            val set = new mutable.HashMap[Seq[Int], Int]()
            var count = 0
            while(!set.contains(seq)) {
                var max = seq.max
                val idx = seq.indexOf(max)
                set += seq.clone()->count
                seq(idx) = 0
                var i = (idx + 1) % seq.size
                while(max > 0) {
                    seq(i) += 1
                    i = (i + 1) % seq.size
                    max -= 1
                }
                count += 1
            }
            count - set(seq)
        }).head

        println(v2)
    }
}
