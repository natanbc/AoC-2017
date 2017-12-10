package com.github.natanbc.aoc

object Day10 {
    def main(args: Array[String]): Unit = {
        val string = "165,1,255,31,87,52,24,113,0,91,148,254,158,2,73,153"

        val v = Seq(string.split(",").map(_.toInt)).map(array=>{
            val list = (0 to 255).toBuffer
            var pos = 0
            var skipSize = 0
            array.foreach(length=>{
                val v = list ++ list
                var i = pos
                v.slice(pos, pos + length).reverseIterator.foreach(element=>{
                    if(i >= list.size) i = 0
                    list(i) = element
                    i += 1
                })
                pos = (pos + length + skipSize) % list.size
                skipSize += 1
            })
            (list.head, list(1))
        }).head.productIterator.map(_.asInstanceOf[Int]).product

        println(v)

        val v2 = Seq(string.toCharArray.map(_.toInt) ++ Array(17, 31, 73, 47, 23)).map(array=>{
            val list = (0 to 255).toBuffer
            var pos = 0
            var skipSize = 0
            1 to 64 foreach {_=>
                array.foreach(length=>{
                    val v = list ++ list
                    var i = pos
                    v.slice(pos, pos + length).reverseIterator.foreach(element=>{
                        if(i >= list.size) i = 0
                        list(i) = element
                        i += 1
                    })
                    pos = (pos + length + skipSize) % list.size
                    skipSize += 1
                })
            }
            list.grouped(16).map(_.fold(0)(_ ^ _)).map("%02X".format(_).toLowerCase).mkString
        }).head

        println(v2)
    }
}
