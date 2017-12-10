package com.github.natanbc.aoc

object Day3 {
    def main(args: Array[String]): Unit = {
        val n = 368078
        val v = ({
            var streamA: Stream[((Int, Int), (Int, Int), Set[(Int, Int)])] = null
                streamA = ((0, 0), (0, -1), Set((0, 0))) #:: streamA.map(e => {
                val (position, direction, set) = e
                val turn = direction match {
                    case (1, 0) => (0, 1)
                    case (0, 1) => (-1, 0)
                    case (-1, 0) => (0, -1)
                    case (0, -1) => (1, 0)
                }
                if(set.contains((position._1 + turn._1, position._2 + turn._2)))
                    ((position._1 + direction._1, position._2 + direction._2), direction, set + ((position._1 + direction._1, position._2 + direction._2)))
                else
                    ((position._1 + turn._1, position._2 + turn._2), turn, set + ((position._1 + turn._1, position._2 + turn._2)))
            })
            streamA
        })(n-1)._1.productIterator.map(_.asInstanceOf[Int].abs).sum

        println(v)

        val v2 = {
            var streamB: Stream[(((Int, Int), (Int, Int), Set[(Int, Int)]), Map[(Int, Int), Int])] = null
            streamB = (((0, 0), (0, -1), Set((0, 0))), Map((0, 0) -> 1)) #:: streamB.map (e => {
                val (cursor, map) = e
                val next = {
                    val (position, direction, set) = cursor
                    val turn = direction match {
                        case (1, 0) => (0, 1)
                        case (0, 1) => (-1, 0)
                        case (-1, 0) => (0, -1)
                        case (0, -1) => (1, 0)
                    }
                    if(set.contains((position._1 + turn._1, position._2 + turn._2)))
                        ((position._1 + direction._1, position._2 + direction._2), direction, set + ((position._1 + direction._1, position._2 + direction._2)))
                    else
                        ((position._1 + turn._1, position._2 + turn._2), turn, set + ((position._1 + turn._1, position._2 + turn._2)))
                }
                val sum = (for(i <- -1 to 1; j <- -1 to 1; if i != 0 || j != 0) yield map.getOrElse((next._1._1 + i, next._1._2 + j), 0)).sum
                (next, map + (next._1 -> sum))
            })
            streamB
        }.find({ case ((position, _, _), map) => map.getOrElse(position, 0) >= n }) match {
            case Some(e) => e._2(e._1._1).toString
        }

        println(v2)
    }
}
