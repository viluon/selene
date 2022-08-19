package me.viluon.computercraft.compression

import me.viluon.lua.computercraft.CCLibrary

trait Lzw extends CCLibrary {
  def compress(tc: Rep[String]): Rep[Seq[Int]] = {
    val startDict = mapToRepMap(initialDictionary.toMap)
    val (fullDict, result, remain) = t3(tc.foldLeft((startDict, List[Int](), "": Rep[String])) {
      case (t, nextChar) =>
        val (dict, res, leftOver) = t3(t)
        if (dict.contains(leftOver + nextChar)) // current substring already in dict
          (dict, res, leftOver + nextChar)
        else if (dict.size < 4096) // add to dictionary
          (dict + ((leftOver + nextChar, dict.size + 1)), dict(leftOver) :: res, "" + nextChar)
        else // dictionary is full
          make_tuple3((dict, dict(leftOver) :: res, "" + nextChar))
    })
    if (remain.isEmpty) result.reverse else (fullDict(remain) :: result).reverse
  }

  def decompress(ns: Rep[Seq[Int]]): Rep[String] = {
    val startDict = mapToRepMap(initialDictionary.map(_.swap).toMap)
//    val (_, result, _) =
//      ns.foldLeft[(Map[Int, String], List[String], Option[(Int, String)])]((startDict, Nil, None)) {
//        case (t, n) => {
//          val (dict, result, conjecture) = t3(t)
//          dict.get(n) match {
//            case Some(output) => {
//              val (newDict, newCode) = conjecture match {
//                case Some((code, prefix)) => ((dict + (code -> (prefix + output.head))), code + 1)
//                case None => (dict, dict.size + 1)
//              }
//              (newDict, output :: result, Some(newCode -> output))
//            }
//            case None => {
//              // conjecture being None would be an encoding error
//              val (code, prefix) = conjecture.get
//              val output = prefix + prefix.head
//              (dict + (code -> output), output :: result, Some(code + 1 -> output))
//            }
//          }
//        }
//      }
//    result.reverse.mkString("")
    ???
  }

  private def initialDictionary = (1 to 255).map(a => ("" + a.toChar, a))

  def testLzwCompression(): Rep[Unit] = {
    val text = "TOBEORNOTTOBEORTOBEORNOT"
    val compressed = compress(text)
    println(compressed)
    val result = decompress(compressed)
    println(result)
  }
}
