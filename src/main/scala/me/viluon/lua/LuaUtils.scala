package me.viluon.lua

object LuaUtils {
  def formatLua(code: String, indentString: String = "  "): String = {
    val it = for {line <- code.lines} yield {
      // FIXME incomplete keyword list, doesn't handle () [] {}
      "\\b(function|then|do|elseif|else|end)\\b".r.findFirstMatchIn(line) match {
        case Some(x) => x.toString match {
          case "function" | "then" | "do" => (0, 1)
          case "elseif" | "else" => (-1, 0)
          case "end" => (-1, -1)
        }
        case None => (0, 0)
      }
    }
    code.lines.zip(it).foldLeft((0, List[String]()))({
      case ((indent, acc), (line, (curr, next))) =>
        (indent + next, indentString * (indent + curr) + line :: acc)
    })._2.reverse.mkString("\n")
  }
}
