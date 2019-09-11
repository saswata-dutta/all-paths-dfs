import scala.collection.mutable
import scala.io.StdIn

object Solution {

  case class Edge(src: Int, dst: Int)

  def main(args: Array[String]): Unit = {
    val n = StdIn.readInt()

    var done = false
    val edges = mutable.Buffer[Edge]()

    while (!done) {
      val line = StdIn.readLine()
      if (line == null || line.trim.isEmpty) done = true
      else {
        val Array(src, dst) = line.split(",").map(_.trim.toInt)
        edges += Edge(src, dst)
      }
    }

    val adjacencyList: Map[Int, Set[Int]] =
      edges.groupBy(_.src).mapValues(v => v.map(_.dst).toSet)

    val sources = (0 until n).toSet -- edges.groupBy(_.dst).keys.toSet

    // sorting to just make test cases pass
    sources.toSeq.sorted
      .foreach(node => printPaths(node, adjacencyList, mutable.Set[Int](), Seq.empty[Int]))
  }

  def printPaths(
    current: Int,
    adjacencyList: Map[Int, Set[Int]],
    visited: mutable.Set[Int],
    pathSoFar: Seq[Int]
  ): Unit = {
    val currentPath = pathSoFar :+ current
    if (!adjacencyList.contains(current)) {
      println(currentPath.mkString("->"))
    } else {
      visited += current
      val neighbours = adjacencyList(current)
      neighbours
        .filterNot(visited)
        .foreach(node => printPaths(node, adjacencyList, visited, currentPath))
      visited -= current
    }
  }
}
