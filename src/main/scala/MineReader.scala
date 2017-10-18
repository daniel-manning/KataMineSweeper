import scala.io.Source

class MineReader(fileName:String) {

  def parseMineLine(str: String): Array[Square] = {
    str.map {
      case '*' => Mine()
      case '.' => Safe(0)
    }.toArray
  }

  def parseMineFile:MineField = {
    val bfs = Source.fromURL(getClass.getResource(fileName))
    val lineIt = bfs.getLines()

    //TODO: Needs a functional while loop to see if we're finished after reading
    val Array(h,w) = lineIt.next().split(" ").take(2).map(_.toInt)
    val mf = MineField(h,w)
    for(n <- 1 to h){
      mf.mines(n-1) = parseMineLine(lineIt.next())
    }

    //When done close source
    bfs.close()

    mf
  }

}
