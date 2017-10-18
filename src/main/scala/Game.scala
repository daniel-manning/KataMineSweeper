import scala.io.Source

object Game extends App{

  val mineField: MineField = new MineReader("MineTest").parseMineFile
  val countList: Seq[(Square, (Int, Int))] = MineUtils.zipWith2DArray(mineField.mines).transform(n => n._1 match {
    case Mine() => (Mine(), n._2)
    case Safe(_) => (Safe(MineUtils.countNeighbours(n._2, mineField)), n._2)
  })
  MineUtils.print(countList)
}
