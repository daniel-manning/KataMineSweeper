import org.scalatest.{FlatSpec, Matchers}

class ReaderSpec extends FlatSpec with Matchers {

  "A test minefield file" should "create a correct sized minefield" in {
    val mineField: MineField = new MineReader("MineTest").parseMineFile
    mineField shouldBe MineField(4,4)
  }

  "A test minefield file" should "create the correct first row of minefield" in {
    val mineField: MineField = new MineReader("MineTest").parseMineFile
    mineField.mines(0) shouldBe Array(Mine(),Safe(0),Safe(0),Safe(0))
  }

  "Neighbours of a position" should "have 8 members" in {
    MineUtils.neighbours.length shouldBe 8
  }


  "A test minefield file" should "count the number of mines correctly" in {
    val mineField: MineField = new MineReader("MineTest").parseMineFile
    MineUtils.countNeighbours((1,1),mineField) shouldBe 2
  }


}
