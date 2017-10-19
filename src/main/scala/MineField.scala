sealed trait Square
case class Mine() extends Square
case class Safe(n:Int) extends Square

case class MineField(height:Int, width:Int){
  val mines:Array[Array[Square]] = Array.ofDim[Square](height,width)
}

object MineUtils {

  def print(square: Square):Unit = {
    square match {
      case Mine() => Console.print('*')
      case Safe(n) => Console.print(n)
    }
  }

  def print(field:Array[Array[Square]]):Unit = {
      field.foreach( arr => {arr.foreach(print); Console.print('\n')})
  }

  def print(board:Seq[(Square, (Int, Int))]):Unit = {
    board.foreach(n => {if(n._2._2 == 0){Console.print('\n')};print(n._1)})
  }

  val neighbours: List[(Int, Int)] = for{
    i <- List(-1,0,1)
    j <- List(-1,0,1)
    if( !(i == 0 && j == 0))
  } yield (i,j)

  def countNeighbours(position:(Int,Int), board:MineField):Int = {
    neighbours.map( vect => if(insideBounds(position, vect, board.height, board.width)){
      val square: Square = board.mines(position._1 + vect._1)(position._2 + vect._2)
        square match {
        case Mine() => 1
        case _ => 0
      }
    } else {
      0
    }).sum
  }

  def insideBounds(position:(Int,Int), vect:(Int,Int), maxHeight:Int, maxWidth:Int):Boolean = {
    (position._1 + vect._1) >= 0 && (position._1 + vect._1) < maxWidth &&
      (position._2 + vect._2) >= 0 && (position._2 + vect._2) < maxHeight
  }

  def zipWith2DArray[A](arr:Array[Array[A]]):Array[(A,(Int,Int))] = arr.map(_.zipWithIndex).zipWithIndex.flatMap( n => n._1.map( xs => (xs._1,(n._2,xs._2))) )


}