import scala.collection.mutable.IndexedSeqView

class NN(width: Int, height: Int) {
  var step: Double = 0.001
  var length: Int = 10
  var displacement = 0

  val nodeValues: Array[Array[Double]] =
    Array.ofDim[Double](width, height)


  val weights: Array[Array[Double]] =
    Array.ofDim[Double](width, height).
      map( line =>
        line.map(_=>0.5)
      )


  def train(input: Array[Byte]):Unit = {
    getResultFor(input.slice(displacement, displacement + length))
  }


  private def getResultFor(input: Array[Byte]) = {
    val arrayTmp: Array[Array[Double]] = Array.ofDim[Double](width, height)

    val first = for (j <- 0 to height; i <- 0 to input.length) yield input(i) * weights(j)(0)
    println(first)
  }

  private def sigmoidFunction(in: Double) = {
    1.0 / ( 1.0 + Math.pow(Math.E, -in))
  }

}
///010101010100101001010101