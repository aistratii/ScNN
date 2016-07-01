import scala.collection.mutable.{IndexedSeqView, ListBuffer}

class NN(width: Int, height: Int, inputLength: Int) {
  var step: Double = 0.001
  var length: Int = 10
  var displacement = 0

  val nodeValues: Array[Array[ListBuffer[Double]]] =
    Array.ofDim[Double](width, height).map(e=> e.map( el => new ListBuffer[Double]))
  nodeValues.foreach(
    line => line.foreach(
      list =>
        for (i <- 0 until height*height)
          list += 0.5
    )
  )
  nodeValues (0).foreach( e =>{
    e.clear()
    for(i <- 0 until inputLength*height)
      e += 0.5
  })


  def train(input: Array[Byte]):Unit = {
    getResultFor(input.slice(displacement, displacement + length))
  }


  private def getResultFor(input: Array[Byte]) = {
    val arrayTmp: Array[Array[Double]] = Array.ofDim[Double](width, height)

    //fill first layer
    for (i <- 0 until input.length){
      arrayTmp(0)(i) = input(i)
    }


    arrayTmp.foreach({ e =>
      e.foreach(println)
      println
    })


  }

  private def reduceToSums(array: IndexedSeq[Double]) = {

  }

  private def sigmoidFunction(in: Double) = {
    1.0 / ( 1.0 + Math.pow(Math.E, -in))
  }

}
///010101010100101001010101