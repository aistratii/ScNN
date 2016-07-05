import scala.collection.mutable.{ListBuffer, Queue}

class NN(width: Int, height: Int, inputLength: Int, outputLength: Int) {
  var step: Double = 0.001
  val errorThreshold = 0.2
  val nodes: ListBuffer[ListBuffer[Node]] = new ListBuffer[ListBuffer[Node]]
  val weightQueue: Queue[(Int, Int, Int)] = new Queue


  private def buildNetStructure: Unit = {
    def createNodes: Unit = {
      for( i <- 0 until width + 2 )
        nodes += new ListBuffer[Node]

      for( i <- 0 until inputLength )
        nodes(0) += new Node

      for ( i <- 1 until width +1; j <- 0 until height)
        nodes(i) += new Node

      for ( i <- 0 until outputLength)
        nodes(width +1) += new Node
      }
    createNodes

    def linkNodes:Unit = {
      for ( i <- 1 until width + 2; j <- 0 until nodes(i).length )
        for ( k <- 0 until nodes(i-1).length )
          nodes(i)(j).weights += 0.5
    }
    linkNodes
  }
  buildNetStructure

  private def initWeightQueue: Unit = {
    (for (i <- 1 until width + 2;
          j <- 0 until nodes(i).length;
          k <- 0 until nodes(i)(j).weights.length)
      yield (i, j, k)).toList.
      foreach( e => weightQueue += e)
  }
  initWeightQueue


  /**
    * Returns true in case if net can construxt the expected result,
    * otherwise it's false.
    * @param input
    * @param output
    * @return
    */
  def improve(input: Array[Double], output: Array[Double]): Boolean = {
    fillInputLayer(input)

    if (getError(output) > errorThreshold){
      tryDescend(output)
      false
    }
    else {
      nodes(width +1).foreach(e => println(e.value))
      true
    }
  }


  private def fillInputLayer(input: Array[Double]): Unit = {
    for ( i <- 0 until input.length )
      nodes(0)(i).value = input(i)
  }


  private def tryDescend(output: Array[Double]): Unit = {
    val wgtCrds = weightQueue.dequeue()
    weightQueue += wgtCrds

    val oldWeight = nodes(wgtCrds._1)(wgtCrds._2).weights(wgtCrds._3)
    val oldError = getError(output)

    //try for first increase/decrease
    nodes(wgtCrds._1)(wgtCrds._2).weights(wgtCrds._3) += step

    //try for second increase/decrease
    val newError = getError(output)
    if (newError > oldError){
      nodes(wgtCrds._1)(wgtCrds._2).weights(wgtCrds._3) = oldWeight - step

      //if it doesn't improve, revert
      if (getError(output) > oldError){
        nodes(wgtCrds._1)(wgtCrds._2).weights(wgtCrds._3) = oldWeight
        println("Haven't improved")
      }
    }
  }

  private def aproximate: Unit = {
    for ( i <- 0 until nodes(width+1).length ){
      val node = nodes(width +1)(i)

      def dif(target: Int) = Math.abs( node.value - target )

      if (dif(1) < errorThreshold)
        node.value = 1
      else if (dif(0) < errorThreshold)
        node.value = 0
    }
  }

  private def recalculateNet: Unit = {
    for( i <- 1 until width +2;
         j <- 0 until nodes(i).length)
      nodes(i)(j).value = (
        for( k <- 0 until nodes(i)(j).weights.length)
          yield
            sigmoidFunction(
              nodes(i-1)(k).value * nodes(i)(j).weights(k)
            )
        ).sum

    //aproximate
  }

  private def getError(output: Array[Double]): Double = {
    recalculateNet

    (for ( i <- 0 until nodes(width +1).length )
      yield
        Math.abs(nodes(width +1)(i).value - output(i))).
      sum
  }

  def isAcceptable(output: Array[Double]): Boolean =
    getError(output) < errorThreshold

  private def sigmoidFunction(in: Double) = {
    1.0 / ( 1.0 + Math.pow(Math.E, -in))
  }
}
///010101010100101001010101