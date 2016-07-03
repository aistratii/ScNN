import scala.collection.mutable.{IndexedSeqView, ListBuffer, Queue}

class NN(width: Int, height: Int, inputLength: Int, outputLength: Int) {
  var step: Double = 0.001
  val nodes: ListBuffer[ListBuffer[Node]] = new ListBuffer[ListBuffer[Node]]
  val weightQueue: Queue[(Int, Int, Int)] = new Queue


  private def initWeightQueue: Unit = {
    (for (i <- 1 until width + 2;
          j <- 0 until nodes(i).length;
          k <- 0 until nodes(i)(j).connectedNodes.length)
      yield (i, j, k)).toList.
      foreach( e => weightQueue += e)
  }
  initWeightQueue

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
          nodes(i)(j).connectedNodes += 0.5
    }
    linkNodes
  }
  buildNetStructure

  def improve(input: Array[Double]): Unit = {
    for ( i <- 0 until input.length )
      nodes(0)(i).value = input(i)

    change
  }

  private def change: Unit = {
    val wgtCrds = weightQueue.dequeue()
    weightQueue += wgtCrds

    val oldWeight = nodes(wgtCrds._1)(wgtCrds._2).connectedNodes(wgtCrds._3)
    val oldError = getError

    //try for first increase/decrease
    nodes(wgtCrds._1)(wgtCrds._2).connectedNodes(wgtCrds._3) += step

    //try for second increase/decrease
    val newError = getError
    if (newError > oldError){
      nodes(wgtCrds._1)(wgtCrds._2).connectedNodes(wgtCrds._3) = oldWeight - step
      if (getError > oldError){
        nodes(wgtCrds._1)(wgtCrds._2).connectedNodes(wgtCrds._3) = oldWeight
        println("Haven't improved")
      }
    }

  }

  private def getError: Double = ???

  private def sigmoidFunction(in: Double) = {
    1.0 / ( 1.0 + Math.pow(Math.E, -in))
  }

}
///010101010100101001010101