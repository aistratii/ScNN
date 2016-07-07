import scala.collection.mutable.{ListBuffer, Queue}

class NN(width: Int, height: Int, inputLength: Int, outputLength: Int) {
  var step: Double = 0.1
  val errorThreshold = 0.3
  val nodes: ListBuffer[ListBuffer[Node]] = new ListBuffer[ListBuffer[Node]]
  val weightQueue: Queue[(Int, Int, Int)] = new Queue
  val displacementNode = new Node


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
      yield (i, j, k)).
      toList.
      foreach( e => weightQueue += e)
  }
  initWeightQueue

  private def insertDisplacementNode: Unit = {
    nodes(0) += displacementNode
    nodes(1).foreach( node => {
      node.weights += 1
    })
  }
  insertDisplacementNode

  /**
    * Returns true in case if net can construt the expected result,
    * otherwise it's false.
    * @param input
    * @param output
    * @return
    */
  def improve(input: Array[Double], output: Array[Double], displacement: Int): Boolean = {
    fillInputLayer(input, displacement)
    print("input: ")
    nodes(0).foreach(e => print(e.value +" "))
    println

    if (getError(output) > errorThreshold){
      print("x Not Accetable!: ")
      nodes(width +1).foreach(e => print(e.value +" "))
      println
      print("should be: ")
      output.foreach(e => print(e +" "))
      println


      tryDescend(output)
      false
    }
    else {
      print("o Acceptable!: ")
      nodes(width +1).foreach(e => print(e.value +" "))
      println
      print("should be: ")
      output.foreach(e => print(e +" "))
      println
      true
    }
  }


  private def fillInputLayer(input: Array[Double], displacement: Int): Unit = {
    for ( i <- 0 until input.length )
      nodes(0)(i).value = input(i)

    displacementNode.value = displacement
  }


  private def tryDescend(output: Array[Double]): Unit = {
    val wgtCrds = weightQueue.dequeue()
    weightQueue += wgtCrds

    val oldWeight = nodes(wgtCrds._1)(wgtCrds._2).weights(wgtCrds._3)
    val oldError = getError(output)


    //try for first increase/decrease
    nodes(wgtCrds._1)(wgtCrds._2).weights(wgtCrds._3) += step

    val newError = getError(output)
    if (newError > oldError){
      //try for second increase/decrease
      nodes(wgtCrds._1)(wgtCrds._2).weights(wgtCrds._3) = oldWeight - step

      //if it doesn't improve, revert
      if (getError(output) > oldError){
        nodes(wgtCrds._1)(wgtCrds._2).weights(wgtCrds._3) = oldWeight
        //step /= 2
        println("Haven't improved")
      } //else step *= 2
    }
    //else step *= 2
    //println(s"step $step")
  }


  /*private def aproximate: Unit = {
    for ( i <- 0 until nodes(width+1).length ){
      val node = nodes(width +1)(i)

      def dif(target: Int) = Math.abs( node.value - target )

      if (dif(1) < errorThreshold)
        node.value = 1
      else if (dif(0) < errorThreshold)
        node.value = 0
    }
  }*/


  private def recalculateNet: Unit = {
    val sum =
      for( i <- 1 until nodes.length;
           j <- 0 until nodes(i).length)
              nodes(i)(j).value =
                sigmoidFunction((
                  for( k <- 0 until nodes(i)(j).weights.length)
                    yield
                      nodes(i-1)(k).value * nodes(i)(j).weights(k)
                  ).
                  sum
                )

    //aproximate
  }


  def getError(output: Array[Double]): Double = {
    recalculateNet

    (for ( i <- 0 until nodes(width +1).length )
      yield
        Math.abs(
          Math.abs(nodes(width +1)(i).value) - Math.abs(output(i)))
      ).
      sum
  }


  def isAcceptable(output: Array[Double]): Boolean =
    getError(output) < errorThreshold


  private def sigmoidFunction(in: Double) = {
    1.0 / ( 1.0 + Math.pow(Math.E, -in))
    //in
  }
}