object Main{
	def main(args: Array[String]) : Unit = {
		/*println("test")
    val input = Array[Double](0, 1, 1, 0, 0, 1, 0, 0)
    val output = Array[Double](0)
		val nn: NN = new NN(3, 3, input.length, output.length)
    /*nn.nodes.foreach(i => {
      i.foreach( j => print(j.value +" "))
      println
    })*/
    //nn.improve(input, output)
    var error = nn.getError(output)
    while (error > nn.errorThreshold){
      nn.improve(input, output)
      println(error)
      error = nn.getError(output)
    }*/

    val archiver = new Archiver("C:\\Users\\aistratii\\Desktop\\in.txt",
      width = 10,
      height = 3,
      inputSize = 2)
    archiver.compress
	}
}