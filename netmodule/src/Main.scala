object Main{
	def main(args: Array[String]) : Unit = {
		println("test")
		val nn: NN = new NN(3, 3, 10, 1)
    nn.nodes.foreach(i => {
      i.foreach( j => print(j.value +" "))
      println
    })
	}
}