object Main{
	def main(args: Array[String]): Unit = {
		println("test")
		val nn: NN = new NN(3,3)
    nn.train(Array[Byte](1, 2, 1, 1))
	}
}