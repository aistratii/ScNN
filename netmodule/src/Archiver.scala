import scala.io.Source
import Math.min

/**
  * Created by aistratii on 05-07-2016.
  */
class Archiver(file: String,
               chunkReadSize: Int = 1000,
               inputSize: Int = 10,
               outputSize: Int = 1,
               width: Int = 5,
               height: Int = 5) {

  private val net: NN = new NN(width, height, inputSize, outputSize)

  def compress: Unit = {
    lazy val source = Source.fromFile(file).map(_.toByte)
    var chunk = nextChunk(source)
    var numberOfCorectAproximations = 0
    val size = source.size
    var displacement = 0

    while(numberOfCorectAproximations < min(size, chunkReadSize)){
      for (i <- 0 until chunk.length){

        val input = (for (i <- displacement until displacement + inputSize)
          yield chunk(i)).toArray[Double]

        val output = (for (i <- displacement + inputSize until displacement + inputSize + outputSize)
          yield chunk(i)).toArray[Double]

        if(!net.improve(input, output))
          i = 0
      }
    }
  }

  private def nextChunk(source: Iterator[Byte]) = {
    (for(i <- 0 until chunkReadSize)
      yield source.next()).toArray[Byte]
  }
}
