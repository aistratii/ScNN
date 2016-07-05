import scala.io.Source
import Math.min

import scala.collection.mutable.ListBuffer

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
      (0 until chunk.length).toStream.takeWhile( i => {

        val input = (for (i <- displacement until displacement + inputSize)
          yield chunk(i)).toArray

        val output = (for (i <- displacement + inputSize until displacement + inputSize + outputSize)
          yield chunk(i)).toArray

        val response = net.improve(input, output)

        if (response)
          numberOfCorectAproximations+=1
        else
          numberOfCorectAproximations = 0

        println(numberOfCorectAproximations)
        response
      })
      }
    }

  private def nextChunk(source: Iterator[Byte]) = {
    val response = new ListBuffer[Byte]
    (0 until min(source.size, chunkReadSize)).
      takeWhile(_ => source.nonEmpty).
      foreach(_ => response += source.next())

    /*(for(i <- 0 until chunkReadSize)
      yield source.next()).toArray[Byte]*/
    response.toArray[Byte]
  }

  private implicit def ABtoAD(input: Array[Byte]): Array[Double] = {
    input.map(_.toDouble)
  }
}
