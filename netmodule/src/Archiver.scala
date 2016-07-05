import scala.io.Source
import Math.min

import scala.collection.mutable.ListBuffer

/**
  * Created by aistratii on 05-07-2016.
  */
class Archiver(file: String,
               chunkMaxSize: Int = 5,
               inputSize: Int = 10,
               outputSize: Int = 1,
               width: Int = 5,
               height: Int = 5) {

  private val net: NN = new NN(width, height, inputSize, outputSize)

  def compress: Unit = {
    println("Compressing...")
    lazy val source = Source.fromFile(file).map(_.toByte)
    var chunk = nextChunk(source)
    var displacement = 0

    while(displacement < chunk.size - outputSize ){
      (0 until chunk.length - inputSize).takeWhile( i => {

        val input = (for (i <- displacement until displacement + inputSize)
          yield chunk(i)).toArray

        val output = (for (i <- displacement + inputSize until displacement + inputSize + outputSize)
          yield chunk(i)).toArray

        //println(output.toList)

        val response = net.improve(input, output)

        if (response)
          displacement += 1
        else
          displacement = 0


        println(displacement)

        println(net.getError(output))
        //Thread.sleep(300)
        response
      })
    }
  }

  private def nextChunk(source: Iterator[Byte]) = {
    val response = new ListBuffer[Byte]
    (0 until chunkMaxSize).
      takeWhile(_ => {
        response += source.next()
        source.hasNext
      })

    println(s"next chunk: ${response.map(_.toChar)}")

    response.toArray[Byte]
  }

  private implicit def ABtoAD(input: Array[Byte]): Array[Double] = {
    input.map(_.toDouble)
  }
}
