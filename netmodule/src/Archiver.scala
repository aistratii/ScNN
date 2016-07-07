import scala.io.Source
import Math.min

import scala.collection.mutable.ListBuffer

/**
  * Created by aistratii on 05-07-2016.
  */
class Archiver(file: String,
               var chunkMaxSize: Int = 5,
               var inputSize: Int = 10,
               var outputSize: Int = 1,
               var width: Int = 5,
               var height: Int = 5) {

  private val net: NN = new NN(width, height, inputSize, outputSize)
  private var totalDisplacement: Int = 0
  private val source = new FileDao(file)
  //private val source = Source.fromFile(file).map(_.toByte)

  def compress: Unit = {
    println("Compressing...")

    var chunk = nextChunk
    println(chunk.toList)
    var displacement = 0

    /*while(displacement < chunk.size - outputSize ){
      (0 until chunk.length - inputSize).takeWhile( i => {

        val input = (for (i <- displacement until displacement + inputSize)
          yield chunk(i)).toArray

        val output = (for (i <- displacement + inputSize until displacement + inputSize + outputSize)
          yield chunk(i)).toArray

        val response = net.improve(input, output, totalDisplacement + displacement)

        if (response)
          displacement += 1
        else
          displacement = 0



        println("disp: " + totalDisplacement + " " + displacement)

        //println(net.getError(output))
        //Thread.sleep(300)
        response
      })
    }
    totalDisplacement += chunk.length*/

    var isGenerable: Boolean = true
    do {
      isGenerable = true

      for ( displacement <- 0 until chunk.size - inputSize){
        val input = (for (i <- displacement until displacement + inputSize)
          yield chunk(i)).toArray

        val output = (for (i <- displacement + inputSize until displacement + inputSize + outputSize)
          yield chunk(i)).toArray

        var isSuccessfull = net.improve(input, output, totalDisplacement + displacement)
        if (!net.improve(input, output, totalDisplacement + displacement))
          isGenerable = false

        println("displacement: " + displacement)
      }

    } while (!isGenerable)

  }


  private def nextChunk: Array[Byte] = {
    /*val response = new ListBuffer[Byte]
    (0 until chunkMaxSize).
      takeWhile(_ => {
        response += source.next()
        source.hasNext
      })

    println(s"next chunk: ${response.map(_.toChar)}")
    response.toArray[Byte]*/

    val array = source.nextChunk(chunkMaxSize).
      map(byte => Integer.toBinaryString(byte)).
      map(string => string.toCharArray.map(
        char =>
          if (char == '1')
            1.toByte
          else 0.toByte
      ))

    array.flatMap(e => e)
  }

  private implicit def ABtoAD(input: Array[Byte]): Array[Double] = {
    input.map(_.toDouble)
  }
}
