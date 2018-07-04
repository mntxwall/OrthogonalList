
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

case class EdgeNode(var tailVertex: String, var headVertex: String,
                    var tailNext: EdgeNode, var headNext: EdgeNode,
                    var edgeInfo: Int = 0){
  override def toString: String = {
    s"$tailVertex->$headVertex $tailNext $headNext"
  }

  def simpleString: String = {
    s"$tailVertex->$headVertex"
  }
}
case class VertexNode(var data: String, var firstIn: EdgeNode, var firstOut: EdgeNode)

object OrthogonalList extends App {
  println("HelloWorld!")

  case class GraphDescribe(var tailVertex: String, var headVertex: String, var dateInfo: Int = 0){

    override def toString: String = {
      s"$tailVertex -> $headVertex"
    }
  }

  //save the Vertex Node
  val vertexNodeList: ArrayBuffer[VertexNode] = ArrayBuffer()

  val edgeList: ArrayBuffer[EdgeNode] = ArrayBuffer()

  //How many vertex in the graph
  val graphVertexList: List[String] = List("0", "1", "2", "3", "4", "5")

  //How many edge in the graph
  val graphEdgeList: Array[GraphDescribe] = Array(
     GraphDescribe("0", "1", 1),
     GraphDescribe("0", "2", 8),
     GraphDescribe("1", "3", 3),
     GraphDescribe("1", "2", 9),
     GraphDescribe("2", "1", 1),
     GraphDescribe("2", "4", 5),
     GraphDescribe("3", "2", 4),
     GraphDescribe("3", "4", 13),
     GraphDescribe("3", "5", 15),
     GraphDescribe("4", "5", 4)
  )


  // val test = (x: GraphDescribe) => x.headVertex = "t"

  //  graph.foreach(test)

  // println(graph(1).headVertex)

  //make the vertexNodeList according to the Vertex node list
  graphVertexList.foreach( x =>
    vertexNodeList +=  VertexNode(x, null, null))

  graphEdgeList.foreach( x =>
    edgeList +=  EdgeNode(x.tailVertex, x.headVertex, null, null, x.dateInfo))


  def insertTail ( x: EdgeNode,  y: EdgeNode):Unit = {

    if (x.tailNext != null) {
      insertTail(x.tailNext, y)
    }
    else x.tailNext = y
  }

  def insertHead(x: EdgeNode, y: EdgeNode):Unit = {
    if(x.headNext != null) insertHead(x.headNext, y)
    else x.headNext = y
  }

  val insertEdge = (x: EdgeNode) => {

    val tailVertexIndex = x.tailVertex
    val headVertexIndex = x.headVertex

    val tailVertexNode: VertexNode = vertexNodeList(tailVertexIndex.toInt)
    val headVertexNode: VertexNode = vertexNodeList(headVertexIndex.toInt)

    if (tailVertexNode.firstOut == null){
      tailVertexNode.firstOut = x
    }
    else {
      //var tempEdge: EdgeNode = tailVertexNode.firstOut
      //while (tempEdge.tailNext != null) tempEdge = tempEdge.tailNext
      //tempEdge.tailNext = x
      insertTail(tailVertexNode.firstOut, x)
    }

    if (headVertexNode.firstIn == null){
      headVertexNode.firstIn = x
    }
    else {
      //var tempEdge: EdgeNode = headVertexNode.firstIn
      //while (tempEdge.headNext != null) tempEdge = tempEdge.headNext
      //tempEdge.headNext = x
      insertHead(headVertexNode.firstIn, x)
    }

  }

  //

 // println(edgeList)
  edgeList.foreach( insertEdge )
  //println(vertexNodeList)

  var tmpPath: ArrayBuffer[String] = ArrayBuffer()
  //val b: ArrayBuffer[String] = ArrayBuffer()

  def findPath(startVertex: String, endVertex: String): Unit = {
    val startVertexNode: VertexNode = vertexNodeList(startVertex.toInt)
    val endVertexNode: VertexNode = vertexNodeList(endVertex.toInt)

    tmpPath += startVertex

    //b += startVertex

    findPathExt(startVertexNode, endVertexNode)

  }

  //val resultList: ArrayBuffer[Int] = ArrayBuffer()

  //store the resultPath
  var maxPathLength: Int = 0
  val resultVertexList: ArrayBuffer[ArrayBuffer[String]] = ArrayBuffer()


  //b.find(1)

 // val resultHashMap: mutable.HashMap[ArrayBuffer, ]

  //findPath("0", "4", 0)

  def findPathExt(startVertex: VertexNode, endVertex: VertexNode,
                  weight: Int = 0): Unit = {
    var a: Int = weight

    var tmp: EdgeNode = startVertex.firstOut

    //var tmpPath: ArrayBuffer[String] = path

    if( ! startVertex.data.eq(endVertex.data)){


      //if(startVertex.firstOut != null) {
      while(tmp != null && tmpPath.indexOf(tmp.headVertex) == -1){
        //val tmp: EdgeNode = vertexNodeList(startVertex.firstOut.headVertex.toInt)
        //findPathExt(vertexNodeList(startVertex.firstOut.headVertex.toInt), endVertex, a)
          a += tmp.edgeInfo
          tmpPath += tmp.headVertex
          //tmpPath.append(tmp.headVertex)
          //resultVertexList += tmp.headVertex
          //findEdge(startVertex.firstOut, startVertex.firstOut.edgeInfo, endVertex.data)

          println(startVertex.firstOut.simpleString)
          println(s"the weight is $a")
          findPathExt(vertexNodeList(tmp.headVertex.toInt), endVertex, a)
          a -= tmp.edgeInfo
          tmpPath -= tmp.headVertex
          tmp = tmp.tailNext

        //startVertex = startVertex.firstOut
      }
    }
    else{
      //resultList += a

      //b += tmpPath.clone()

      if(maxPathLength == 0 ||  maxPathLength >= a){
        maxPathLength = a
        resultVertexList += tmpPath.clone()
      }
    }


  }
  findPath("0", "4")


  //resultList.foreach(println)
  //b.foreach(println)
  println(maxPathLength)
  resultVertexList.foreach(println)

}
