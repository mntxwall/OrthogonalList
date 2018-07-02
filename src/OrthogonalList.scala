
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
  /*
  val vertexList: List[VertexNode] = List(
    new VertexNode("0", null, null),
    new VertexNode("1", null, null),
    new VertexNode("2", null, null),
    new VertexNode("3", null, null)

  )*/

  //How many edge in the graph
  val graphEdgeList: Array[GraphDescribe] = Array(
     GraphDescribe("0", "1", 1),
     GraphDescribe("0", "2", 12),
     GraphDescribe("1", "3", 3),
     GraphDescribe("1", "2", 9),
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
/*
  def findPath(startVertex: String, endVertex: String): Unit = {

    //var a: Int = sumWeight;

    if ( !startVertex.eq( endVertex )){
      var tmp: EdgeNode = vertexNodeList(startVertex.toInt).firstOut
      //var weight: Int = tmp.edgeInfo
      //a += sumWeight

      while(tmp != null){
        println(tmp.simpleString)
        //a += tmp.edgeInfo
        //println(s"the weight is $a")
        //findEdge(tmp)
        //findPath(tmp.headVertex, endVertex)
        //findEdge(tmp, tmp.edgeInfo)
        tmp = tmp.tailNext
      }

    }
    else{
      println(s"find return")

    }
  }
  */

  def findPath(startVertex: String, endVertex: String): Unit = {
    val startVertexNode: VertexNode = vertexNodeList(startVertex.toInt)
    val endVertexNode: VertexNode = vertexNodeList(endVertex.toInt)

    findPathExt(startVertexNode, endVertexNode, 0)

  }

  val resultList: ArrayBuffer[Int] = ArrayBuffer()

  //findPath("0", "4", 0)

  def findPathExt(startVertex: VertexNode, endVertex: VertexNode, weight: Int): Unit = {
    var a: Int = weight

    if( !(startVertex.data).eq(endVertex.data)){

      if(startVertex.firstOut != null) {

        a += startVertex.firstOut.edgeInfo
        //findEdge(startVertex.firstOut, startVertex.firstOut.edgeInfo, endVertex.data)
        findPathExt(vertexNodeList(startVertex.firstOut.headVertex.toInt), endVertex, startVertex.firstOut.edgeInfo)

        //startVertex = startVertex.firstOut
      }
    }
    else
      resultList += a
  }

  def findEdge(edge: EdgeNode, weight: Int, endVertexNode: String): Unit = {

    var a: Int = weight

    if(edge != null && !edge.headVertex.eq(endVertexNode)){

      a += edge.edgeInfo
      findEdge(edge.headNext, a, endVertexNode)
    }
    else{

      resultList += a
    }

  }

  findPath("0", "4")



  //println(resultList)
  resultList.foreach(println)

}
