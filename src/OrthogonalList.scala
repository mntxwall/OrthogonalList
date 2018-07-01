
import scala.collection.mutable.ArrayBuffer

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
    new GraphDescribe("0", "1", 1),
    new GraphDescribe("0", "2", 12),
    new GraphDescribe("1", "3", 3),
    new GraphDescribe("1", "2", 9),
    new GraphDescribe("2", "4", 5),
    new GraphDescribe("3", "2", 4),
    new GraphDescribe("3", "4", 13),
    new GraphDescribe("3", "5", 15),
    new GraphDescribe("4", "5", 4)
  )


  // val test = (x: GraphDescribe) => x.headVertex = "t"

  //  graph.foreach(test)

  // println(graph(1).headVertex)

  //make the vertexNodeList according to the Vertex node list
  graphVertexList.foreach( x =>
    vertexNodeList += new VertexNode(x, null, null))

  graphEdgeList.foreach( x =>
    edgeList += new EdgeNode(x.tailVertex, x.headVertex, null, null, x.dateInfo))


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

  def findPath(startVertex: String, endVertex: String):Unit = {

    if ( !startVertex.eq( endVertex )){
      var tmp: EdgeNode = vertexNodeList(startVertex.toInt).firstOut
      while(tmp != null){
        println(tmp.simpleString)
        findPath(tmp.headVertex, endVertex)
        tmp = tmp.tailNext
      }

    }
    else{
      println("find return")
    }
  }


  findPath("0", "4")


}
