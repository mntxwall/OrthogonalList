
import scala.collection.mutable.ArrayBuffer

/**
  *Author MntCw
  *Data 2018/7/1
  *
*/

/*
* 总体思路是使用十字链表建立图后
* 遍例图，找到两个点间最小的距离
*
* */

//十字链表中图的边的数据结构
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
//十字链表中图的顶点的数据结构
case class VertexNode(var data: String, var firstIn: EdgeNode, var firstOut: EdgeNode)

object OrthogonalList extends App {

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
      insertTail(tailVertexNode.firstOut, x)
    }

    if (headVertexNode.firstIn == null){
      headVertexNode.firstIn = x
    }
    else {
      insertHead(headVertexNode.firstIn, x)
    }

  }

  edgeList.foreach( insertEdge )

  /*
  * 存储遍历时经过的点，
  * 用于判断是否有环和记录最后找到的结果
  */
  var tmpPath: ArrayBuffer[String] = ArrayBuffer()

  def findPath(startVertex: String, endVertex: String): Unit = {
    val startVertexNode: VertexNode = vertexNodeList(startVertex.toInt)
    val endVertexNode: VertexNode = vertexNodeList(endVertex.toInt)

    tmpPath += startVertex
    findPathExt(startVertexNode, endVertexNode)

  }

  //store the resultPath
  var maxPathLength: Int = 0
  val resultVertexList: ArrayBuffer[ArrayBuffer[String]] = ArrayBuffer()


  def findPathExt(startVertex: VertexNode, endVertex: VertexNode,
                  weight: Int = 0): Unit = {
    var a: Int = weight

    var tmp: EdgeNode = startVertex.firstOut

    if( ! startVertex.data.eq(endVertex.data)){

      while(tmp != null && tmpPath.indexOf(tmp.headVertex) == -1){
          a += tmp.edgeInfo
          tmpPath += tmp.headVertex
          findPathExt(vertexNodeList(tmp.headVertex.toInt), endVertex, a)
          a -= tmp.edgeInfo
          tmpPath -= tmp.headVertex
          tmp = tmp.tailNext
      }
    }
    else{
      /*
      * 找到路径后，对比保留最小
      * */
      if(  maxPathLength >= a || maxPathLength == 0){
        maxPathLength = a
        resultVertexList += tmpPath.clone()
      }
    }


  }
  findPath("0", "4")

  println(maxPathLength)
  resultVertexList.foreach(println)

}
