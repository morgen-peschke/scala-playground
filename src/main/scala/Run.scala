import BinaryTree._

object Runner {
  def main(args: Array[String]): Unit = {
    val root = generateTree
    traverseInOrder(root, n => print(n.value + ", "))
    println()
    println(s"Height: ${height2(root)}")
    println(s"IsBalanced: ${isBalanced(root)}")
  }

  def generateTree: BinaryTree[Int] = {
    val n6 =
      Node(
        6,
        Node(271, 28, 0),
        Node.rightOnly(
          561,
          Node.leftOnly(3, 17)
        )
      )

    val n6i = Node(
      6,
      Node.rightOnly(
        2,
        Node(
          1,
          Node.rightOnly(401, 641),
          Node(257)
        )
      ),
      Node.rightOnly(271, 28)
    )

    Node(314, n6, n6i)
  }
}
