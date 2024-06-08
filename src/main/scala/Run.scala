import BinaryTree._

object Runner {
  def main(args: Array[String]): Unit = {
    val root = generateTree
    println(root.foldValuesNRL(Vector.newBuilder[Int])(_.addOne(_)).result().mkString(", "))
    println(s"Height: ${height0(root)} / ${height1(root)}")
    println(s"IsBalanced: ${isBalanced0(root)} / ${isBalanced1(root)}")

    println(render0(root))
  }

  def generateTree: BinaryTree[Int] = {
    import BinaryTree.builderSyntax._
    314.branch(
      6.branch(
        271.branch(
          28.leaf,
          0.leaf
        ),
        561.right(
          3.left(
            17.leaf
          )
        )
      ),
      6.branch(
        2.right(
          1.branch(
            401.right(
              641.leaf
            ),
            257.leaf
          )
        ),
        271.right(28.leaf)
      )
    )
  }
}
