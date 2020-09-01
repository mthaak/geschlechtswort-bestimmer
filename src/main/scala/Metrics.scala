/** Various metric calculation methods. */
object Metrics {

  /** Calculates the accuracy for given predicted and true labels. */
  def calcAccuracy(predLabels: Seq[Int],
                   trueLabels: Seq[Int]): Double = {
    val numTotal = predLabels.length

    val numCorrect = predLabels.zip(trueLabels).count({ case (pred, true_) => pred == true_ })

    numCorrect.toDouble / numTotal
  }

  /** Partitions the nouns by whether their article was correctly predicted.
   * Returns two sequences of (true article, nouns, predicted article) */
  def partitionCorrect(nouns: Seq[String],
                       articles: Seq[String],
                       predArticles: Seq[String]): (Seq[(String, String, String)], Seq[(String, String, String)]) = {

    val equals = predArticles.zip(articles).map({ case (pred, true_) => pred.equals(true_) })
    val zipped = articles.lazyZip(nouns).lazyZip(predArticles).toVector

    val correct = zipped.zip(equals).filter(_._2).map(_._1)
    val incorrect = zipped.zip(equals).filter(!_._2).map(_._1)

    (correct, incorrect)
  }

  /** Counts the total number of nodes, including the root and all the children. */
  def countNodes(node: Node): Int = node match {
    case node: DecisionNode => 1 + countNodes(node.left) + countNodes(node.right)
    case _: LeafNode => 1
  }

  def treeDepth(node: Node): Int = node match {
    case node: DecisionNode => 1 + Math.max(treeDepth(node.left), +treeDepth(node.right))
    case _: LeafNode => 1
  }

}
