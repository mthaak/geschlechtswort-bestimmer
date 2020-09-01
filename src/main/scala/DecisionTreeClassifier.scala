/** A trained decision tree classifier.
 *
 * @tparam I the input type
 * @tparam O the output type
 * @param root       the root node of the tree
 * @param vectorizer the used Vectorizer
 * @param labeler    the used Labeler
 */
case class DecisionTreeClassifier[I, O](root: Node, vectorizer: Vectorizer[I], labeler: Labeler[O]) {

  def predict(item: Vector[Float]): Int = {
    predictWithContext(item)._1
  }

  def predictWithContext(item: Vector[Float]): (Int, DecisionContext[I, O]) = {
    val decisionContext = new DecisionContext(vectorizer, labeler)
    root.decide(item)(decisionContext)
  }

}

object DecisionTreeClassifier {

  def train[I, O](vectors: Seq[Vector[Float]],
                  labels: Seq[Int],
                  vectorizer: Vectorizer[I],
                  labeler: Labeler[O]): DecisionTreeClassifier[I, O] = {
    val root = Node.train(vectors, labels)
    DecisionTreeClassifier(root, vectorizer, labeler)
  }

}