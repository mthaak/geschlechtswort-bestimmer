import scala.collection.mutable.ListBuffer

/** The decision context that is passed around when a DecisionTreeClassifier is predicting for a vector.
 *
 * @tparam I the input type
 * @tparam O the output type
 * @param vectorizer the used Vectorizer
 * @param labeler    the used Labeler
 */
case class DecisionContext[I, O](vectorizer: Vectorizer[I], labeler: Labeler[O], decisions: ListBuffer[String]) {

  def this(vectorizer: Vectorizer[I], labeler: Labeler[O]) = this(vectorizer, labeler, new ListBuffer[String])

  def decisionAdded(message: String): DecisionContext[I, O] = {
    decisions.addOne(message)
    this
  }

}
