import FeatureType.{BOOL, INT}

/** The class that decides whether the item should go left or right.
 */
abstract class Decider {

  def decideWithMessage[I, O](item: Vector[Float])(implicit dc: DecisionContext[I, O]): (Boolean, String) = {
    if (decide(item))
      (true, positiveDecision(dc))
    else
      (false, negativeDecision(dc))
  }

  def decide(item: Vector[Float]): Boolean

  def positiveDecision[I, O](dc: DecisionContext[I, O]): String

  def negativeDecision[I, O](dc: DecisionContext[I, O]): String

}

/** Decider which looks at a single item feature in order to decide whether it should go left or right.
 *
 * @param feature   index of the feature to test
 * @param threshold the deciding threshold for the feature
 */
class SingleFeatureDecider(feature: Int, threshold: Float) extends Decider {

  override def decide(item: Vector[Float]): Boolean = {
    item(feature) >= threshold
  }

  def positiveDecision[I, O](dc: DecisionContext[I, O]): String = {
    val featureName = dc.vectorizer.getFeatureName(feature)
    val featureType = dc.vectorizer.getFeatureType(feature)
    if (featureType == INT)
      f"feature $featureName is equal to or larger than $threshold"
    else if (featureType == BOOL)
      s"feature $featureName is $trueOrFalse"
    else
      throw new IllegalStateException()
  }

  def negativeDecision[I, O](dc: DecisionContext[I, O]): String = {
    val featureName = dc.vectorizer.getFeatureName(feature)
    val featureType = dc.vectorizer.getFeatureType(feature)
    if (featureType == INT)
      f"feature $featureName is smaller than $threshold"
    else if (featureType == BOOL)
      s"feature $featureName is not $trueOrFalse"
    else
      throw new IllegalStateException()
  }

  private val trueOrFalse = if (threshold == 1) "true" else "false"

}
