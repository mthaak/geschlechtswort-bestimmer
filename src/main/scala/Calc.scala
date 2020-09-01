/** Miscellaneous calculation methods. */
object Calc {

  /** Determines the label with the highest mode. */
  def majority(labels: Seq[Int]): Int = labels.groupBy(identity).toSeq.sortBy(x => x._2.length).reverse.head._1

  /** Calculates the confidence (i.e. probability) of the majority label. */
  def confidence(labels: Seq[Int], majority: Int): Double = labels.count(_ == majority) / labels.length

  /** Calculates the Gini impurity for splitting labels in groups. */
  def avgGini(groups: Seq[Seq[Any]]): Double = {
    weightedAverage(groups, entropy)
  }

  /** Calculates the Gini impurity for a sequence of labels. */
  def gini(labels: Seq[Any]): Double = {
    // Gini impurity
    val n = labels.length
    1 - labels.groupBy(identity).values.map(grp => (grp.length / n) ^ 2).sum
  }

  /** Calculates the information gain for splitting labels in groups. */
  def infoGain(groups: Seq[Seq[Any]]): Double = {
    val entropyParent = entropy(groups.flatten)
    val entropyChildren = weightedAverage(groups, entropy)
    entropyParent - entropyChildren
  }

  /** Applies a function to the groups and returns the weighted average. */
  def weightedAverage[A](groups: Seq[Seq[A]], fn: Seq[A] => Double): Double = {
    groups.map(grp => grp.length * fn(grp)).sum / groups.flatten.length
  }

  /** Calculates the entropy for a sequence of labels. */
  def entropy(labels: Seq[Any]): Double = {
    val n = labels.length
    -labels.groupBy(identity).values.map(grp => grp.length.toFloat / n * log2(grp.length.toFloat / n)).sum
  }

  /** Calculates the base 2 logarithm. */
  def log2(float: Double): Double = Math.log(float) / Math.log(2)

}