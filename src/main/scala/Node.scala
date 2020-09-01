/** A single node of a decision tree. */
abstract class Node {

  def decide[I, O](item: Vector[Float])(implicit dc: DecisionContext[I, O]): (Int, DecisionContext[I, O])

}

/** A single decision node of a decision tree, which decides whether items should go left or right. */
case class DecisionNode(decider: Decider,
                        left: Node,
                        right: Node
                       ) extends Node {

  override def decide[I, O](item: Vector[Float])(implicit dc: DecisionContext[I, O]): (Int, DecisionContext[I, O]) = {
    val (decision, message) = decider.decideWithMessage(item)
    if (decision)
      this.left.decide(item)(dc.decisionAdded(message))
    else
      this.right.decide(item)(dc.decisionAdded(message))
  }

}

/** A single leaf node of a decision tree, which decides the final label based on a majority vote. */
case class LeafNode(majority: Int, confidence: Double) extends Node {

  override def decide[I, O](item: Vector[Float])(implicit dc: DecisionContext[I, O]): (Int, DecisionContext[I, O]) = {
    (this.majority, dc.decisionAdded(message(dc)))
  }

  private def message[I, O](dc: DecisionContext[I, O]) = "majority vote: '%s' (%1.2f%% certain)"
    .format(dc.labeler.unlabel(majority), confidence * 100)

}

/** Object that builds the decision tree. */
object Node {

  val MIN_NUM_ITEMS: Int = 3
  val MIN_INFO_GAIN: Double = 0.001

  def train(items: Seq[Vector[Float]], labels: Seq[Int]): Node = {

    if (items.length < MIN_NUM_ITEMS)
      return createLeafNode(labels)

    if (labels.forall(_ == labels.head))
      return createLeafNode(labels)

    val deciders = this.generatePossibleDeciders(items)

    val decidersAndTheirInfoGains = deciders.map(decider => {
      val ((itemsL, labelsL), (itemsR, labelsR)) = this.splitItemsAndLabels(decider, items, labels)
      val infoGain = Calc.infoGain(Seq(labelsL, labelsR))
      (decider, infoGain)
    })

    val (bestDecider, bestInfoGain) = decidersAndTheirInfoGains.maxBy(_._2)

    if (bestInfoGain < MIN_INFO_GAIN)
      return createLeafNode(labels)

    createDecisionNode(bestDecider, items, labels)
  }

  private def splitItemsAndLabels(decider: Decider,
                                  items: Seq[Vector[Float]],
                                  labels: Seq[Int]) = {
    // Splits items and labels into two partitions
    val split = items.zip(labels).partition({ case (item, _) => decider.decide(item) })
    (split._1.unzip, split._2.unzip)
  }

  private def generatePossibleDeciders(items: Seq[Vector[Float]]): Seq[Decider] = {
    val features = items.transpose
    features.zipWithIndex
      .flatMap(o => o match {
        case (feat: Seq[Int], j: Int) =>
          feat.distinct.map(s => new SingleFeatureDecider(j, s))
      })
  }

  def createDecisionNode(decider: Decider, items: Seq[Vector[Float]], labels: Seq[Int]): DecisionNode = {
    val ((itemsL, labelsL), (itemsR, labelsR)) = splitItemsAndLabels(decider, items, labels)

    val leftNode = Node.train(itemsL, labelsL)
    val rightNode = Node.train(itemsR, labelsR)

    DecisionNode(decider, leftNode, rightNode)
  }

  def createLeafNode(labels: Seq[Int]): LeafNode = {
    val majority = Calc.majority(labels)
    val confidence = Calc.confidence(labels, majority)
    LeafNode(majority, confidence)
  }

}
