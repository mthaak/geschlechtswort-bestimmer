import scala.io.Source

/** Main app */
object Main extends App {

  val TEST_SIZE = 500

  val (articles, nouns) = getWords("nouns2977.txt")

  // Split test and train set
  val (articlesTest, articlesTrain) = articles.splitAt(TEST_SIZE)
  val (nounsTest, nounsTrain) = nouns.splitAt(TEST_SIZE)

  // Training
  val vectorizer = NounFeaturizer.createVectorizer(nounsTrain)
  val labeler = new Labeler(Set("der", "die", "das"))
  val vectors = vectorizer.vectorizeAll(nounsTrain)
  val labels = labeler.labelAll(articlesTrain)
  val decisionTree = DecisionTreeClassifier.train(vectors, labels, vectorizer, labeler)

  val numNodes = Metrics.countNodes(decisionTree.root)
  val depth = Metrics.treeDepth(decisionTree.root)
  println("Size of decision tree: %d nodes, %d levels deep".format(numNodes, depth))

  // Performance on training set
  val predLabelsTrain = vectors.map(decisionTree.predict)
  val accuracyTrain = Metrics.calcAccuracy(predLabelsTrain, labels)
  println("Accuracy train: %s".format(accuracyTrain))

  // Performance on test set
  val vectorsTest = vectorizer.vectorizeAll(nounsTest)
  val labelsTest = labeler.labelAll(articlesTest)
  val predLabelsTest = vectorsTest.map(decisionTree.predict)
  val accuracyTest = Metrics.calcAccuracy(predLabelsTest, labelsTest)
  println("Accuracy test: %s".format(accuracyTest))
  val predArticles = labeler.unlabelAll(predLabelsTest)
  val (correct, incorrect) = Metrics.partitionCorrect(nounsTest, articlesTest, predArticles)
  println("First 20 correct:")
  correct.slice(0, 20).foreach(println)
  println("First 20 incorrect:")
  incorrect.slice(0, 20).foreach(println)

  // Show decisions for one noun
  val myNoun = "Ursprung"
  val myTrueArticle = "der"
  val (predictedLabel, decisionContext) = decisionTree.predictWithContext(vectorizer.vectorize(myNoun))
  val myPredArticle = labeler.unlabel(predictedLabel)
  println(f"Decisions for noun '$myNoun':")
  decisionContext.decisions.foreach(println)
  println("=> is " ++ (if (myPredArticle == myTrueArticle) "correct" else "incorrect"))

  def getWords(filename: String): (List[String], List[String]) = {
    val lines = Source.fromResource(filename).getLines
    lines.toList.reverse.foldLeft((List.empty[String], List.empty[String]))((acc, x) =>
      acc match {
        case (articles, nouns) => (x.substring(0, 3) :: articles, x.substring(4) :: nouns)
      }
    )
  }

}

