import FeatureType.FeatureType

/** Class that maps input items to the vectors used internally.
 *
 * @tparam I the input type, the output type is always Vector[Float]
 * @param features a list of (feature name, feature type, feature function applied to input)
 */
class Vectorizer[I](features: Seq[(String, FeatureType, I => Float)]) {

  def getFeatureName(idx: Int): String = {
    features(idx)._1
  }

  def getFeatureType(idx: Int): FeatureType.Value = {
    features(idx)._2
  }

  def vectorize(x: I): Vector[Float] = {
    features.map(_._3).map(_ (x)).toVector
  }

  def vectorizeAll(xs: Seq[I]): Seq[Vector[Float]] = {
    xs.map(vectorize)
  }

}
