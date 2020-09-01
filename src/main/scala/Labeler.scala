/** Class that maps the target output values to their internal label representations.
 *
 * @tparam O the output type
 * @param possibleOutputs the set of all possible output values
 */
class Labeler[O](possibleOutputs: Set[O]) {

  private val mapping: Map[O, Int] = possibleOutputs.zipWithIndex.toMap
  private val inverseMapping = mapping.map(_.swap)

  def label(y: O): Int = {
    mapping(y)
  }

  def labelAll(ys: Seq[O]): Seq[Int] = {
    ys.map(label)
  }

  def unlabel(label: Int): O = {
    inverseMapping(label)
  }

  def unlabelAll(labels: Seq[Int]): Seq[O] = {
    labels.map(unlabel)
  }

}
