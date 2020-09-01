/** Common constants and functions.
 */
object Common {

  val VOWELS: Set[Char] = Set('a', 'e', 'i', 'o', 'u', 'ä', 'ö', 'ü')
  val CONSONANTS: Set[Char] = ('a' to 'z').filter(c => !VOWELS.contains(c)).toSet

  def isVowel(c: Char): Boolean = {
    VOWELS.contains(c.toLower)
  }

  def isConsonant(c: Char): Boolean = {
    CONSONANTS.contains(c.toLower)
  }

  def boolToFloat(bool: Boolean): Float = {
    (if (bool) 1.0 else 0.0).toFloat
  }

  def boolToInt(bool: Boolean): Int = {
    if (bool) 1 else 0
  }

  def intToFloat(int: Int): Float = {
    int.toFloat
  }

}

