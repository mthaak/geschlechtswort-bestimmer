import Common.{boolToFloat, intToFloat, isConsonant, isVowel}
import FeatureType.{BOOL, FeatureType, INT}

/** Object for calculating features for the provided nouns. */
object NounFeaturizer {

  val MIN_ENDING_COUNT = 5

  def convert: String => String = (value: String) => "converted"

  def createVectorizer(nouns: Seq[String]): Vectorizer[String] = {
    val mostCommonEndings = calcTopEndings(nouns)

    val mostCommonEndingsFeatures: Seq[(String, FeatureType, String => Float)] =
      mostCommonEndings.map(ending => (s"ends_with_$ending", BOOL, str => boolToFloat(str.endsWith(ending))))

    val allLetterStartFeatures: Seq[(String, FeatureType, String => Float)] =
      ('A' to 'Z').map(letter => (s"starts_with_$letter", BOOL, str => boolToFloat(str.startsWith(letter.toString))))

    val features: Seq[(String, FeatureType, String => Float)] = List(
      ("number_of_letters", INT, intToFloat _ compose numberOfLetters),
      ("number_of_syllables", INT, intToFloat _ compose numberOfSyllables),
      ("starts_with_vowel", BOOL, boolToFloat _ compose startsWithVowel),
      ("ends_with_vowel", BOOL, boolToFloat _ compose endsWithVowel)
    ) ++ mostCommonEndingsFeatures ++ allLetterStartFeatures

    new Vectorizer(features)
  }

  private def calcTopEndings(nouns: Seq[String]) = {
    calcMostCommonEndings(nouns).takeWhile(_._2 >= MIN_ENDING_COUNT).map(_._1).toList
  }

  private def calcMostCommonEndings(nouns: Seq[String]): Iterator[(String, Int)] = {
    def ending(word: String): String = {
      val lastVowelIdx = word.lastIndexWhere(isVowel)
      val lastConsonantBeforeVowelIdx = word.slice(0, lastVowelIdx).lastIndexWhere(isConsonant)
      if (lastConsonantBeforeVowelIdx >= 0)
        word.substring(lastConsonantBeforeVowelIdx + 1)
      else
        word
    }

    nouns.groupMapReduce(ending)(_ => 1)(_ + _)
      .toList
      .sortBy(x => x._2)
      .reverseIterator
  }

  // HERE START THE FEATURE FUNCTIONS

  def startsWithVowel: String => Boolean = noun => {
    isVowel(noun.head)
  }

  def endsWithVowel: String => Boolean = (noun: String) =>
    isVowel(noun.last)

  def numberOfLetters: String => Int = (noun: String) => {
    noun.length
  }

  def numberOfSyllables: String => Int = (noun: String) => {
    noun.toSeq.sliding(2).count(s => isConsonant(s.head) && isVowel(s.last))
      .+(if (startsWithVowel(noun)) 1 else 0) // in case word starts with a vowel
  }

}
