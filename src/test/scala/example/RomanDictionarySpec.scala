package example

import org.scalatest.WordSpec

class RomanDictionarySpec extends WordSpec {
    "A Roman dictionary" when {
        "given a Roman number 'I I'" should {
          "return 2" in {
            val romanDictionary = new RomanDictionary
            romanDictionary.sumRomanNumbers("II")
          }
        }
      }
}
