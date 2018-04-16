package example

import org.scalatest.{Matchers, WordSpec}

class RomanDictionarySpec extends WordSpec with Matchers {
  "A Roman dictionary" when {
      "given roman number: 'MCMXLIV'" should {
        "return 1944" in {
          val romanDictionary = new RomanDictionary
          romanDictionary.translateRomanNumber("MCMXLIV") shouldBe 1944
        }
      }
    }
}
