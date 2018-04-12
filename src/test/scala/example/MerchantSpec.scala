package example

import org.scalatest.{Matchers, WordSpec}

class MerchantSpec extends WordSpec with Matchers {
  "A Merchant" when {
    "given a knowledge base line 'glob is I'" should {
      "return TranslatedKnowledgeBaseItem('glob', 1)" in {
        val merchant = new Merchant()
        merchant.mapLine("glob is I") shouldBe Some(TranslatedKnowledgeBaseItem("glob", 1))
      }
    }

    "given knowledge base lines" should {
      "return a KnowledgeBase" in {
        val merchant = new Merchant()
        merchant.mapLines("glob is I\nprok is V\npish is X\ntegj is L".trim) shouldEqual
          KnowledgeBase(List(
            TranslatedKnowledgeBaseItem("glob", 1),
            TranslatedKnowledgeBaseItem("prok", 5),
            TranslatedKnowledgeBaseItem("pish", 10),
            TranslatedKnowledgeBaseItem("tegj", 50)))
      }
    }

    "given a knowledge base complex line: 'glob glob Silver is 34 Credits'" should {
      "return TranslatedKnowledgeBaseItem('Silver', 17)" in {
        val merchant = new Merchant()
        merchant.mapLine("glob glob Silver is 34 Credits".trim) shouldEqual
          Some(TranslatedKnowledgeBaseItem("Silver", 17))
      }
    }

    "given translated symbols '1 1'" should {
      "return 2 " in {
        Merchant.sumTranslatedSymbols("1 1") shouldBe 2
      }
    }
  }
}
