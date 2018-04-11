package example

import org.scalatest.{Matchers, WordSpec}


class MerchantSpec extends WordSpec with Matchers {
  "A Merchant" when {
    "given a knowledge base line 'glob is I'" should {
      "return KnowledgeBaseItem('glob','I')" in {
        val merchant = new Merchant()
        merchant.mapLine("glob is I") shouldBe Some(KnowledgeBaseItem("glob", "I"))
      }
    }

    "given knowledge base lines" should {
      "return a KnowledgeBase" in {
        val merchant = new Merchant()
        merchant.mapLines("glob is I\nprok is V\npish is X\ntegj is L".trim) shouldEqual
          KnowledgeBase(List(KnowledgeBaseItem("glob", "I"),
            KnowledgeBaseItem("prok", "V"),
            KnowledgeBaseItem("pish", "X"),
            KnowledgeBaseItem("tegj", "L")))
      }
    }

    "given a knowledge base complex line: 'glob glob Silver is 34 Credits'" should {
      "return TranslatedKnowledgeBaseItem('Silver', 17)" in {
        val merchant = new Merchant()
        merchant.mapLine("glob glob Silver is 34 Credits".trim) shouldEqual
          Some(TranslatedKnowledgeBaseItem("Silver", 17))
      }
    }
  }
}
