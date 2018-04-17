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

    "given a knowledge base complex line: 'glob prok Gold is 57800 Credits'" should {
      "return TranslatedKnowledgeBaseItem('Gold', 14450)" in {
        val merchant = new Merchant()
        merchant.mapLine("glob prok Gold is 57800 Credits".trim) shouldEqual
          Some(TranslatedKnowledgeBaseItem("Gold", 14450))
      }
    }

    "given a knowledge base complex line: 'pish pish Iron is 3910 Credits'" should {
      "return TranslatedKnowledgeBaseItem('Iron', 195)" in {
        val merchant = new Merchant()
        merchant.mapLine("pish pish Iron is 3910 Credits".trim) shouldEqual
          Some(TranslatedKnowledgeBaseItem("Iron", 195))
      }
    }

    "given translated symbols '11'" should {
      "return 2 " in {
        Merchant.sumTranslatedSymbols("11") shouldBe 2
      }
    }

    "given a query line: 'how much is pish tegj glob glob ?'" should {
      "return 'pish tegj glob glob is 42'" in {
        val merchant = new Merchant()
        merchant.mapLine("how much is pish tegj glob glob ?".trim) shouldEqual
          Some(TranslatedKnowledgeBaseItem("pish tegj glob glob is", 42))
      }
    }

    "given a query line: 'how many Credits is glob prok Silver ?'" should {
      "return 'glob prok Silver is 68 Credits'" in {
        val merchant = new Merchant()
        merchant.mapLine("how many Credits is glob prok Silver ?".trim) shouldEqual
          Some(TranslatedKnowledgeBaseItem("glob prok Silver is 68 Credits", 68))
      }
    }
  }
}