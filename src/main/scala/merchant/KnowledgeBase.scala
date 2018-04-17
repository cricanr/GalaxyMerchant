package merchant

import merchant.RomanDictionary._

case class KnowledgeBaseItem(word: String, romanNumber: String)

case class KnowledgeBase(knowledgeBaseItems: Seq[TranslatedKnowledgeBaseItem])

case class RomanKnowledgeBase(romanKnowledgeBase: Seq[RomanKnowledgeBaseItem])

case class TranslatedKnowledgeBaseItem(resource: String, credits: Int)

case class RomanKnowledgeBaseItem(resource: String, credits: String)

object KnowledgeLibrary {
  val knowledgeBase = KnowledgeBase(List(
    TranslatedKnowledgeBaseItem("glob", romanSymbols("I")),
    TranslatedKnowledgeBaseItem("prok", romanSymbols("V")),
    TranslatedKnowledgeBaseItem("pish", romanSymbols("X")),
    TranslatedKnowledgeBaseItem("tegj", romanSymbols("L"))))

  val romanKnowledgeBase = RomanKnowledgeBase(List(
    RomanKnowledgeBaseItem("glob", "I"),
    RomanKnowledgeBaseItem("prok", "V"),
    RomanKnowledgeBaseItem("pish", "X"),
    RomanKnowledgeBaseItem("tegj", "L")))
}
