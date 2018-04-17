package merchant

case class KnowledgeBaseItem(word: String, romanNumber: String)

case class KnowledgeBase(knowledgeBaseItems: Seq[TranslatedKnowledgeBaseItem])

case class RomanKnowledgeBase(romanKnowledgeBase: Seq[RomanKnowledgeBaseItem])

case class TranslatedKnowledgeBaseItem(resource: String, credits: Int)

case class RomanKnowledgeBaseItem(resource: String, credits: String)

object KnowledgeLibrary {
  private val romanDictionary = new RomanDictionary

  val knowledgeBase = KnowledgeBase(List(
    TranslatedKnowledgeBaseItem("glob", romanDictionary.romanSymbols("I")),
    TranslatedKnowledgeBaseItem("prok", romanDictionary.romanSymbols("V")),
    TranslatedKnowledgeBaseItem("pish", romanDictionary.romanSymbols("X")),
    TranslatedKnowledgeBaseItem("tegj", romanDictionary.romanSymbols("L"))))

  val romanKnowledgeBase = RomanKnowledgeBase(List(
    RomanKnowledgeBaseItem("glob", "I"),
    RomanKnowledgeBaseItem("prok", "V"),
    RomanKnowledgeBaseItem("pish", "X"),
    RomanKnowledgeBaseItem("tegj", "L")))
}
