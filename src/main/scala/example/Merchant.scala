package example

import scala.util.Try

/*
https://github.com/chalkmaster/MerchantsGuideToTheGalaxy

A merchant buys and sells items in the galaxy. Buying and selling over the galaxy requires you to convert numbers and units.
The numbers used for intergalactic transactions follows similar convention to the roman numerals.
Roman numerals are based on seven symbols:

I 1
V 5
X 10
L 50
C 100
D 500
m 1000

Numbers are formed by combining symbols together and adding the values. For example, MMVI is 1000 + 1000 + 5 + 1 = 2006.
Generally, symbols are placed in order of value, starting with the largest values.
When smaller values precede larger values, the smaller values are subtracted from the larger values, and the result is added to the total.
For example MCMXLIV = 1000 + (1000 − 100) + (50 − 10) + (5 − 1) = 1944.
The symbols "I", "X", "C", and "M" can be repeated three times in succession, but no more.
(They may appear four times if the third and fourth are separated by a smaller value, such as XXXIX.) "D", "L", and "V" can never be repeated.
"I" can be subtracted from "V" and "X" only. "X" can be subtracted from "L" and "C" only.
"C" can be subtracted from "D" and "M" only. "V", "L", and "D" can never be subtracted.
Only one small-value symbol may be subtracted from any large-value symbol. A number written in [16]Arabic numerals can be broken into digits.
For example, 1903 is composed of 1, 9, 0, and 3. To write the Roman numeral, each of the non-zero digits should be treated separately.
In the above example, 1,000 = M, 900 = CM, and 3 = III. Therefore, 1903 = MCMIII.
Input to your program consists of lines of text detailing your notes on the conversion between intergalactic units and roman numerals.
You are expected to handle invalid queries appropriately.

Test input
glob is I
prok is V
pish is X
tegj is L
glob glob Silver is 34 Credits
glob prok Gold is 57800 Credits
pish pish Iron is 3910 Credits
how much is pish tegj glob glob ?
how many Credits is glob prok Silver ?
how many Credits is glob prok Gold ?
how many Credits is glob prok Iron ?
how much wood could a woodchuck chuck if a woodchuck could chuck wood ?

Test Output
pish tegj glob glob is 42
glob prok Silver is 68 Credits
glob prok Gold is 57800 Credits
glob prok Iron is 782 Credits
I have no idea what you are talking about
 */

case class KnowledgeBaseItem(word: String, romanNumber: String)

case class KnowledgeBase(knowledgeBaseItems: Seq[TranslatedKnowledgeBaseItem])

case class TranslatedKnowledgeBaseItem(resource: String, credits: Int)

class Merchant {

  import Merchant._
  import Resource._

  def mapLines(lines: String): KnowledgeBase = {
    KnowledgeBase(lines.split("\n").map(mapLine).toList.filter(_.isDefined).flatten)
  }

  def translateSymbol(word: String): Option[Int] = {
    val romanDictionary = new RomanDictionary
    val knowledgeBase = KnowledgeBase(List(TranslatedKnowledgeBaseItem("glob", romanDictionary.romanSymbols("I")),
      TranslatedKnowledgeBaseItem("prok", romanDictionary.romanSymbols("V")),
      TranslatedKnowledgeBaseItem("pish", romanDictionary.romanSymbols("X")),
      TranslatedKnowledgeBaseItem("tegj", romanDictionary.romanSymbols("L"))))

    knowledgeBase.knowledgeBaseItems.collectFirst {
      case translatedKnowledgeBaseItem if translatedKnowledgeBaseItem.resource == word => translatedKnowledgeBaseItem.credits
    }
  }

  def getResVal(resource: String, line: String): Option[TranslatedKnowledgeBaseItem] = {
    // glob glob is 34
    val romanDictionary = new RomanDictionary
    val words = line.split(" is ")
    line match {
      case _ if line contains " is " =>
        val translatedSymbols = words(0).split(" ").map(word => translateSymbol(word)).collect {
          case romanLiteral if romanLiteral.isDefined => romanLiteral.get
        }.mkString(" ")

        Some(TranslatedKnowledgeBaseItem(resource, words(1).toInt / sumTranslatedSymbols(translatedSymbols)))
    }
  }

  def getResorcesValue(line: String): Option[TranslatedKnowledgeBaseItem] = {
    val resource = containsResource(line)
    line match {
      case ln if !resource.equals(Undefined) => getResVal(resource.toString, ln.replace(s" $resource", ""))
    }
  }

  def mapLineTranslated(line: String): Option[TranslatedKnowledgeBaseItem] = {
    mapLine(line)
  }

  def mapLine(line: String): Option[TranslatedKnowledgeBaseItem] = {
    val romanDictionary = new RomanDictionary
    val words = line.split(" is ")
    line match {
      case declaration if declaration contains "Credits" => getResorcesValue(declaration.replace(" Credits", ""))
      case constant if constant contains "is" => Some(TranslatedKnowledgeBaseItem(words(0), romanDictionary.romanSymbols(words(1))))
      case "" => None
    }
  }
}

object Merchant {
  def sumTranslatedSymbols(translatedSymbols: String): Int = {
    translatedSymbols.split(" ").map(numberStr => Try(numberStr.toInt).getOrElse(0)).sum
  }
}
