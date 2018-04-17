package merchant


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

class Merchant {
  import merchant.Resource._
  import KnowledgeLibrary._

  def findResVal(resource: String, ln: String): TranslatedKnowledgeBaseItem = {
    val romanDictionary = new RomanDictionary
    val translatedSymbols = ln.split(" ").map(word => translateToRomanSymbol(word)).collect {
      case romanLiteral if romanLiteral.isDefined => romanLiteral.get
    }.mkString("")

    val resourceValue = getResourceValue(resource) * romanDictionary.translateRomanNumber(translatedSymbols)
    TranslatedKnowledgeBaseItem(s"$ln is $resourceValue Credits", resourceValue)
  }

  def findResourceValue(line: String): TranslatedKnowledgeBaseItem = {
    val resource = containsResource(line)
    line match {
      case ln if !resource.equals(Undefined) => findResVal(resource.toString, ln)
    }
  }

  def queryLine(query: String): TranslatedKnowledgeBaseItem = {
    query match {
      case in if in.startsWith("how much is") && in.contains(" ?") =>
        getValue(query.replace("how much is ", "").replace(" ?", ""))
      case in if in.startsWith("how many Credits is") && in.contains(" ?") =>
        findResourceValue(in.replace("how many Credits is ", "").replace(" ?", ""))
    }
  }

  private def getValue(query: String) = {
    val romanDictionary = new RomanDictionary()
    val translatedRomanNumbers = query.split(" ").flatMap(translateToRomanSymbol).mkString("")
    TranslatedKnowledgeBaseItem(s"$query is", romanDictionary.translateRomanNumber(translatedRomanNumbers))
  }

  def mapLines(lines: String): KnowledgeBase = {
    KnowledgeBase(lines.split("\n").map(mapLine).toList.filter(_.isDefined).flatten)
  }

  def translateToRomanSymbol(word: String): Option[String] = {
    romanKnowledgeBase.romanKnowledgeBase.collectFirst {
      case romanKnowledgeBaseItem if romanKnowledgeBaseItem.resource == word => romanKnowledgeBaseItem.credits
    }
  }

  def translateSymbol(word: String): Option[Int] = {
    knowledgeBase.knowledgeBaseItems.collectFirst {
      case translatedKnowledgeBaseItem if translatedKnowledgeBaseItem.resource == word => translatedKnowledgeBaseItem.credits
    }
  }

  def getResVal(resource: String, line: String): Option[TranslatedKnowledgeBaseItem] = {
    val romanDictionary = new RomanDictionary
    val words = line.split(" is ")
    line match {
      case _ if line contains " is " =>
        val translatedSymbols = words(0).split(" ").map(word => translateToRomanSymbol(word)).collect {
          case romanLiteral if romanLiteral.isDefined => romanLiteral.get
        }.mkString("")

        Some(TranslatedKnowledgeBaseItem(resource, words(1).toInt / romanDictionary.translateRomanNumber(translatedSymbols)))
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

  def isQueryLine(input: String): Boolean = {
    input match {
      case in if in.startsWith("how much is") && in.contains(" ?") => true
      case in if in.startsWith("how many Credits is") && in.contains(" ?") => true
      case _ => false
    }
  }

  def mapLine(line: String): Option[TranslatedKnowledgeBaseItem] = {
    val romanDictionary = new RomanDictionary
    val words = line.split(" is ")
    val isQueryLn = isQueryLine(line)

    line match {
      case l if isQueryLn => Some(queryLine(l))
      case l => l match {
        case declaration if declaration contains "Credits" => getResorcesValue(declaration.replace(" Credits", ""))
        case constant if constant contains "is" => Some(TranslatedKnowledgeBaseItem(words(0), romanDictionary.romanSymbols(words(1))))
        case _ if !isQueryLn => Some(TranslatedKnowledgeBaseItem("I have no idea what you are talking about", 0))
      }
    }
  }
}

object Merchant {
  def sumTranslatedSymbols(translatedSymbols: String): Int = {
    translatedSymbols.map(numberStr => numberStr.asDigit).sum
  }
}
