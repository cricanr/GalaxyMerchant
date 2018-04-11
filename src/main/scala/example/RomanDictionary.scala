package example

class RomanDictionary {
  val romanSymbols = Map("I" -> 1,
  "V" -> 5,
  "X" -> 10,
  "L" -> 50,
  "C" -> 100,
  "D" -> 500,
  "M" -> 1000)

  def sumRomanNumbers(romanNumber: String): Int = {
    romanNumber.split(" ").map(number => romanSymbols.getOrElse(number.toUpperCase(), 0)).sum
  }

}
