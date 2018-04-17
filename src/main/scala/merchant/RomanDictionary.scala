package merchant

class RomanDictionary {
  def translateRomanNumber(romanNumber: String): Int = {
    if (romanNumber.length == 1) romanSymbols(romanNumber) else {
    for (i <- 0 until romanNumber.length-1) yield {
      (romanNumber(i), romanNumber(i + 1)) match {
        case (first, second) if romanSymbols(first.toString) > romanSymbols(second.toString) =>
          romanSymbols(first.toString)
        case (first, second) if romanSymbols(first.toString) < romanSymbols(second.toString) && second != romanNumber(romanNumber.length-1) =>
          -romanSymbols(first.toString)
        case (first, second) if romanSymbols(first.toString) < romanSymbols(second.toString) && second == romanNumber(romanNumber.length-1) =>
          -romanSymbols(first.toString) + romanSymbols(second.toString)
        case (first, second) => romanSymbols(first.toString) + romanSymbols(second.toString)
      }}}.sum
  }

  val romanSymbols = Map(
    "I" -> 1,
    "V" -> 5,
    "X" -> 10,
    "L" -> 50,
    "C" -> 100,
    "D" -> 500,
    "M" -> 1000)
}