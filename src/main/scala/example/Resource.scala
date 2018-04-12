package example

object Resource extends Enumeration {
  type Resource = Value
  val Silver, Gold, Undefined = Value

  def containsResource(line: String): Resource = {
    Resource.values.find(resource => line.contains(resource.toString)).getOrElse(Undefined)
  }
}