package merchant

object Resource extends Enumeration {
  type Resource = Value
  val Silver, Gold, Iron, Undefined = Value
  
  private val allResources = Seq(Silver, Gold, Iron)
  private val resourceValue = Map(Silver -> 17, Gold -> 14450, Iron -> 195)

  def containsResource(line: String): Resource = {
    Resource.values.find(resource => line.contains(resource.toString)).getOrElse(Undefined)
  }

  def getResourceValue(resource: String): Int = {
    val maybeResource = allResources.find(res => res.toString == resource)
    maybeResource.flatMap(resource => resourceValue.get(resource)).getOrElse(0)
  }
}