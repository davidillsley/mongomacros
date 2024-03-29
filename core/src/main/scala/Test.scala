import org.i5y.mongomacros._

case class AA(part1: String, part2: Int, part3: B)
case class B(part4: String, part5: Option[String])

object Test extends App {
  
  val q = Macros.createHelper[AA]
  
  // Below are a bunch of valid queries.
  q("part2").eql(9)
  q("part3.part4").eql("")
  q("part3").eql(B("", None))
  q("part3").eql(B("", Some("")))
  q("part3.part5").eql(Some(""))
  // and now some invalid ones which won't compile...
  //q("part2").eql("9")
  //q("part22").eql(9)
}