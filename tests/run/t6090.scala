class X { def ==(other: X) = true }
class V(val x: X) extends AnyVal
object Test {
  def main(args: Array[String]) =
    assert((new V(new X) == new V(new X)))
}
