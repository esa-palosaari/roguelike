package s1.world

// XXX consolidate with HasEdges trait
// XXX use elsewhere
import s1.util.ConvenientInt
//import s1.world.Pos

case class Bounds(val left: Int, val top: Int, val width: Int, val height: Int) { // XXX hasposition
  lazy val right  = left + width
  lazy val bottom = top + height
  lazy val pos = Pos(left, top)
  def contains(x: Int, y: Int) = x.isBetween(this.left, this.right) && y.isBetween(this.top, this.bottom)
  def contains(pos: Pos): Boolean = this.contains(pos.xInt, pos.yInt) // XXX floor or round or what?
  override def toString = s"($left,$top):w=$width,h=$height"
}

object Bounds {
  def apply(pos: Pos, width: Int, height: Int): Bounds = Bounds(pos.xInt, pos.yInt, width, height) // XXX floor or round or what?
}
  
