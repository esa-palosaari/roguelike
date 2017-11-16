
////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it's not necessary    
// that you understand or even look at the code in this file. 
//////////////////////////////////////////////////////////////

package s1.world


import scala.math._


case class Direction private(val dx: Double, val dy: Double) { // dx*dx+dy*dy ~= 1, or both zero
  def switchY  = Direction( dx, -dy)
  def switchX  = Direction(-dx,  dy)
  def opposite = Direction(-dx, -dy)
  def isRightward = this.dx >= 0
  def isLeftward  = this.dx <= 0
  def isDownward  = this.dy >= 0
  def isUpward    = this.dy <= 0
  def +(another: Direction) = Direction.fromDegrees(this.toDegrees + another.toDegrees)
  def sharesQuadrant(another: Direction) = {
    def sameHalf(dx1: Double, dx2: Double) = dx1.signum == dx2.signum || dx1.signum == 0 || dx2.signum == 0
    sameHalf(this.dx, another.dx) && sameHalf(this.dy, another.dy)
  }
  def roughly = f"dx$dx%1.2f,dy$dy%1.2f"
  def toRadians = {
    val atan = atan2(-this.dy, this.dx) 
    if (atan < 0) atan + 2 * Pi else atan
  }
  def toDegrees = this.toRadians.toDegrees 
}

object Direction {
  val Up    = Direction(0, -1) 
  val Down  = Direction(0,  1) 
  val Left  = Direction(-1, 0) 
  val Right = Direction(1,  0) 
  val NoDirection = Direction(0, 0)
  def fromRadians(angle: Double) = Direction(cos(angle), -sin(angle)) 
  def fromDegrees(angle: Double) = fromRadians(angle.toRadians)
  def fromDeltas(dx: Double, dy: Double) = fromRadians(atan2(-dy, dx))
  def random() = {
    Direction.fromDegrees(scala.util.Random.nextInt(360))
  }

  // XXX riippuvuus Swingiin ikävä. siirretään o1.gui-pakkaukseen?
  private type Key = scala.swing.event.Key.Value
  private val  Key = scala.swing.event.Key 
  private val ArrowToDir = Map(Key.Up -> Up, Key.Left -> Left, Key.Down-> Down, Key.Right-> Right)
  private val WASDToDir  = Map(Key.W  -> Up, Key.A    -> Left, Key.S   -> Down, Key.D    -> Right)
  private val KeyToDir   = ArrowToDir ++ WASDToDir
  def fromArrowKey(key: Key) = ArrowToDir.get(key) 
  def fromWASD(key: Key)     = WASDToDir.get(key) 
  def fromKey(key: Key)      = KeyToDir.get(key)
}

 