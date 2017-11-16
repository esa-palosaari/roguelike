
////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it's not necessary    
// that you understand or even look at the code in this file. 
//////////////////////////////////////////////////////////////

package s1.world


import s1.util.ConvenientDouble
import scala.math._
//import s1.world.Direction
//import s1.world.Pos

// XXX jos tämä integroidaan Posin ja Directionin mukaan SMCL:ään, niin noiden luokkien rajapinnoissa voi käyttää tätä 

case class Velocity(val direction: Direction, val speed: Double) {
  lazy val dx = this.direction.dx * speed
  lazy val dy = this.direction.dy * speed
  def switchY = this.copy(direction = this.direction.switchY)
  def switchX = this.copy(direction = this.direction.switchX)
  def noFasterThan(maxSpeed: Double) = this.copy( speed = this.speed atMost maxSpeed )
  def unary_- = this.copy( direction = this.direction.opposite )
  def +(another: Velocity)  = Velocity(this.dx + another.dx, this.dy + another.dy)
  def -(another: Velocity)  = this + -another
  def *(multiplier: Double) = this.copy(speed = this.speed * multiplier)
  def /(divisor: Double)    = this * (1 / divisor)  
  def roughly = f"$speed%1.2f towards ${direction.roughly}"
  def toPos = Pos(this.dx, this.dy)
  def moveFrom(position: Pos) = position + this.toPos // XXX voisi olla pikemminkin/lisäksi Pos-luokassa
  def changeDirection(newDirection: Direction) = this.copy(direction = newDirection) 
}

object Velocity {
  val Still = Velocity(Direction.NoDirection, 0)
  
  def apply(vector: Pos): Velocity            = Velocity(vector.x, vector.y)
  def apply(dx: Double, dy: Double): Velocity = Velocity(Direction.fromDeltas(dx, dy), hypot(dx, dy))
  def between(from: Pos, to: Pos): Velocity   = Velocity(to - from)

  def sum(velocities: Seq[Velocity])     = velocities.reduceLeftOption( _ + _ ) getOrElse Velocity.Still
  def average(velocities: Seq[Velocity]) = if (velocities.nonEmpty) sum(velocities) / velocities.size else Velocity.Still
}
