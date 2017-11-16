
////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it's not necessary    
// that you understand or even look at the code in this file. 
//////////////////////////////////////////////////////////////

package s1.world


import java.awt.Point
import s1.util.ConvenientDouble
import scala.math.hypot
//import s1.world.Direction

case class Pos(val x: Double, val y: Double) { // XXX pos vai position; konsistenssi
  /** Returns the x-coordinate rounded to the closest integer */
  def xInt = this.x.closestInt 

  /** Returns the Y-coordinate rounded to the closest integer */
  def yInt = this.y.closestInt

  /** Returns a copy of this object with the y-value set to no lower than `yMax`.
   * @param yMax limit for the y-coordinate  
   */ 
  def noLowerThan(yMax: Double) = Pos(this.x, this.y atMost yMax)

  /** Returns a copy of this object with the y-value set to no higher than `yMin`.
   *  @param yMin limit for the y-coordinate
   */ 
  def noHigherThan(yMin: Double) = Pos(this.x, this.y atLeast yMin)
  
  /** Returns a copy of this object with the x-value set to no more left than than `xMin`.
   *  @param xMin limit for the x-coordinate
   */   
  def noFurtherLeftThan(xMin: Double) = Pos(this.x atLeast xMin, this.y)

  /** Returns a copy of this object with the x-value set to no more right than than `xMax`.
   *  @param xMax limit for the x-coordinate
   */   
  def noFurtherRightThan(xMax: Double) = Pos(this.x atMost xMax, this.y)
  
  /** Returns a copy of this object with the x-value set to the original if it is between the two limits or to the closest limit.
   *  @param min min-limit for the x-coordinate
   *  @param max max-limit for the x-coordinate
   */     
  def clampX(min: Double, max: Double) = Pos(this.x atLeast min atMost max, this.y)

  /** Returns a copy of this object with the y-value set to the original if it is between the two limits or to the closest limit.
   *  @param min min-limit for the y-coordinate
   *  @param max max-limit for the y-coordinate
   */     
  def clampY(min: Double, max: Double) = Pos(this.x, this.y atLeast min atMost max)
  
  /** Returns a copy of this object with the x and y-values set to their original values
   *  if they are between the two limits or to the closest limit.
   *  @param xMin min-limit for the x-coordinate
   *  @param xMax max-limit for the x-coordinate
   *  @param yMin min-limit for the y-coordinate
   *  @param yMax max-limit for the y-coordinate
   */     

  def clamp(xMin: Double, xMax: Double, yMin: Double, yMax: Double) = Pos(this.x atLeast xMin atMost xMax, this.y atLeast yMin atMost yMax)

  /** Returns a copy of this object with the x and y-values offset from their original values
   *  @param dx x-offset
   *  @param dy y-offset
   */
  
  def offset(dx: Double, dy: Double) = Pos(this.x + dx, this.y + dy) 

  /** Returns a copy of this object with the x and y-values offset from their original values
   *  @param pos a Pos object with the offsets in its x and y coordinates 
   */
  def offset(pos: Pos): Pos = this.offset(pos.x, pos.y)
  
  /** Returns a copy of this object with the coordinates `(-x, -y)`
   */
  def unary_- = Pos(-this.x, -this.y)
  
  /**
   * Calculates the difference between x coordinates between this and the other Pos object `another.x - this.x`. 
   * @param another the Pos object being compared to  
   */
  def xDiff(another: Pos) = another.x - this.x 

  /**
   * Calculates the difference between y coordinates between this and the other Pos object `another.y - this.y`. 
   * @param another the Pos object being compared to  
   */
  def yDiff(another: Pos) = another.y - this.y
  
  /** Returns a copy of this object with the x and y-values offset from their original values
   *  @param pos a Pos object with the offsets in its x and y coordinates 
   */  
  def +(another: Pos) = this.offset(another) 

  /** Returns a copy of this object with the x and y-values negatively offset from their original values
   *  @param pos a Pos object with the offsets in its x and y coordinates 
   */
  def -(another: Pos) = this + -another
  
  /**
   * Returns a copy of this object with the x coordinate set to `newX`.
   * @param newX x-coordinate for the new object
   */
  def setX(newX: Double) = this.copy(x = newX)

  /**
   * Returns a copy of this object with the y coordinate set to `newY`.
   * @param newY y-coordinate for the new object
   */
  def setY(newY: Double) = this.copy(y = newY)

  /** Returns a copy of this object with the x value offset by the parameter `dx`
   *  @param dx x-offset
   */
  def addX(dx: Double) = this.setX(this.x + dx) 

  /** Returns a copy of this object with the y value offset by the parameter `dy`
   *  @param dy y-offset
   */
  def addY(dy: Double) = this.setY(this.y + dy)
  
  /** Returns a copy of this object with the x value reduced by the parameter `dx`
   *  @param dx x-offset
   */  
  def subtractX(dx: Double) = this.setX(this.x - dx)
  
  /** Returns a copy of this object with the y value reduced by the parameter `dy`
   *  @param dy y-offset
   */
  def subtractY(dy: Double) = this.setY(this.y - dy)
  
  /** Returns a new Pos object with the coordinates transformed by the given function.
   *  @param computeNew function used to transform the coordinates
   */
  def withXY(computeNew: (Double, Double) => (Double, Double)) = computeNew(this.x, this.y) match { case (newX, newY) => Pos(newX, newY) }

  /** Returns a new Pos object with the x-coordinate transformed by the given function.
   *  @param computeNew function used to transform the x-coordinate
   */
  def withX(computeNew: Double => Double) = this.setX(computeNew(this.x))

  /** Returns a new Pos object with the y-coordinate transformed by the given function.
   *  @param computeNew function used to transform the y-coordinate
   */
  def withY(computeNew: Double => Double) = this.setY(computeNew(this.y))

  /**
   * Returns the a [[s1.world.Direction]] object describing the direction from this object to the one in the parameter.
   * @param destination the object used as the destination 
   */
  def directionOf(destination: Pos) = Direction.fromDeltas(this.xDiff(destination), this.yDiff(destination))
  
  /**
   * Returns the difference between two Pos objects as a new Pos object `(destination - this)`.
   * @param destination the other Pos object
   */
  def vectorTo(destination: Pos) = destination - this
  
  /**
   * Returns the distance between this object and another Pos-object as a Double. 
   * @param another the other Pos object 
   */
  def distance(another: Pos) = hypot(this.xDiff(another), this.yDiff(another))

  /**
   * Returns a string representation of this Pos with a maximum of two digits after the decimal point.
   */
  def roughly = f"$x%1.2f,$y%1.2f" 
}

object Pos {
  /**
   * Creates a Pos object from a `java.awt.Point`.
   * @param point a coordinate in the awt format
   */
  def apply(point: Point) = new Pos(point.getX, point.getY)
}


