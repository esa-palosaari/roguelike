
package o1.grid


/** An object of type `Coords` represents a pair of integer coordinates on two axes named x and y. 
  *
  * In this coordinate system `x` increases "eastwards" and `y` increases "southwards" (like in 
  * many computer-based systems but unlike in many mathematics classrooms).
  *
  * A `Coords` object is immutable.
  *
  * @param x  an x coordinate
  * @param y  a y coordinate */
class Coords(val x: Int, val y: Int) {
  
  
  /** Returns another pair of coordinates that is in the given direction from this pair
    * and at a given distance.
    *
    * For instance, say this pair has an `x` of 10 and a `y` of 20. If `direction` is 
    * `Direction.North` and `distance` is 3, then the result has an `x` of 10 and a `y` of 17.
    *
    * @see [[Direction.xStep]], [[Direction.yStep]] */
  def relative(direction: Direction, distance: Int) = 
    new Coords(this.x + direction.xStep * distance,
               this.y + direction.yStep * distance)
  
  
  /** Returns a pair of coordinates that "neighbors" this one in the given direction. 
    * For instance, if this pair has an `x` of 10 and a `y` of 20, and the `direction` 
    * parameter is given the value `Direction.South`, then the result has an `x` of 10 
    * and a `y` of 21. Calling this method is essentially the same as calling `relative` 
    * with a `distance` of one. */
  def neighbor(direction: Direction) = this.relative(direction, 1)

  
  /** Determines whether this pair of coordinates equals the given one. This is the case 
    * if both have identical x and y coordinates. */
  def ==(another: Coords) =
    this.x == another.x && this.y == another.y

   
  /** Returns a textual description of this pair of coordinates. The description is of the form `"(x,y)"`. */
  override def toString = "(" + this.x + "," + this.y + ")"
    
  
}