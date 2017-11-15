package o1.grid

import scala.collection.mutable.Buffer
import scala.reflect.ClassTag

/** The class `Grid` represents rectangular grids that contain elements of a particular 
  * kind. Each element in a grid is located at a unique pair of coordinates.
  *
  * There are different kinds of grids: the type of element that a grid contains is 
  * defined by a type parameter. For instance `Grid[Glass]` is a grid where each pair 
  * of x and y coordinates contains a `Glass` object, and `Grid[Square]` is a grid 
  * containing `Square` objects.
  *
  * X coordinates run from 0 to `width-1`, y coordinates from 0 to `height-1`. 
  * (0, 0) corresponds to the upper left corner of the grid. 
  *
  * This class does not initialize the elements of a newly created grid. Subclasses may 
  * initialize the elements of the grid in whichever way is appropriate for the subclass.
  *
  * @param width   the number of elements in each row of the grid
  * @param height  the number of elements in each column of the grid */
abstract class Grid[ElementType: ClassTag](val width: Int, val height: Int) {

  private val contents = Array.ofDim(width, height) 
  
  
  /** the number of elements in this grid, in total. (Equals `width` times `height`.) */
  val size = width * height
  
  
  /** Returns the element at the given pair of coordinates. (This does the same as `elementAt`.) */
  def apply(location: Coords) = this.elementAt(location)

  
  /** Returns the element at the given pair of coordinates. (This does the same as `apply`.) */
  def elementAt(location: Coords) = this.contents(location.x)(location.y)
  
  
  /** Modifies the grid by replacing the existing element at the given location 
    * with the new, given element.  */
  def update(location: Coords, newElement: ElementType) = {
    this.contents(location.x)(location.y) = newElement
  }

  
  /** Checks whether the grid contains the given x and y coordinates. */
  private def contains(x: Int, y: Int): Boolean = 
    x >= 0 && x < this.width && 
    y >= 0 && y < this.height 
    
  
  /** Determines whether the grid contains the given pair of coordinates. 
    * For instance,a grid with a width and height of 5 will contain (0, 0) 
    * and (4, 4) but not (-1, -1), (4, 5) or (5, 4). */
  def contains(location: Coords): Boolean = this.contains(location.x, location.y) 


  /** Returns a list of all the neighboring elements of the indicated element. Depending on 
    * the value of the second parameter, either only the four neighbors in cardinal compass 
    * directions (north, east, south, west) are considered, or the four diagonals as well. 
    *
    * Note that an element that is at the edge of the grid has fewer neighbors than one 
    * in the middle. For instance, the element at (0, 0) of a 5-by-5 grid has only three 
    * neighbors, diagonals included. 
    *
    * @param location          the coordinates of the element whose neighbors are to be returned
    * @param includeDiagonals  `true` if diagonal neighbors also count (resulting in up to eight neighbors),
    *                          `false` if only cardinal directions count (resulting in up to four) */
  def neighbors(location: Coords, includeDiagonals: Boolean) = {
    val results = Buffer[ElementType]()
    for (direction <- Direction.Clockwise) {
      val cardinal = location.neighbor(direction)
      if (this.contains(cardinal)) {
        results += this(cardinal)
      }
      val ordinal = cardinal.neighbor(direction.clockwise)
      if (includeDiagonals && this.contains(ordinal)) {
        results += this(ordinal)
      }
    }
    results.toVector
  }
  

  /** Returns a collection of all the elements currently in the grid. */
  def allElements: Iterable[ElementType] = 
    for {
      row <- 0 until this.height
      col <- 0 until this.width
      elem <- Option(this(new Coords(col, row)))
    } yield elem
  
  
}


