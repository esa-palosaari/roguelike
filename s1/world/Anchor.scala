
////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it's not necessary    
// that you understand or even look at the code in this file. 
//////////////////////////////////////////////////////////////

package s1.world

//import s1.world.HasAnchor


// XXX siirretään smclään?


/**
 * The class Anchor helps in alignment of pictures in a number of operations.
 * 
 * Look into the companion object for the predefined Anchor points to be used in operations.
 * 
 * The following example from mutable.View shows how anchors are used in the method `onto`
 * to align the two Pics below each other, the stem's top center in the same point as the hat's
 * bottom center.
 * 
 * {{{
 * def makePic = {
 *     val hatWidth = 3 * mushroom.size
 *   
 *     val hat  = circle(hatWidth, Red).cropToSizeOf(Bounds(0, 0, hatWidth, hatWidth/2))
 *     val stem = rectangle(mushroom.size, mushroom.size*2, SandyBrown)
 *     
 *     stem.onto(hat, my=TopCenter, atIts=BottomCenter)
 *   }
 * }}}
 */

abstract class Anchor {
  /** Returns the x-coordinate of the Anchor. */
  def internalXWithin(anchored: HasAnchor): Double 

  /** Returns the y-coordinate of the Anchor. */
  def internalYWithin(anchored: HasAnchor): Double 

  /** Returns the coordinates of the Anchor. */
  def internalPosWithin(anchored: HasAnchor) =  Pos(this.internalXWithin(anchored), this.internalYWithin(anchored))

  /** REturns an absolute anchor in the position of the current anchor. */
  def toAbsoluteWithin(anchored: HasAnchor) = Anchor.Absolute(this.internalPosWithin(anchored)) 
    
}

/**
 * The Anchor object defines a number of usable Anchor points that can be used in the image operations
 * of the Pic-class.
 * 
 * The following example from mutable.View shows how anchors are used in the method `onto`
 * to align the two Pics below each other, the stem's top center in the same point as the hat's
 * bottom center.
 * 
 * {{{
 * def makePic = {
 *     val hatWidth = 3 * mushroom.size
 *   
 *     val hat  = circle(hatWidth, Red).cropToSizeOf(Bounds(0, 0, hatWidth, hatWidth/2))
 *     val stem = rectangle(mushroom.size, mushroom.size*2, SandyBrown)
 *     
 *     stem.onto(hat, my=TopCenter, atIts=BottomCenter)
 *   }
 * }}}
 */

object Anchor {

  private[world] trait Left    extends Anchor { def internalXWithin(anchored: HasAnchor) = 0 }
  private[world] trait HCenter extends Anchor { def internalXWithin(anchored: HasAnchor) = anchored.centerFromTopLeft.x }
  private[world] trait Right   extends Anchor { def internalXWithin(anchored: HasAnchor) = anchored.width }
  private[world] trait Top     extends Anchor { def internalYWithin(anchored: HasAnchor) = 0 }
  private[world] trait VCenter extends Anchor { def internalYWithin(anchored: HasAnchor) = anchored.centerFromTopLeft.y }
  private[world] trait Bottom  extends Anchor { def internalYWithin(anchored: HasAnchor) = anchored.height }
  
  /** Top left corner of a picture.*/
  case object TopLeft      extends Top     with Left
  /** Top center point of a picture.*/
  case object TopCenter    extends Top     with HCenter  
  /** Top right corner of a picture.*/
  case object TopRight     extends Top     with Right  
  /** Center left point of a picture.*/
  case object CenterLeft   extends VCenter with Left
  /** Center of a picture.*/
  case object Center       extends VCenter with HCenter
  /** Center right point of a picture.*/
  case object CenterRight  extends VCenter with Right
  /** Bottom left corner of a picture.*/
  case object BottomLeft   extends Bottom  with Left
  /** Bottom right corner of a picture.*/
  case object BottomRight  extends Bottom  with Right
  /** Bottom center of a picture.*/
  case object BottomCenter extends Bottom  with HCenter

  /** Anchor placed in coordinates relative to the top left corner.
   *  @param deltaFromTopLeft: coordinates of the anchor point relative to the top left corner
   */
  case class Absolute(private val deltaFromTopLeft: Pos) extends Anchor {
    def internalXWithin(anchored: HasAnchor) = this.deltaFromTopLeft.x
    def internalYWithin(anchored: HasAnchor) = this.deltaFromTopLeft.y
  }
  
}

