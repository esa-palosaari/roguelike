
////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it's not necessary    
// that you understand or even look at the code in this file. 
//////////////////////////////////////////////////////////////

package s1.world

import s1.util.ConvenientDouble
/*import s1.world.Anchor
import s1.world.Direction
import s1.world.Pos
import s1.world.Velocity
import s1.world.Direction
import s1.world.Velocity
*/

trait HasSize { 
  def width: Int
  def height: Int
  def centerFromTopLeft = Pos(this.width / 2, this.height / 2) 
}

trait HasAnchor extends HasSize {
  def anchor: Anchor
  def internalAnchorPos = this.anchor.internalPosWithin(this)
  def internalAnchorX   = this.anchor.internalXWithin(this)
  def internalAnchorY   = this.anchor.internalYWithin(this)
}

trait HasPosition { 
  def position: Pos
}


trait HasVelocity extends HasPosition {
  def velocity: Velocity
  def nextPos      = this.velocity.moveFrom(this.position)
  def isRightbound = this.velocity.direction.isRightward
  def isLeftbound  = this.velocity.direction.isLeftward
  def isUpbound    = this.velocity.direction.isUpward
  def isDownbound  = this.velocity.direction.isDownward
  def isBoundFor(direction: Direction) = this.velocity.direction.sharesQuadrant(direction)
}


trait HasEdges extends HasPosition with HasAnchor {
  
  def anchor: Anchor = Anchor.Center
  
  def positionXInside(container: HasEdges, desiredPosition: Pos = this.position) =
    desiredPosition.clampX(container.left + this.internalAnchorX, container.right + this.internalAnchorX - this.width)  

  def positionYInside(container: HasEdges, desiredPosition: Pos = this.position) =
    desiredPosition.clampY(container.top + this.internalAnchorY, container.bottom + this.internalAnchorY - this.height)  
    
  def positionInside(container: HasEdges, desiredPosition: Pos = this.position) = {
    val anchorPos = this.internalAnchorPos
    desiredPosition.clamp(xMin = container.left + anchorPos.x, xMax = container.right  + anchorPos.x - this.width,  
                          yMin = container.top  + anchorPos.y, yMax = container.bottom + anchorPos.y - this.height)  
  }

  def left   = this.position.x - this.internalAnchorX
  def top    = this.position.y - this.internalAnchorY
  def right  = this.left + this.width
  def bottom = this.top + this.height

  def topLeft     = Pos(this.left, this.top)
  def bottomLeft  = Pos(this.left, this.bottom)
  def topRight    = Pos(this.right, this.top)
  def bottomRight = Pos(this.right, this.bottom)
  
  def center = this.topLeft + this.centerFromTopLeft
  
  def containsBetweenEdges(candidate: Pos) = candidate.x.isBetween(this.left, this.right) &&
                                             candidate.y.isBetween(this.top,  this.bottom)
}


