package os1.grid

class Box(val topLeft: Coords, val bottomRight: Coords) {
  
  def size = (bottomRight.x - topLeft.x + 1) * (bottomRight.y - topLeft.y + 1)
  
  def distance(other: Box) = {
    (if(this.topLeft.x > other.bottomRight.x) this.topLeft.x - other.bottomRight.x
    else if(other.topLeft.x > this.bottomRight.x) other.topLeft.x - this.bottomRight.x
    else 0) +
    (if(this.topLeft.y > other.bottomRight.y) this.topLeft.y - other.bottomRight.y
    else if(other.topLeft.y > this.bottomRight.y) other.topLeft.y - this.bottomRight.y
    else 0)
  }
}

object Box{
  def apply(tl: Coords, br: Coords) = new Box(
      Coords(if(tl.x < br.x) tl.x else br.x, if(tl.y < br.y) tl.y else br.y),
      Coords(if(tl.x > br.x) tl.x else br.x, if(tl.y > br.y) tl.y else br.y)
    )
  
  def apply(left: Int, top: Int, right: Int, bottom: Int) = new Box(Coords(left, top), Coords(right, bottom))
}