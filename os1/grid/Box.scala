package os1.grid

class Box(val topLeft: Coords, val bottomRight: Coords) {
  
  def size = (bottomRight.x - topLeft.x + 1) * (bottomRight.y - topLeft.y + 1)
}

object Box{
  def apply(tl: Coords, br: Coords) = new Box(tl, br)
  def apply(top: Int, left: Int, bottom: Int, right: Int) = new Box(Coords(top, left), Coords(bottom, right))
}