package flappy
import o1._
import s1._
import os1.monsters._
import os1.grid._
import scala.swing.event.Key
import scala.util.Random

object FlappyWorld extends App {
  
  val rand = new Random()

  var timerCnt = 0   // increased +1 every time model is updated.
  /* Size of the game area */
  val tileSize = 24
  
  val floorWidth = 24
  val floorHeight = 16
  
  val height = tileSize * (floorHeight + 2)
  val width = tileSize * (floorWidth + 2)

  // Convert Coords coordinates to Pos-coordinates.  
  def coords2Pos(coords: Coords): Pos = {
    val x = coords.x + 1
    val y = coords.y + 1
    return new Pos(x * tileSize - tileSize/2, y * tileSize - tileSize/2)
  }
  
  val wallPic = rectangle(tileSize, tileSize, Red)
  val stairsPic = rectangle(tileSize, tileSize, Yellow)
  
  def makeBackground() = rectangle(width, height, Blue)
    
  /* The data model used by the game - You will need to extend this
   * with at least 3 obstacles, but you are free to take it as much further you
   * want. */
  object Flappy {
    
    /* Each time the model is updated ...*/
    def act()={
      timerCnt = (timerCnt + 1 ) % hero.animationSpeed // Speed of animation
      if (timerCnt == 0) {
        hero.pind = (hero.pind + 1) % 2
        hero.pic = hero.heroPics(hero.pind)
      }

      
      
    // TODO ****************  MOnsters here keep moving all the time towards hero (see Hero.visibilityToMonster)
    }
  }
  
  var hero = new Hero(4, null, null, North)
  
  //world generation stuff
  var floor: RobotWorld = null
  var floor_pic = makeBackground()
  
  def getRandomEmptyTile(rn: Random): (Square, Coords) = {
    var ret: (Square, Coords) = null
    while(ret == null){
      val (x, y) = (rn.nextInt(floorWidth), rn.nextInt(floorHeight))
      val elem = floor.elementAt(Coords(x, y))
      if(elem.isEmpty){
        ret = (elem, Coords(x, y))
      }
    }
    ret
  }
  
  def createNewFloor(): Unit = {
    floor = WorldGenerator.default(floorWidth, floorHeight, rand)
    
    val start = getRandomEmptyTile(rand)
    hero.place(floor, start._2)
    
    floor_pic = makeBackground()
    for(tile <- floor.allElementsIndexes){
      tile._1 match{
        case _: Stairs => {
          floor_pic = stairsPic.onto(floor_pic, new Pos(tile._2.x * tileSize + tileSize / 2, tile._2.y * tileSize + tileSize / 2))
        }
        case Wall => {
          floor_pic = wallPic.onto(floor_pic, new Pos(tile._2.x * tileSize + tileSize / 2, tile._2.y * tileSize + tileSize / 2))
        }
        case _ => Unit
      }
    }
  }
  
  createNewFloor()
  
  /** This view is responsible for updating the model at static intervals,
   * listening to key presses and mouse movements and most importantly, drawing
   * Flappy. (or whatever the model depicts) 
   */
  
  val view = new s1.gui.mutable.View(Flappy) {
    // Let's store the model into 'bird' for more clarity
    val bird = model
    
    // and on each time tick, make her act
    override def onTick() = bird.act()
    
    def makePic() = {
      var pic = floor_pic
      pic.place(hero.pic, coords2Pos(hero.location))  
    }
    
    // And whenever cursor key is pressed we make it move
    override def onKeyUp(key: Key) = {
      // bird.jump()
      var d : os1.grid.Direction = key match {
        case Key.Up => North
        case Key.Down => South
        case Key.Right => East
        case Key.Left => West
        
        case Key.Numpad1 => SouthWest
        case Key.Numpad2 => South
        case Key.Numpad3 => SouthEast
        case Key.Numpad4 => West
        case Key.Numpad5 => NoDirection
        case Key.Numpad6 => East
        case Key.Numpad7 => NorthWest
        case Key.Numpad8 => North
        case Key.Numpad9 => NorthEast
        
        case _ => hero.facing
      }
      hero.spinTowards(d)
      
      println("DEBUG:", hero.neighboringSquare(d)) // DEBUG
      
      if(hero.canMoveTowards(d))
        hero.moveTowards(d)
        
      if(hero.facing == NoDirection){
        hero.neighboringSquare(d) match {
          case _: Stairs => createNewFloor()
          case _ => Unit
        }
      }
    } 
    
  }
  
  // Finally, this starts the game engine
  view.start()

}