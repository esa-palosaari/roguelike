package flappy
import o1._
import s1._
import os1.monsters._
import os1.grid._
import scala.swing.event.Key

object FlappyWorld extends App {

  /* Size of the game area */
  val tileSize = 24
  
  val floorWidth = 24
  val floorHeight = 16
  
  val height = tileSize * (floorHeight + 2)
  val width = tileSize * (floorWidth + 2)
  
  /* Image of Flappy and her background */
  
  
  
  def coords2Pos(coords: Coords): Pos = {
    val x = coords.x + 1
    val y = coords.y + 1
    return new Pos(x * tileSize - tileSize/2, y * tileSize - tileSize/2)
  }
  
  //val background = rectangle(width, height, Blue)
  val heroPic2 = circle(20, Green)
  val wallPic = rectangle(tileSize, tileSize, Red)
  
  def makeBackground() = rectangle(width, height, Blue)
    
  /* The data model used by the game - You will need to extend this
   * with at least 3 obstacles, but you are free to take it as much further you
   * want. */
  object Flappy {
    
    /* Each time the model is updated ...*/
    def act()={
    // TODO ****************  MOnsters here keep moving all the time towards hero (see Hero.visibilityToMonster)
    }
  }

  //var floor = new RobotWorld(floorWidth, floorHeight)
  var floor = WorldGenerator.default(floorWidth, floorHeight, 2)
  var floor_pic = makeBackground()
  for(tile <- floor.allElementsIndexes) {
    tile._1 match{
      case Wall => {
        floor_pic = wallPic.onto(floor_pic, new Pos(tile._2.x * tileSize + tileSize / 2, tile._2.y * tileSize + tileSize / 2))
      }
      case _ => Unit
    }
  }

  var hero = new Hero(4, floor, new Coords(5, 5), North)
  
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
      pic.place(heroPic2, coords2Pos(hero.location))  
    }
    
    // And whenever any key is pressed we make it move
    override def onKeyUp(key: Key) = {
      // bird.jump()
      var d : os1.grid.Direction = key match {
        case Key.Up => North
        case Key.Down => South
        case Key.Right => East
        case Key.Left => West
        case _ => hero.facing
      }
      hero.spinTowards(d)
      
      println("DEBUG:", hero.neighboringSquare(d)) // DEBUG
      
      if(hero.canMoveTowards(d))
        hero.moveTowards(d)
        
        
    } 
    
  }
  
  // Finally, this starts the game engine
  view.start()

}