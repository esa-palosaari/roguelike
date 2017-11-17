package flappy
import o1._
import s1._
import os1.monsters._
import os1.grid._

object FlappyWorld extends App {

  /* Size of the game area */
  val tileSize = 24
  
  val floorWidth = 24
  val floorHeight = 16
  
  val height = tileSize * (floorHeight + 2)
  val width = tileSize * (floorWidth + 2)
  
  /* Image of Flappy and her background */
  
  
  
  //val background = rectangle(width, height, Blue)
  val flappyPic = circle(20, Red)
  val wallPic = rectangle(tileSize, tileSize, Red)
  
  def makeBackground() = rectangle(width, height, Blue)
    
  /* The data model used by the game - You will need to extend this
   * with at least 3 obstacles, but you are free to take it as much further you
   * want. */
  object Flappy {
    var y      = 200   // The current height of Flappy
    var ySpeed = 0     // Her vertical speed
    val yAccel = 2     // ...and her vertical acceleration
    
    /* Each time the model is updated ...*/
    def act()={
      // ..use the speed to move flappy
      y      = math.min(y - ySpeed, height)
      // ..and update the speed with the gravity
      ySpeed = ySpeed - yAccel
    }
    
    /* If the bird jumps, its upward speed is set to 20 */
    def jump()={
      ySpeed = 20
    }    
  }
  
  var floor = new RobotWorld(floorWidth, floorHeight)
  
  /** This view is responsible for updating the model at static intervals,
   * listening to key presses and mouse movements and most importantly, drawing
   * Flappy. (or whatever the model depicts) 
   */
  
  val view = new s1.gui.mutable.View(Flappy) {
    // Let's store the model into 'bird' for more clarity
    val bird = model
    
    // and on each time tick, make her act
    override def onTick() = bird.act()

    // When we need to draw Flappy, we make an image with flappy on the background
    //def makePic() = background.place(flappyPic, new Pos(width / 2, bird.y))
    
    def makePic() = {
      var pic = makeBackground()
      
      for(tile <- floor.allElementsIndexes){
        tile._1 match {
          case Wall => {
            pic = pic.place(wallPic, new Pos(tile._2.x * tileSize + tileSize / 2, tile._2.y * tileSize + tileSize / 2))
          }
          case _ => Unit
        }
      }
      
      pic
    }
    
    // And whenever any key is pressed and released, we make her jump
    override def onKeyUp(key: Key) = {
      bird.jump()
    } 
    
  }
  
  // Finally, this starts the game engine
  view.start()

}