package flappy

import os1.grid._
import os1.monsters._

object WorldGenerator {
  
  def default(width: Int, height: Int, seed: Int) = {
    
    val world = new RobotWorld(width, height)
    
    world.addWalls(Box(1, 1, width, height))
    
    world
  }
}