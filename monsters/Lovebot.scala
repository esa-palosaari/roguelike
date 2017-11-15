package o1.robots

import Math._
import o1.grid._

/** The class Lovebot represents the "brains" (or AI) of robots which have 
 *  another robot as their "beloved". A lovebot tries to home in on its beloved.
 * */
 
class Lovebot(name: String, body: RobotBody, val beloved: RobotBody) extends RobotBrain(name, body) {
  
  /** Moves the robot. A lovebot tries to home in on its beloved unless it already is in an adjacent 
   *  square (diagonally adjacent is not good enough). The lovebot blindly follows its primitive urges 
   *  and doesn't know how to avoid obstacles. If there is a wall or another bot in the chosen direction, 
   *  the lovebot will collide with it (as per the moveTowards method of its body, possibly colliding and 
   *  breaking itself or another robot). 
   *  The path of movement is chosen as follows. First the lovebot calculates its distance to its target 
   *  in both the x and y dimensions. It moves one square at a time so that either the horizontal (x) or 
   *  the vertical (y) distance decreases by one, choosing the one that is greater. In case the distances 
   *  are equal, the horizontal distance is chosen. The lovebot only moves one square per turn. It turns to 
   *  face the direction it attempts to move in, even if that attempt fails because of a collision. The bot 
   *  does not turn or move if it is already close enough to its beloved.
   *  This method assumes that it is called only if the robot is not broken or stuck. */
  def moveBody = {
    // check if adjacent to beloved. if yes do nothing
    if(!world.neighbors(body.location, false).contains(beloved.locationSquare)) {
      // turn to direction of movement
      // body.spinTowards(belovedDirection)
      // move one square
      body.moveTowards(belovedDirection)
   }
  }
  
  private def belovedDirection: Direction = {
    // calculate distance in x and y
    val x = beloved.location.x - body.location.x
    val y = beloved.location.y - body.location.y
    // choose greater of x and y
    val abx = abs(x)
    val aby = abs(y)
    // if equal, choose x
    if(abx >= aby) {
      if(x > 0) East else West
    } else {
      if(y > 0) South else North
    }
  }
}
