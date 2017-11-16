package os1.monsters

import os1.grid._
import Math._

/** The class Psychobot represents the "brains" (or AI) of robots 
 *  which stand still until they see another unbroken robot on their radar. 
 *  When this happens, they ram the victim with incredible speed.
 */

class Psychobot(name: String, body: RobotBody) extends RobotBrain(name, body) {
  
  private var suunta: Direction = East
  private var returnBot: Option[RobotBody] = None
  
  /* Moves the robot. During its turn, a psychobot uses its radar 
   * to scan the four main compass directions, always starting with 
   * north and continuing clockwise. (It does not change facing while 
   * doing this.) When the psychobot notices a mobile robot, it turns 
   * to face it, instantly moves an unlimited number of squares towards 
   * the robot, and rams into it, causing the victim to break. After 
   * the collision, the psychobot remains in the square adjacent to its 
   * victim, and stops scanning for the rest of the turn. During its next 
   * turn, it will start over by looking north.
   * 
   * If there are no victims in any of the four main directions, the robot 
   * waits still. It does not attack broken robots. A psychobot can not 
   * scan through walls and it can never collide with a wall. It also does 
   * not see through robots, not even broken ones.
   * 
   * This method assumes that it is called only if the robot is not broken or stuck. */
  
  def moveBody = {
    // scan for robots
    scanner
    // if a mobile robot is found
    if(returnBot.isDefined) {
      var matka = 0
      if (suunta == East || suunta == West) {
        matka = abs(returnBot.get.location.x - this.body.location.x)
      } else {
        matka = abs(returnBot.get.location.y - this.body.location.y)
      }
      
      // the bot moves an unlimited number of squares towards it until collision
      for(i <- 0 until matka) {
        this.body.moveTowards(suunta)
      }
    }
    returnBot = None

  }
  
  def scanner: Unit = {
    // scan for a mobile bot, 4 directions, starting from North
    // cannot scan through walls
    // does not attack broken robots
    val dirs = os1.grid.Direction.Clockwise
    for(dir <- dirs) {
      var i = 1  
      var ruutu = this.body.location.neighbor(dir)
      while(!world.elementAt(ruutu).isUnpassable && world.elementAt(ruutu).robot.isEmpty) {
        ruutu = ruutu.neighbor(dir)     
      }
      if(world.elementAt(ruutu).robot.exists(_.isMobile)) {
        returnBot = world.elementAt(ruutu).robot
        suunta = dir
        return
      }
    }
  }
}