package flappy
import os1.grid._
import os1.monsters._
import scala.util.Random

class Monster (name: String, body: RobotBody, val hero: Hero) extends RobotBrain(name, body) {
  val r = new Random()
  def moveBody = {
    // if hero is visible move towards and attack
    val directionToHero = hero.visibilityToMonster(this.body.location)
    if (directionToHero.isDefined) {
      body.moveTowards(directionToHero.get)
    }/*  else { // random movement
      val dirs = os1.grid.Direction.Clockwise
      val index = r.nextInt(4)
      body.moveTowards(dirs(index))
    }*/
  }
  
}