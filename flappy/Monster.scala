package flappy
import os1.grid._
import os1.monsters._

class Monster (name: String, body: RobotBody, val hero: Hero) extends RobotBrain(name, body) {
  def moveBody = {
    // if hero is visible move towards and attack
    val directionToHero = hero.visibilityToMonster(this.body.location)
    if (directionToHero.isDefined) {
      body.moveTowards(directionToHero.get)
    }
  }
  
}