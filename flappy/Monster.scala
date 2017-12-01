package flappy
import os1.grid._
import os1.monsters._
import scala.util.Random
import scala.collection.mutable.Queue

class Monster (name: String, body: RobotBody, val hero: Hero, val pathFinder: PathFinder) extends RobotBrain(name, body) {
  val r = new Random()
  
  private var path: Option[Queue[Direction]] = None
  private var target: Coords = Coords(-1, -1)
  
  def moveBody = {
    // if hero is visible move towards and attack
    
    if(hero.visibleFrom(this.body.location)){
      if(hero.location != target){
        this.path = pathFinder.findPath(this.location, hero.location)
      }
    }
    
    this.path match{
      case Some(p) => {
        val d = p.dequeue()
        if(p.isEmpty){
          this.path = None
        }
        body.moveTowards(d)
      }
      case None => Unit
    }

  }
  
}