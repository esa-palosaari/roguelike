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
    
    val hero_visible = hero.visibleFrom(this.body.location)
    
    if(hero_visible){
      if(hero.location != target){
        this.path = pathFinder.findPath(this.location, hero.location)
      }
    }
    
    this.path match{
      case Some(p) => {
        val d = p.front
        if(body.moveTowards(d)){
          p.dequeue()
          if(p.isEmpty){
            this.path = None
          }
        }else{
          fight()
        }
      }
      case None => Unit
    }

  }
  
  def walkToLocation(coords: Coords) = {
    this.path = pathFinder.findPath(this.location, coords)
  }
  
  def fight() = {
    ???
  }
  
}