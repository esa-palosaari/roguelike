/**
 * 
 */
package flappy;

import o1._
//import s1._
import os1.monsters._
import os1.grid._
import scala.collection.mutable._
import math._
import scala.util.Random

import s1._

/**
 * 
 * @param world            the world inhabited by the hero
 * @param initialLocation  the initial location of the hero in its world
 * @param initialFacing    the direction the hero initially faces in
 * @param visibleRadius    the visibility distance/radius of the hero in the grid
 * 
 * Look from RobotBody how to move Hero and other stuff.
 */
class Hero(initialLocation: Coords, initialFacing: os1.grid.Direction) 
  extends RobotBody(null, initialLocation, initialFacing) {
	
  //var radius = visibleRadius;
    
  val heroPic2 = circle(20, Green)
  val heroPic3 = circle(20, LightGray)
  val heroPics = Array(heroPic2, heroPic3)
  var pind = 1  // index of current picture of hero
  var pic = heroPics(pind) 
  this.maxHealthPoints = 100
  this.currentHealthPoints = maxHealthPoints

	val animationSpeed = 20  // every N th model update.
  
	def visibleFrom(loc: Coords): Boolean = {
    val dx = if(loc.x - location.x < 0) -1 else 1
    val dy = if(loc.y - location.y < 0) -1 else 1
    val mx = abs(location.x - loc.x)
    val my = abs(location.y - loc.y)
    val bigger = max(mx, my)
    for(i <- 1 until bigger){
      
      val loca = Coords(
          (0.5 + location.x.toFloat + dx * ((i * mx).toFloat / bigger)).toInt,
          (0.5 + location.y.toFloat + dy * ((i * my).toFloat / bigger)).toInt
      )
      val elem = this.world.elementAt(loca)
      if(elem == Wall){
        return false
      }
    }
    true
  }
  def fight(monster: RobotBody) = {
    val r = new Random()
    monster.currentHealthPoints -= r.nextInt(monster.maxHealthPoints)
    this.currentHealthPoints -= r.nextInt(5)
  }
  
}
 

