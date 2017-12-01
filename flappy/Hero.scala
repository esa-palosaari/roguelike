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

import s1._

/**
 * @author jlohikos
 * 
 * @param world            the world inhabited by the hero
 * @param initialLocation  the initial location of the hero in its world
 * @param initialFacing    the direction the hero initially faces in
 * @param visibleRadius    the visibility distance/radius of the hero in the grid
 * 
 * Look from RobotBody how to move Hero and other stuff.
 */
class Hero(visibleRadius: Int, initialLocation: Coords, initialFacing: os1.grid.Direction) 
  extends RobotBody(null, initialLocation, initialFacing) {
	
  var radius = visibleRadius;
    
  val heroPic2 = circle(20, Green)
  val heroPic3 = circle(20, LightGray)
  val heroPics = Array(heroPic2, heroPic3)
  var pind = 1  // index of current picture of hero
  var pic = heroPics(pind) //  circle(20, Green)

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
	
  // returns the direction (wrapped in Some) from monster to hero 
  // May be None if the hero cannot see the monster.
	def visibilityToMonster(loc: Coords): Option[os1.grid.Direction] = {
	// Checks if block-distance (Manhattan) is less than equal to radius. If not => return None
	// Check if there is object blocking visibility in between. If yes => None
	// return direction (the closest of 4 or 8) to the hero
    val distance = distance8ways(location,loc)
	  // val distance = blockdistance(location, loc)
	  if (distance > radius) 
	    return None
	  val dy = location.y - loc.y
	  val dx = location.x - loc.x
	  if (abs(dy) > abs(dx))
	    if (dy < 0)
	      return Some(North)
	    else
	      return Some(South)
	   else
	     if (dx < 0)
	       return Some(West)
	     else
	       return Some(East)
	}
	// returns a buffer of coordinates visible to the hero
	// May return None, if hero has gone blind or it is clowdy midnight.
	def visibleSquares: Option[ListBuffer[Coords]] = {
  	// Get a block with width=2*visibleRadius and height=2*radius.
  	// Check all grid coords in the block, if they are inside radius.
  	// https://en.wikipedia.org/wiki/Taxicab_geometry
  	// return the list of coords.
  	val squares = new ListBuffer[Coords]
	  val ly = location.y
	  val lx = location.x
  	for {
  	  y <- ly-radius/2 to ly+radius/2 + 1
  	  x <- lx-radius/2 to lx+radius/2 + 1
  	} {
  	  var loc2 = new Coords(x,y)
  	  if (distance8ways(location, loc2) <= visibleRadius)
  	    squares.+=(loc2)
  	}
  	return Some(squares)
	}
	
	// Manhattan distance between two points
  // https://en.wikipedia.org/wiki/Taxicab_geometry
	def blockdistance(loc1: Coords, loc2: Coords): Int = {
	  return abs(loc1.y - loc2.y) + abs(loc1.x - loc2.x)
	  }

  def distance8ways(loc1: Coords, loc2: Coords): Int = {
    return (0.5 + math.sqrt(math.pow(abs(loc1.y - loc2.y), 2) + math.pow(abs(loc1.x - loc2.x), 2))).toInt
  }
  
}
 

