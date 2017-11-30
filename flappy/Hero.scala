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
class Hero(visibleRadius: Int, world: RobotWorld, initialLocation: Coords, initialFacing: Direction) 
  extends RobotBody(world, initialLocation, initialFacing) {
	
  var radius = visibleRadius;
  var heroPic = None //  circle(20, Red)
	
  // returns the direction (wrapped in Some) from monster to hero 
  // May be None if the hero cannot see the monster.
	def visibilityToMonster(loc: Coords): Option[Direction] = {
	// Checks if block-distance (Manhattan) is less than equal to radius. If not => return None
	// Check if there is object blocking visibility in between. If yes => None
	// return direction (the closest of 4 or 8) to the hero
	  val distance = blockdistance(location, loc)
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
  	  if (blockdistance(location, loc2) <= visibleRadius)
  	    squares.+=(loc2)
  	}
  	return Some(squares)
	}
	
	// Manhattan distance between two points
  // https://en.wikipedia.org/wiki/Taxicab_geometry
	def blockdistance(loc1: Coords, loc2: Coords): Int = {
	  return abs(loc1.y - loc2.y) + abs(loc1.x - loc2.x)
	  }
}


