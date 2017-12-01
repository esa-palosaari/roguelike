package flappy

import os1.grid._
import os1.monsters._

import scala.collection.mutable.Queue

class HelperGrid(width: Int, height: Int) extends Grid[Direction](width, height){}

class PathFinder(world: RobotWorld) {
  
  def findPath(from: Coords, to: Coords): Option[Queue[Direction]] = {
    val helperGrid = new HelperGrid(world.width, world.height)
    
    helperGrid.update(to, NoDirection)
    
    val searchList = Queue[Coords]()
    
    searchList.enqueue(to)
    
    var done = false
    while(!done){
      if(searchList.isEmpty){
        return None
      }
      val elem = searchList.dequeue()
      val new_elems = helperGrid.neighborsIndexes(elem, true).filter(_._1 == null).map(_._2)
      for(e <- new_elems){
        helperGrid.update(e, e.direction(elem))
        if(world.elementAt(e).isEmpty){
          searchList.enqueue(e)
        }
        if(e == from){
          done = true
        }
      }
    }
    
    val res = Queue[Direction]()
    var coord = from
    while(coord != to){
      val dir = helperGrid(coord)
      res += helperGrid(coord)
      coord = coord.neighbor(helperGrid(coord))
    }
    Some(res)
  }
}
