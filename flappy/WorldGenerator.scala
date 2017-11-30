package flappy

import os1.grid._
import os1.monsters._

import scala.util.Random

import scala.collection.mutable.ArrayBuffer

object WorldGenerator {
  
  def default(width: Int, height: Int, seed: Int) = {
    
    val world = new RobotWorld(width, height)
    
    world.addWalls(Box(1, 1, width, height))
    
    var current_cover = width * height
    val target_cover = current_cover / 2
    
    val rand = new Random(seed)
    
    //place a bunch of rooms
    var rooms = ArrayBuffer[Box]()
    
    var max_rooms = current_cover / 16
    while(current_cover > target_cover && max_rooms > 0){
      max_rooms -= 1
      
      var (left, top) = (-1, -1)
      
      while(left < 0){
        val (x, y) = (rand.nextInt(width) + 1, rand.nextInt(height) + 1)
        if(world.elementsAt(Box(x - 1, y - 1, x + 1, y + 1)).forall(!_.isEmpty)){
          left = x
          top = y
        }
      }
      
      var (right, bottom) = (left, top)
      
      for(i <- 0 until rand.nextInt(7) + 5){
        rand.nextInt(4) match{
          case 0 => {
            if(left > 1 && world.elementsAt(Box(left - 2, top - 1, left - 1, bottom + 1)).forall(!_.isEmpty)){
              left -= 1
            }
          }
          case 1 => {
            if(right < width - 2 && world.elementsAt(Box(right + 1, top - 1, right + 2, bottom + 1)).forall(!_.isEmpty)){
              right + 1
            }
          }
          case 2 => {
            if(top > 1 && world.elementsAt(Box(left - 1, top - 2, right + 1, top - 1)).forall(!_.isEmpty)){
              top -= 1
            }
          }
          case 3 => {
            if(bottom < height - 2 && world.elementsAt(Box(left - 1, bottom + 1, right + 1, bottom + 2)).forall(!_.isEmpty)){
              bottom += 1
            }
          }
        }
      }
      
      val area = Box(left, top, right, bottom)
      world.removeWalls(area)
      current_cover -= area.size
      rooms += area
    }
    
    //connect all the rooms
    val room_pairs: Array[(Int, Int)] = (for{
      i <- 0 until rooms.length
      j <- i + 1 until rooms.length
    } yield (i, j, rooms(i).distance(rooms(j)))).sortBy(_._3).map(elem => (elem._1, elem._2)).toArray
    
    val room_sets = (0 until rooms.length).toArray
    def set_id(idx: Int): Int = {
      if(room_sets(idx) == idx) idx else set_id(room_sets(idx))
    }
    
    for((i, j) <- room_pairs){
      if(set_id(i) != set_id(j)){
        val point1 = Coords(
            rand.nextInt(rooms(i).bottomRight.x + 1 - rooms(i).topLeft.x) + rooms(i).topLeft.x,
            rand.nextInt(rooms(i).bottomRight.y + 1 - rooms(i).topLeft.y) + rooms(i).topLeft.y
        )
        val point2 = Coords(
            rand.nextInt(rooms(j).bottomRight.x + 1 - rooms(j).topLeft.x) + rooms(j).topLeft.x,
            rand.nextInt(rooms(j).bottomRight.y + 1 - rooms(j).topLeft.y) + rooms(j).topLeft.y
        )
        val point3 = rand.nextInt(2) match{
          case 0 => Coords(point1.x, point2.y)
          case 1 => Coords(point2.x, point1.y)
        }
        world.removeWalls(Box(point1, point3))
        world.removeWalls(Box(point2, point3))
        
        room_sets(set_id(i)) = set_id(j)
      }
    }
    
    world
  }
}