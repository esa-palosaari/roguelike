package o1.robots

import o1.grid._


/** The class `RobotBrain` represents the "brains" (or artificial intelligence, AI) of virtual 
  * robots that inhabit two dimensional grid worlds. A robot brain is equipped with an algorithm 
  * for determining what a robot should do during its turn in a robot simulation. In other words, 
  * a robot brain is capable of controlling the actions of a robot body.
  *
  * Concrete classes that extend this class need to provide implementations for the abstract
  * `moveBody` method; each such concrete class can represent a new kind of robot behavior.
  *
  * @param initialName  the name of the robot 
  * @param body         the body the robot brain will control */
abstract class RobotBrain(initialName: String, val body: RobotBody) {
  
  private var nameIfAny: Option[String] = None
  this.name = initialName

  
  /** Changes the robot's name to the given one.
    *
    * Note to students: In Scala, a method whose name ends in an underscore and an
    * equals sign -- like this one's -- can be called using a special syntax. 
    * For instance, this method can be called either with the statement `bot.name_=("Bob")` 
    * or simply with an assignment statement: `bot.name = "Bob"`. You won't find many 
    * uses for this yet in this introductory course, but it's nice to know nevertheless. */
  def name_=(newName: String) = {
    this.nameIfAny = if (newName.trim.isEmpty) None else Some(newName)
  }
  
  
  /** Returns the name of the robot. If the robot's name has been set to the empty 
    * string or contains only whitespace, returns `"Incognito"` instead. */
  def name = this.nameIfAny.getOrElse("Incognito")

  
  /** Moves the robot. What this means in practice depends on the type (subclass) of the 
    * robot. This method assumes that it is not called if the robot is broken or stuck. */
  def moveBody() 
  
  
  /** Returns the world that this robot is located in. */
  def world = this.body.world

  
  /** Returns the location of this robot in its robot world. */
  def location = this.body.location

  
  /** Returns the direction this robot is facing in. */
  def facing = this.body.facing

  
  /** Returns the coordinates that point at the square that is immediately in front 
    * of this robot. */
  def locationInFront: Coords = this.location.neighbor(facing)
  
  
  /** Returns the square that is immediately in front of this robot. */
  def squareInFront = this.body.neighboringSquare(facing)

  
  /** Returns the brain of the robot immediately in front of this robot.
    * The brain is returned in an `Option`; `None` is returned if there is 
    * no robot in that square or if the robot that is there has no brain. 
    * [square.robot = Option[RobotBody]]
    * [.brain = Option[RobotBrain]]
    * */
  def robotInFront: Option[RobotBrain] = {
    if(!squareInFront.robot.isEmpty) {
      squareInFront.robot.get.brain
    } else {
      None
    }
  }
  

  /** Moves the robot one square forwards, if there is nothing there.
    * If there is any obstacle in the square, does ''not'' move, so 
    * this method never causes collisions.
    * @return `true` if the move was successful, `false` if it was blocked */
  def moveCarefully() = {
    if(!squareInFront.robot.isEmpty || !this.body.canMoveTowards(facing)) {
      false
    } else {
      this.body.moveTowards(facing)
      true
    }
  }
  
  /** Returns a textual representation of the robot (which is the robot's name). */
  override def toString = this.name
  
  
}