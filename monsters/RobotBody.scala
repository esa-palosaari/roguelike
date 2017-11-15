package monsters

import o1.grid._


/** The class `RobotBody` represents virtual robots (or "bots") which inhabit two-dimensional 
  * grid worlds. More specifically, each instance of the class represents a "robot body" 
  * (or "chassis" or "hardware") and basic functionality of such a robot. Each "robot body" 
  * is associated with a "robot brain" that controls the body and determines what functionality 
  * is activated and when.
  *
  * A robot is equipped with the various capabilities:
  *
  * - It can sense its own surroundings (location, facing, the world that it is in).
  *
  * - It can spin around in any one of the four main compass directions.
  *
  * - It can move into the next square in a given direction.
  *
  * - It can sense whether it is broken or not, and whether it is "stuck" in a square between 
  *   four walls.
  *
  * When a robot's `takeTurn` method is called, it uses its "brain" to figure out what to do 
  * (move, turn about, etc.). Robots with different kinds of brains behave differently.
  *
  * @param world            the world inhabited by the robot
  * @param initialLocation  the initial location of the robot in its world
  * @param initialFacing    the direction the robot initially faces in
  *
  * @see [[RobotWorld]]
  * @see [[RobotBrain]] */
class RobotBody(val world: RobotWorld, initialLocation: Coords, initialFacing: Direction) { 
  
  /** the robot's brain (if it has one) */ 
  var brain: Option[RobotBrain] = None              // most-recent holder (it is possible for robot to change brains, but this is only done in Chapter 12.1)
  private var coordinates = initialLocation         // gatherer: changes in relation to the old location
  private var isIntact = true                       // flag: can be broken or repaired
  private var facesTowards = initialFacing          // most-recent holder (can be changed arbitrarily)
  
  
  /** Returns the coordinates that indicate the robot's current location in the robot world. */
  def location = this.coordinates

  
  /** Returns the square the robot is currently in. */
  def locationSquare = this.world(this.location)

  
  /** Returns a square that neighbors the robot's current location in the given direction. */
  def neighboringSquare(direction: Direction): Square = world.elementAt(this.location.neighbor(direction)) 

  /** Returns the direction the robot is currently facing in. */
  def facing = this.facesTowards
  
  
  /** Turns the robot to face in the specified direction. */
  def spinTowards(newFacing: Direction) = {
    this.facesTowards = newFacing 
  }

  
  /** Turns the robot 90 degrees clockwise. */
  def spinClockwise() = {
    this.facesTowards = this.facesTowards.clockwise
  } 

  
  /** Causes the robot to malfunction (typically as the result of a collision).
    * A broken robot does not do anything during its turns.
    * @see [[fix]] */
  def destroy() = {
    this.isIntact = false
  }

  
  /** Repairs a broken robot. The robot can now start taking its turns normally.
    * @see [[destroy]] */
  def fix() = {
    this.isIntact = true
  }
  
  
  /** Relocates the robot within its current world to the square next to the robot's 
    * current location, in the given direction. The direction does not necessarily have 
    * to be the same one that the robot is originally facing in.
    *
    * This method turns the robot to face in the direction it moves in. 
    *
    * Two robots can never be in the same square; neither can a robot and a wall. If the 
    * robot's would-be location is not empty, a collision occurs instead and the robot 
    * does not change locations (but still turns to face whatever it collided with). 
    *
    * If the moving robot collides with a wall, the robot itself breaks. If a moving robot 
    * collides with another robot, the other robot breaks and the moving robot stays intact. 
    *
    * @return `true` if the robot successfully changed locations, `false` if it 
    *         did not (even if it changed facing) */
  def moveTowards(direction: Direction) = { 
    this.spinTowards(direction)
    val targetCoordinates = this.location.neighbor(direction)
    val targetSquare = this.world(targetCoordinates)
    val managedToMove = targetSquare.addRobot(this)
    if (managedToMove) {
      this.locationSquare.clear()
      this.coordinates = targetCoordinates
    }
    managedToMove
  }
  
  
  /** Gives the robot a turn to act. 
    *
    * A robot that is not mobile (as defined by `isMobile`) does not do anything during 
    * its turn. A brainless robot likewise does nothing. A mobile robot with a brain 
    * consults its brain to find out what to do. It does this by calling the brain's 
    * `moveBody` method.   
    *
    * @see [[RobotBrain.moveBody]] */
  def takeTurn() = {
    if (this.isMobile) {
      // TODO: call the brain's moveBody method (if there is a brain)
      brain.foreach(_.moveBody())
    }
  }
  
  
  /** Determines whether the robot is currently mobile or not. A robot is not mobile if it
    * is broken or stuck. Otherwise, the robot is considered mobile. */
  def isMobile = this.isIntact && !this.isStuck 
  
  
  /** Determines whether the robot is currently broken or not, that is, if it has been 
    * broken with the `destroy` method and not fixed since. */
  def isBroken = !this.isIntact 
  
  
  /** Determines whether the square neighboring the robot in the given direction contains 
    * a permanently unpassable obstacle (a wall, that is). Another robot is not considered an 
    * unpassable obstacle for this purpose. */
  def canMoveTowards(direction: Direction): Boolean =  {
    neighboringSquare(direction) != Wall
  }
  
  /** Determines whether the robot is stuck or not, that is, if there are any squares around 
    * the robot that don't contain unpassable obstacles (walls). Only the nearest squares in 
    * the four main compass directions are considered. If there is a wall in all directions, 
    * the robot is considered stuck. Another robot is not considered an unpassable obstacle 
    * for this purpose.
    * @see [[canMoveTowards]] */
  def isStuck = (!canMoveTowards(East) && !canMoveTowards(West) && !canMoveTowards(North) && !canMoveTowards(South))

 
  
}
