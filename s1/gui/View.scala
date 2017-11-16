
package s1.gui

import s1.gui.event._
import View._
import AbstractView.unimplementedDefaultHandler 
import AbstractView.warn
import o1.util._

object View {
  
  val TicksPerSecondDefault = 24
  val TicksPerSecondMax = 1000

  abstract class RefreshPolicy {
    def shouldRefresh(from: Any, to: Any): Boolean
  }
  case object UnlessSameReference extends RefreshPolicy {
    def shouldRefresh(from: Any, to: Any) = (from, to) match {
      case (ref1: AnyRef, ref2: AnyRef) => ref1 ne ref2
      case otherwise                    => from != to
    }
  }
  case object UnlessIdentical extends RefreshPolicy {
    def shouldRefresh(from: Any, to: Any) = from != to
  }
  case object Always extends RefreshPolicy {
    def shouldRefresh(from: Any, to: Any) = true
  }

  object NothingToDraw extends RuntimeException
}


object immutable {
  abstract class View[Model](initialState: Model,
                             tickRate: Double = TicksPerSecondDefault,
                             title: String = "",
                             initialDelay: Int = 600, 
                             terminateOnClose: Boolean = true, 
                             closeWhenDone: Boolean = false, 
                             refreshPolicy: RefreshPolicy = Always) 
                             extends AbstractView(initialState, tickRate, title, initialDelay, terminateOnClose, closeWhenDone, refreshPolicy) { 
    view =>
  
    def makePic(state: Model): Pic
    override def isDone(state: Model)   = super.isDone(state) 
    override def isPaused(state: Model) = super.isPaused(state)
    override def sound(state: Model)    = super.sound(state)
  
    def onTick(previousState: Model): Model = unimplementedDefaultHandler
    
    def onMouseMove(state: Model, position: Pos)  : Model = unimplementedDefaultHandler
    def onMouseDrag(state: Model, position: Pos)  : Model = unimplementedDefaultHandler
    def onWheel    (state: Model, rotation: Int)  : Model = unimplementedDefaultHandler
    def onClick    (state: Model, position: Pos)  : Model = unimplementedDefaultHandler
    def onMouseDown(state: Model, position: Pos)  : Model = unimplementedDefaultHandler
    def onMouseUp  (state: Model, position: Pos)  : Model = unimplementedDefaultHandler
    def onKeyDown  (state: Model, key: Key)       : Model = unimplementedDefaultHandler
    def onKeyUp    (state: Model, key: Key)       : Model = unimplementedDefaultHandler
    def onType     (state: Model, character: Char): Model = unimplementedDefaultHandler
  
    def onMouseMove (state: Model, event: MouseMoved     ): Model = unimplementedDefaultHandler
    def onMouseDrag (state: Model, event: MouseDragged   ): Model = unimplementedDefaultHandler
    def onMouseEnter(state: Model, event: MouseEntered   ): Model = unimplementedDefaultHandler
    def onMouseExit (state: Model, event: MouseExited    ): Model = unimplementedDefaultHandler
    def onMouseUp   (state: Model, event: MouseReleased  ): Model = unimplementedDefaultHandler
    def onMouseDown (state: Model, event: MousePressed   ): Model = unimplementedDefaultHandler
    def onWheel     (state: Model, event: MouseWheelMoved): Model = unimplementedDefaultHandler
    def onClick     (state: Model, event: MouseClicked   ): Model = unimplementedDefaultHandler
    def onKeyDown   (state: Model, event: KeyPressed     ): Model = unimplementedDefaultHandler
    def onKeyUp     (state: Model, event: KeyReleased    ): Model = unimplementedDefaultHandler
    def onType      (state: Model, event: KeyTyped       ): Model = unimplementedDefaultHandler

    
    protected lazy val display = new Display { // XXX perhaps overkill; check if this kind of optimization (through complexity) is useful after main efficiency improvements to SMCL
      import s1.util.DiscardingThrottler
      import scala.concurrent.ExecutionContext.Implicits.global
      private val renderer = new DiscardingThrottler 
  
      protected def attemptRender(stateToDraw: Model) = {
        this.renderer.ifIdle( this.render(stateToDraw) ).onFailure {   
          case DiscardingThrottler.Busy => // warn("Skipped a frame.")
          case unexpectedProblem        => throw unexpectedProblem
        }
      }
    }
  }
}


object mutable {
  
    /**
     * Base class for an animated view that updates and displays the a state of a model. There are
     * two "flavors" of Views, one for a model that has `var`-type instance variables (which 
     * can change their values) and another where all variables are of type val.
     * 
     * This class is for the former type.
     * 
     * Any user-created class can be used as a model. At its simplest a model can be a just a number wrapped in a class.
     * For example it could be a Mushroom with its size stored as an Int.
     * 
     * {{{
     * class Mushroom() {
     *   var size = 10
     *   def grow() = {
     *     size = size + 1
     *   }
     * }
     * 
     * val mushroomModel = new Mushroom() // This is our model
     * }}}
     * 
     * To be used by our View, we must pass our model to it as a constructor parameter and tell
     * some key methods like [[onTick()*]] and [[makePic:*]] how to modify and display our model to the user.
     * In the example below our mushroom grows on each tick, and the picture of the mushroom created on each round
     * is a bit bigger as well.
     * 
     * Please note how the view handles creating the picture and the model knows how mushrooms grow.
     * 
     * 
     * {{{
     * val shroomShow = new mutable.View(mushroomModel) {
     *   // For convenience, we can call the model of this View with a more suitable name
     *   val mushroom = model
     *   
     *   def makePic = {
     *     val hatWidth = 3 * mushroom.size
     *   
     *     val hat  = circle(hatWidth, Red).cropToSizeOf(Bounds(0, 0, hatWidth, hatWidth/2))
     *     val stem = rectangle(mushroom.size, mushroom.size*2, SandyBrown)
     *     
     *     stem.onto(hat, my=TopCenter, atIts=BottomCenter)
     *   }
     *   
     *   override def onTick() = {
     *     mushroom.grow()
     *   }
     * }
     * 
     * }}}
     * 
     * There are also methods like `onClick(p:Pos):Unit` etc. that are used to make the view interactive.
     * 
     * @param model The model animated and shown by this View.
     */
  
  abstract class View[Model <: AnyRef](val model: Model, 
                                       tickRate: Double = TicksPerSecondDefault, 
                                       title: String = "",
                                       initialDelay: Int = 600, 
                                       terminateOnClose: Boolean = true, 
                                       closeWhenDone: Boolean = false, 
                                       refreshPolicy: RefreshPolicy = Always) 
                                       extends AbstractView(model, tickRate, title, initialDelay, terminateOnClose, closeWhenDone, refreshPolicy) {
    view =>
    
    /**
     * Creates and returns a picture that shows the latest state of the model.  

     * {{{
     * class Mushroom() {
     *   var size = 10
     *   def grow() = {
     *     size = size + 1
     *   }
     * }
     * 
     * val shroom = new Mushroom() // This is our model
     * 
     * new mutable.View(shroom) {
     *   // For convenience, we can call the model of this View with a more suitable name
     *   val mushroom = model
     *   
     *   def makePic = {
     *     val hatWidth = 3 * mushroom.size
     *   
     *     val hat  = circle(hatWidth, Red).cropToSizeOf(Bounds(0, 0, hatWidth, hatWidth/2))
     *     val stem = rectangle(mushroom.size, mushroom.size*2, SandyBrown)
     *     
     *     stem.onto(hat, my=TopCenter, atIts=BottomCenter)
     *   }
     *   
     *   // other methods like onTick omitted from this example
     * }
     * 
     * }}}
     */
    def makePic: Pic
    
    def isDone   = super.isDone(this.model)
    
    /**
     * Returns whether the view ticking is paused.
     */
    def isPaused = super.isPaused(this.model)
    
    /**
     * Returns a [[s1.sound.sampled.Sound]] to be played (on the next tick) in an Option wrapper or None.
     * If you need sounds played based on the current state you need to override this method.
     * 
     * 
     * {{{
     * val explosionSound = s1.sound.samples.Sound("explosion.wav", 0)
     * 
     * override def sound = if (mushroom.explodesNow) Some(explosionSound) else None
     * }}}
     * 
     */
    def sound    = super.sound(this.model)
    
    final override protected def makePic (model: Model) = this.makePic
    final override protected def isDone  (model: Model) = this.isDone 
    final override protected def isPaused(model: Model) = this.isPaused
    final override protected def sound   (model: Model) = this.sound
  
    private[s1] final override def onTick(model: Model) = { this.onTick(); this.model } 
    
    private[s1] final override def onMouseMove(model: Model, position: Pos)   = { this.onMouseMove(position); this.model }
    private[s1] final override def onMouseDrag(model: Model, position: Pos)   = { this.onMouseDrag(position); this.model }
    private[s1] final override def onWheel    (model: Model, rotation: Int)   = { this.onWheel(rotation)    ; this.model }
    private[s1] final override def onClick    (model: Model, position: Pos)   = { this.onClick(position)    ; this.model }
    private[s1] final override def onMouseDown(model: Model, position: Pos)   = { this.onMouseDown(position); this.model }
    private[s1] final override def onMouseUp  (model: Model, position: Pos)   = { this.onMouseUp(position)  ; this.model }
    private[s1] final override def onKeyDown  (model: Model, key: Key)        = { this.onKeyDown(key)       ; this.model }
    private[s1] final override def onKeyUp    (model: Model, key: Key)        = { this.onKeyUp(key)         ; this.model }
    private[s1] final override def onType     (model: Model, character: Char) = { this.onType(character)    ; this.model }
  
    private[s1] final override def onMouseMove (model: Model, event: MouseMoved     ) = { this.onMouseMove(event) ; this.model }
    private[s1] final override def onMouseDrag (model: Model, event: MouseDragged   ) = { this.onMouseDrag(event) ; this.model }
    private[s1] final override def onMouseEnter(model: Model, event: MouseEntered   ) = { this.onMouseEnter(event); this.model }
    private[s1] final override def onMouseExit (model: Model, event: MouseExited    ) = { this.onMouseExit(event) ; this.model }
    private[s1] final override def onMouseUp   (model: Model, event: MouseReleased  ) = { this.onMouseUp(event)   ; this.model }
    private[s1] final override def onMouseDown (model: Model, event: MousePressed   ) = { this.onMouseDown(event) ; this.model }
    private[s1] final override def onWheel     (model: Model, event: MouseWheelMoved) = { this.onWheel(event)     ; this.model }
    private[s1] final override def onClick     (model: Model, event: MouseClicked   ) = { this.onClick(event)     ; this.model }
    private[s1] final override def onKeyDown   (model: Model, event: KeyPressed     ) = { this.onKeyDown(event)   ; this.model }
    private[s1] final override def onKeyUp     (model: Model, event: KeyReleased    ) = { this.onKeyUp(event)     ; this.model }
    private[s1] final override def onType      (model: Model, event: KeyTyped       ) = { this.onType(event)      ; this.model }
  
    /**
     * Steps the model forward one tick (timestep). Typically the implementation of this method
     * will change the state of some objects in the model.
     * 
     * {{{
     * class Mushroom() {
     *   var size = 0
     *   def grow() = {
     *     size = size + 1
     *   }
     * }
     * 
     * val shroom = new Mushroom()
     * 
     * new mutable.View(shroom) {
     *   // by default the name of the model in side a View is `model`.
     *   // For clarity, we can call in with a more suitable name, in this case "mushroom".
     *   
     *   val mushroom = model
     *   
     *   override def onTick() = {
     *     mushroom.grow()
     *   }
     *   
     *   // other methods like makePic omitted from this example
     * }
     * 
     * }}}
     */
    def onTick(): Unit = unimplementedDefaultHandler  
    
    /**
     * Allows the view to react to a mouse move. The unimplemented method does not
     * change the model. To react to mouse movement events, please override this method.
     * 
     * @param position The mouse position
     */
    def onMouseMove(position: Pos)  : Unit = unimplementedDefaultHandler

    /**
     * Allows the view to react to a mouse drag. The unimplemented method does not
     * change the model. To react to mouse drag events, please override this method.
     * 
     * @param position The mouse position during the drag
     */
    def onMouseDrag(position: Pos)  : Unit = unimplementedDefaultHandler
    
    /**
     * Allows the view to react to rolling of the mouse wheel. The unimplemented method does not
     * change the model. To react to such events, please override this method.
     * 
     * @param rotation The change in mouse wheel position.
     */
    def onWheel    (rotation: Int)  : Unit = unimplementedDefaultHandler
    
    /**
     * Allows the view to react to a mouse click. The unimplemented method does not
     * change the model. To react to mouse click events, please override this method.
     * 
     * @param position The coordinates clicked
     */
    def onClick    (position: Pos)  : Unit = unimplementedDefaultHandler

    /**
     * Allows the view to react to the mouse button being pressed. Note that every mouse click
     * consists of both mouse down and mouse up events. The unimplemented method does not
     * change the model. To react to mouse button events, please override this method.
     * 
     * @param position The coordinates where the mouse was pressed
     */
    def onMouseDown(position: Pos)  : Unit = unimplementedDefaultHandler

    /**
     * Allows the view to react to the mouse button being released. Note that every mouse click
     * consists of both mouse down and mouse up events. The unimplemented method does not
     * change the model. To react to mouse button events, please override this method.
     * 
     * @param position The coordinates where the mouse was released
     */
    def onMouseUp  (position: Pos)  : Unit = unimplementedDefaultHandler
    
    /**
     * Allows the view to react to a keyboard button being pressed. Note that key press
     * consists of both key down and key up events. The unimplemented method does not
     * change the model. To react to keyboard events, please override this method.
     * 
     * @param key The button pressed
     */
    def onKeyDown  (key: Key)       : Unit = unimplementedDefaultHandler

    /**
     * Allows the view to react to a keyboard button being released. Note that key press
     * consists of both key down and key up events. The unimplemented method does not
     * change the model. To react to keyboard events, please override this method.
     * 
     * @param key The button released
     */
    def onKeyUp    (key: Key)       : Unit = unimplementedDefaultHandler

    /**
     * Allows the view to react to a keyboard button being pressed and released. 
     * The unimplemented method does not
     * change the model. To react to keyboard events, please override this method.
     * 
     * @param key The keyboard character typed
     */
    def onType     (character: Char): Unit = unimplementedDefaultHandler
    
    /**
     * Allows the view to react to a mouse move. The unimplemented method does not
     * change the model. To react to mouse movement events, please override this method.
     * 
     * @param event The mouse move event
     */
    def onMouseMove (event: MouseMoved     ): Unit = unimplementedDefaultHandler

    /**
     * Allows the view to react to a mouse drag. The unimplemented method does not
     * change the model. To react to mouse movement events, please override this method.
     * 
     * @param event The mouse drag event
     */
    def onMouseDrag (event: MouseDragged   ): Unit = unimplementedDefaultHandler
    
    /**
     * Allows the view to react to the mouse cursor entering the visual area of this View.  The unimplemented method does not
     * change the model. To react to mouse enter events, please override this method.
     * 
     * @param event The mouse enter event
     */    
    def onMouseEnter(event: MouseEntered   ): Unit = unimplementedDefaultHandler

    /**
     * Allows the view to react to the mouse cursor exiting the visual area of this View.  The unimplemented method does not
     * change the model. To react to mouse exit events, please override this method.
     * 
     * @param event The mouse exit event
     */    
    def onMouseExit (event: MouseExited    ): Unit = unimplementedDefaultHandler

    /**
     * Allows the view to react to a mouse button being released. The unimplemented method does not
     * change the model. To react to mouse button release events, please override this method.
     * 
     * @param event The mouse button release event
     */
    def onMouseUp   (event: MouseReleased  ): Unit = unimplementedDefaultHandler

    /**
     * Allows the view to react to a mouse button being pressed. The unimplemented method does not
     * change the model. To react to mouse button press events, please override this method.
     * 
     * @param event The mouse button press event
     */

    def onMouseDown (event: MousePressed   ): Unit = unimplementedDefaultHandler
    
    /**
     * Allows the view to react to the mouse wheel being rolled. The unimplemented method does not
     * change the model. To react to mouse wheel events, please override this method.
     * 
     * @param event The mouse wheel event
     */

    def onWheel     (event: MouseWheelMoved): Unit = unimplementedDefaultHandler

    /**
     * Allows the view to react to a mouse click. The unimplemented method does not
     * change the model. To react to mouse click events, please override this method.
     * 
     * @param event The mouse click event
     */
    def onClick     (event: MouseClicked   ): Unit = unimplementedDefaultHandler

    /**
     * Allows the view to react to a keyboard button being pressed. Note that key press
     * consists of both key down and key up events. The unimplemented method does not
     * change the model. To react to keyboard events, please override this method.
     * 
     * @param event The key press event
     */
    def onKeyDown   (event: KeyPressed     ): Unit = unimplementedDefaultHandler

    /**
     * Allows the view to react to a keyboard button being released. Note that key press
     * consists of both key down and key up events. The unimplemented method does not
     * change the model. To react to keyboard events, please override this method.
     * 
     * @param event The key release event
     */
    def onKeyUp     (event: KeyReleased    ): Unit = unimplementedDefaultHandler

    /**
     * Allows the view to react to a keyboard button being pressed and released. 
     * The unimplemented method does not
     * change the model. To react to keyboard events, please override this method.
     * 
     * @param event The keyboard character type event
     */
    def onType      (event: KeyTyped       ): Unit = unimplementedDefaultHandler
    
    protected lazy val display = new Display {   
      protected def attemptRender(stateToDraw: Model) = {
        this.render(stateToDraw)
      }
    }
  }
}


