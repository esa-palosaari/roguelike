
package s1.gui

import scala.swing._
import s1.gui.event._
import s1.util._
import s1.sound.sampled.Sound
import View.{RefreshPolicy,TicksPerSecondMax,TicksPerSecondDefault}
import AbstractView._ 
import AbstractView.Events._ 
import com.typesafe.config.{Config, ConfigFactory}
import akka.actor.{Actor, ActorSystem, ActorRef, Props}
import scala.concurrent.ExecutionContext.Implicits.global


private[gui] object AbstractView {
  
  object Events {
    import akka.dispatch._
    import akka.util.StablePriorityBlockingQueue

    object Message {
      type Kind = String
    }
    sealed abstract class Message {
      val start = System.currentTimeMillis() 
      def delay = System.currentTimeMillis() - this.start
      def isDelayed = this.delay > 500
      def isBadlyDelayed = this.delay > 3000
    }
    case class Tick() extends Message 
    case class GUIMessage(val event: InputEvent) extends Message
    
    object Mailbox {
      val BacklogSizeThreshold = TicksPerSecondDefault * 100
      val Priorities = PriorityGenerator { 
        case tick: Tick => 100
        case guiMessage => 1
      }
  
      class Queue extends QueueBasedMessageQueue {
        final val queue = new StablePriorityBlockingQueue(100, Priorities)
        private var warnedAboutMessageLagAlready = false
        
        def dequeue(): Envelope = if (this.hasMessages) this.queue.remove() else null
        
        def enqueue(receiver: ActorRef, envelope: Envelope): Unit = envelope.message match {
          case Tick()          => this.addTickUnlessSwamped(envelope)
          case anyOtherMessage => this.queue.add(envelope)
        }
         
        private def addTickUnlessSwamped(envelope: Envelope) = {
          if (this.queue.size() < Mailbox.BacklogSizeThreshold) {
            this.queue.add(envelope)
          } else {
            if (!this.warnedAboutMessageLagAlready) { // later: send to DeadLetters instead?
              warn("Failing to compute states fast enough. Discarding some clock ticks.")    
              this.warnedAboutMessageLagAlready = true
            } 
          }
        }
      }
    
    }
    
    case class Mailbox() extends MailboxType {
      def this(settings: ActorSystem.Settings, config: Config) = this()
      final override def create(owner: Option[ActorRef], system: Option[ActorSystem]) = new Mailbox.Queue
    }
  }

  object NoHandlerDefined extends Throwable 
  def unimplementedDefaultHandler: Nothing = throw NoHandlerDefined

  def warn(message: Any) = {
    System.err.println("o1.gui warning: " + message)
  }
}


abstract class AbstractView[Model](private val initialState: Model,
                                   private val tickRate: Double,
                                   protected val title: String,
                                   private val initialDelay: Int, 
                                   val terminateOnClose: Boolean, 
                                   val closeWhenDone: Boolean,
                                   val refreshPolicy: RefreshPolicy) { 
  view =>

    
  def onStop() = { }
  def onClose() = { }

  protected def makePic(state: Model): Pic
  protected def isDone(state: Model) = false 
  protected def isPaused(state: Model) = false
  protected def sound(state: Model): Option[Sound] = None

  private[s1] def onTick(previousState: Model): Model
  
  // Simple GUI handlers:
  private[s1] def onMouseMove(state: Model, position: Pos)  : Model
  private[s1] def onMouseDrag(state: Model, position: Pos)  : Model
  private[s1] def onMouseDown(state: Model, position: Pos)  : Model 
  private[s1] def onMouseUp  (state: Model, position: Pos)  : Model 
  private[s1] def onWheel    (state: Model, rotation: Int)  : Model
  private[s1] def onClick    (state: Model, position: Pos)  : Model
  private[s1] def onKeyDown  (state: Model, key: Key)       : Model
  private[s1] def onKeyUp    (state: Model, key: Key)       : Model
  private[s1] def onType     (state: Model, character: Char): Model

  // Full GUI handlers:
  private[s1] def onMouseMove (state: Model, event: MouseMoved     ): Model
  private[s1] def onMouseDrag (state: Model, event: MouseDragged   ): Model
  private[s1] def onMouseEnter(state: Model, event: MouseEntered   ): Model
  private[s1] def onMouseExit (state: Model, event: MouseExited    ): Model
  private[s1] def onMouseUp   (state: Model, event: MouseReleased  ): Model
  private[s1] def onMouseDown (state: Model, event: MousePressed   ): Model
  private[s1] def onWheel     (state: Model, event: MouseWheelMoved): Model
  private[s1] def onClick     (state: Model, event: MouseClicked   ): Model
  private[s1] def onKeyDown   (state: Model, event: KeyPressed     ): Model
  private[s1] def onKeyUp     (state: Model, event: KeyReleased    ): Model
  private[s1] def onType      (state: Model, event: KeyTyped       ): Model

  
  private val eventSystem = ActorSystem("ViewEventSystem", ConfigFactory.load("lib/akka.conf"))
  private lazy val events = eventSystem.actorOf(Props(new ModelState(initialState)).withDispatcher("view-mailbox"), name = "modelstate")
  protected def display: Display
  private lazy val ticker = {
    import s1.util.ConvenientDouble // prevents unnecessary warning from Scala IDE 
    val tickDelayDouble = 1000 / (this.tickRate atMost TicksPerSecondMax)
    val (init, between) = (ms(this.initialDelay), ms(tickDelayDouble.toInt))
    if (tickDelayDouble.isInfinity) None else Some(new Ticker(init, between)( view.events ! new Tick ))
  }
  private val frame = new Frame {
    contents  = view.display
    resizable = false
    title     = view.title
    location  = new Point(200, 200)
    override def closeOperation = { view.onFrameClose() }
  }

  /**
   * Tells whether the view is visible or not
   */
  def visible = this.frame.visible

  /**
   * Sets the view visible
   */
  def start() = {
    this.visible = true
  }

  /**
   * Sets the views visibility. One first time a view is displayed its
   * ticking system is started.
   */
  def visible_=(visibility: Boolean) = {
    if (visibility && !this.ticker.exists( _.isRunning )) {
      this.display.loadModel(this.initialState)
      this.frame.pack()
      this.frame.visible = true
      this.ticker.foreach( _.start() )
    } else {
      this.frame.visible = visibility
    }
  }

  private def onFrameClose() = {
    this.stop()
    this.onClose()
    if (this.terminateOnClose && !Program.isRunningInScalaREPL) {
      System.exit(0)
    }
  }
  
  private def onDone() = {
    if (this.closeWhenDone) {
      this.close()
    } else {
      this.stop()
    }
  }
  
  /**
   * Closes the view window
   */
  def close() = {
    this.frame.closeOperation()
    this.frame.close()
  }
  
  /**
   * Stops the view from ticking the model forward, but does not close the window
   */
  def stop() = {
    this.ticker.foreach( _.stop() )
    this.eventSystem.terminate()
    this.onStop()
  }

  protected abstract class Display extends Panel {
    display =>
    
    private case class Latest(val state: Model, val image: Option[java.awt.image.BufferedImage]) { // XXX Pic
      val timestamp = System.currentTimeMillis
    }

    private var latestComputed = Latest(initialState, None)

    this.listenTo(this.mouse.clicks)
    this.listenTo(this.mouse.moves)
    this.listenTo(this.mouse.wheel)
    this.listenTo(this.keys)
    this.reactions += {
      case KeyPressed(_, Key.Escape, _, _) => view.close()
      case event: InputEvent               => view.events ! GUIMessage(event)                                 
    }
  
    // XXX http://stackoverflow.com/questions/1736828/how-to-stop-repeated-keypressed-keyreleased-events-in-swing

    override def paintComponent(myGraphics: Graphics2D) = {
      for (image <- latestComputed.image) {
        myGraphics.drawImage(image, 0, 0, null)
        // XXX myGraphics.drawImage(pic.toRenderedRepresentation.awtBufferedImage, 0, 0, null)
      }
    }
    
    private[AbstractView] def loadModel(initialState: Model) = {
      val initialPic = try { PicWithAnchor(view.makePic(initialState), Anchor.Center) } catch { case View.NothingToDraw => rectangle(150, 150, Black) } // XXX pwa 
      this.preferredSize = initialPic.dimensions
      this.latestComputed = Latest(initialState, Some(initialPic.toImage))
      this.requestFocusInWindow()
    }
    private[AbstractView] def renderIfAppropriate(currentState: Model) = {
      if (view.refreshPolicy.shouldRefresh(this.latestComputed.state, currentState)) { 
        this.attemptRender(currentState)  
      } else {
        // no change; nothing to do
      }
    }
    
    protected def attemptRender(stateToDraw: Model): Unit
    
    protected def render(stateToDraw: Model): Unit = {
      Try(PicWithAnchor(view.makePic(stateToDraw), Anchor.Center).toImage) match { // XXX pwa
        case Success(newPic) => 
          this.latestComputed = Latest(stateToDraw, Some(newPic))
          this.repaint()
        case Failure(View.NothingToDraw) => 
          // keep earlier image
        case Failure(unexpectedProblem) =>
          throw unexpectedProblem
      }
    }
  }

  private class ModelState(initialState: Model) extends Actor {
    private var isActive = true
    private var state = initialState
   
    def receive = {
      case message: Message => if (this.isActive) { this.handleIfHandlerEnabled(message) } 
      case unexpected       => warn("Unexpected event: " + unexpected) 
    }

    private def handleIfHandlerEnabled(message: Message) = {
      val enabledHandlers = this.handlersFor(message).filter( _.isEnabled ) 

      def applyHandlers() = {
        val handlersForMessage = enabledHandlers.map( (h: Handler) => (oldState: Model) => h(message, oldState) ) 
        this.state = handlersForMessage.reduce( _ compose _ )(this.state)
        view.sound(this.state).foreach( _.play() )
        if (view.isDone(this.state)) {
          this.isActive = false
          view.onDone()
        }
        display.renderIfAppropriate(this.state) 
      }
      
      if (enabledHandlers.nonEmpty) {
        message match {
          case tick: Tick => 
            if (!view.isPaused(this.state)) { 
              applyHandlers() 
            }
          case message: GUIMessage => 
            this.checkForGUIDelay(message)
            applyHandlers()
        }
      }
    }
    
    private def checkForGUIDelay(message: GUIMessage) = {
      if (message.isDelayed) {
        val description = message.getClass 
        warn(s"Response to GUI event ($description) lagging behind.")
      }
    }
    
    private case class Handler(underlying: PartialFunction[Message, Model => Model]) extends Function2[Message, Model, Model] {
      private var hasDefaulted = false
      
      def apply(message: Message, oldState: Model) = {
        if (this.underlying.isDefinedAt(message)) {
          val callClientMethod = this.underlying(message)
          Try(callClientMethod(oldState)) match { 
            case Success(newState)          => newState  
            case Failure(NoHandlerDefined)  => this.hasDefaulted = true; oldState 
            case Failure(crashInClientCode) => crashInClientCode.printStackTrace(); oldState         
          }
        } else {
          warn("Unexpected failure while handling message: " + message)
          oldState
        }
      }
      def isEnabled = !this.hasDefaulted
    }
    
    private def handlersFor(message: Message): Seq[Handler] = message match {
      case Tick()                         => Seq(SimpleHandlers.tick)
      case GUIMessage(_: MouseEntered)    => Seq(FullHandlers.mouseEnter)
      case GUIMessage(_: MouseExited)     => Seq(FullHandlers.mouseExit)
      case GUIMessage(_: MouseReleased)   => Seq(SimpleHandlers.mouseUp, FullHandlers.mouseUp)
      case GUIMessage(_: MousePressed)    => Seq(SimpleHandlers.mouseDown, FullHandlers.mouseDown)
      case GUIMessage(_: MouseMoved)      => Seq(SimpleHandlers.mouseMove, FullHandlers.mouseMove)
      case GUIMessage(_: MouseDragged)    => Seq(SimpleHandlers.mouseDrag, FullHandlers.mouseDrag)
      case GUIMessage(_: MouseWheelMoved) => Seq(SimpleHandlers.wheel,     FullHandlers.wheel)
      case GUIMessage(_: MouseClicked)    => Seq(SimpleHandlers.click,     FullHandlers.click)
      case GUIMessage(_: KeyPressed)      => Seq(SimpleHandlers.keyDown,   FullHandlers.keyDown)
      case GUIMessage(_: KeyReleased)     => Seq(SimpleHandlers.keyUp,     FullHandlers.keyUp)
      case GUIMessage(_: KeyTyped)        => Seq(SimpleHandlers.typed,     FullHandlers.typed)
      case GUIMessage(unexpected: InputEvent) => warn("No valid handlers for event " + unexpected); Seq.empty 
    }
    private[this] object SimpleHandlers {
      val tick      = Handler({ case Tick() => (s: Model) => view.onTick(s) })
      val mouseMove = Handler({ case GUIMessage(event: MouseMoved)     => (s: Model) => view.onMouseMove(s, Pos(event.point)) })
      val mouseDrag = Handler({ case GUIMessage(event: MouseDragged)   => (s: Model) => view.onMouseDrag(s, Pos(event.point)) })
      val mouseDown = Handler({ case GUIMessage(event: MousePressed)   => (s: Model) => view.onMouseDown(s, Pos(event.point)) })
      val mouseUp   = Handler({ case GUIMessage(event: MouseReleased)  => (s: Model) => view.onMouseUp  (s, Pos(event.point)) })
      val wheel     = Handler({ case GUIMessage(event: MouseWheelMoved)=> (s: Model) => view.onWheel    (s, event.rotation) })
      val click     = Handler({ case GUIMessage(event: MouseClicked)   => (s: Model) => view.onClick    (s, Pos(event.point)) })
      val keyDown   = Handler({ case GUIMessage(event: KeyPressed)     => (s: Model) => view.onKeyDown  (s, event.key) })
      val keyUp     = Handler({ case GUIMessage(event: KeyReleased)    => (s: Model) => view.onKeyUp    (s, event.key) })
      val typed     = Handler({ case GUIMessage(event: KeyTyped)       => (s: Model) => view.onType     (s, event.char) })
    }
    private[this] object FullHandlers {
      val mouseEnter = Handler({ case GUIMessage(event: MouseEntered)    => (s: Model) => view.onMouseEnter(s, event) })
      val mouseExit  = Handler({ case GUIMessage(event: MouseExited)     => (s: Model) => view.onMouseExit (s, event) })
      val mouseUp    = Handler({ case GUIMessage(event: MouseReleased)   => (s: Model) => view.onMouseUp   (s, event) })
      val mouseDown  = Handler({ case GUIMessage(event: MousePressed)    => (s: Model) => view.onMouseDown (s, event) })
      val mouseMove  = Handler({ case GUIMessage(event: MouseMoved)      => (s: Model) => view.onMouseMove (s, event) })
      val mouseDrag  = Handler({ case GUIMessage(event: MouseDragged)    => (s: Model) => view.onMouseDrag (s, event) })
      val wheel      = Handler({ case GUIMessage(event: MouseWheelMoved) => (s: Model) => view.onWheel     (s, event) })
      val click      = Handler({ case GUIMessage(event: MouseClicked)    => (s: Model) => view.onClick     (s, event) })
      val keyDown    = Handler({ case GUIMessage(event: KeyPressed)      => (s: Model) => view.onKeyDown   (s, event) })
      val keyUp      = Handler({ case GUIMessage(event: KeyReleased)     => (s: Model) => view.onKeyUp     (s, event) })
      val typed      = Handler({ case GUIMessage(event: KeyTyped)        => (s: Model) => view.onType      (s, event) })
    }
  }
   
}

