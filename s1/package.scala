import s1.world.Bounds


////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it's not necessary    
// that you understand or even look at the code in this file. 
//////////////////////////////////////////////////////////////


// XXX document package

package object s1 {

  // CONVENIENT ACCESS TO BUFFER
  
  type Buffer[Element] = scala.collection.mutable.Buffer[Element]
  val Buffer = scala.collection.mutable.Buffer
  
  
  // SOUND
  
  
  type Sound = s1.sound.sampled.Sound
  val  Sound = s1.sound.sampled.Sound
  
  def playRecording(id: String, volume: Float = 0, repeats: Int = 0) = {
    s1.sound.sampled.playRecording(id, volume, repeats)
  }
  def loadRecording(id: String, volume: Float = 0) = s1.sound.sampled.loadRecording(id, volume) 


  // GRAPHICS

  // XXX TEMP; DUPLICATED IN o1.gui FOR NOW
  import scala.language.implicitConversions
  implicit def picToConvenient(pic: Pic) = new s1.gui.ConvenientBitmap(pic) // XXX pwa
  implicit def colorToConvenient(color: Color) = new s1.gui.ConvenientColor(color)
  
  type Pic = s1.gui.Pic
  val  Pic = s1.gui.Pic
  
  type Color = s1.gui.Color
  val  Color = s1.gui.Color
  
  //type Animation = s1.gui.Animation
  //val  Animation = s1.gui.Animation

  type Anchor      = s1.gui.Anchor
  val TopLeft      = s1.gui.Anchor.TopLeft
  val TopCenter    = s1.gui.Anchor.TopCenter
  val TopRight     = s1.gui.Anchor.TopRight
  val CenterLeft   = s1.gui.Anchor.CenterLeft
  val Center       = s1.gui.Anchor.Center
  val CenterRight  = s1.gui.Anchor.CenterRight
  val BottomLeft   = s1.gui.Anchor.BottomLeft
  val BottomCenter = s1.gui.Anchor.BottomCenter
  val BottomRight  = s1.gui.Anchor.BottomRight
  
  
  def näytä(kuva: Pic) = { // Finnish version of "show", below.
    kuva.show()
  }
  
  def show(pic: Pic) = { // English version of "näytä", above.
    pic.show()
  }
  
  /*def animoi(kuvat: Iterable[Pic], kuviaSekunnissa: Double) = { // Finnish version of "animate", below.
    Animation.show(frames = kuvat, frameRate = kuviaSekunnissa)
  }
  def animoiFunktiolla(kuvanluontifunktio: Int => Pic, kuvienLukumäärä: Int, kuviaSekunnissa: Double) = { 
    Animation.generate(kuvanluontifunktio, kuvienLukumäärä)
  }
  
  def animate(pics: Iterable[Pic], picsPerSecond: Double) = { // English version of "animoi", above.
    Animation.show(frames = pics, frameRate = picsPerSecond)
  }
  def animateUsingFunction(picGeneratingFunction: Int => Pic, numberOfPics: Int, picsPerSecond: Double) = { 
    Animation.generate(picGeneratingFunction, numberOfPics, picsPerSecond)
  }*/
  
  type View[Model <: AnyRef] = mutable.View[Model]
  object immutable {
    type View[Model <: Any] = s1.gui.immutable.View[Model]
  }
  object mutable {
    type View[Model <: AnyRef] = s1.gui.mutable.View[Model]
  }
  

  type Key = s1.gui.Key
  val  Key = s1.gui.Key
  
  type Horizontal = s1.gui.Horizontal
  val  Horizontal = s1.gui.Horizontal
  type Vertical = s1.gui.Vertical
  val  Vertical = s1.gui.Vertical 

  def rectangle(width: Int, height: Int, color: Color, anchor: Anchor = Center) = s1.gui.rectangle(width, height, color, anchor)
  def rectangle(bounds: Bounds, color: Color) = s1.gui.rectangle(bounds, color)
  def circle(diameter: Int, color: Color, backgroundColor: Color = Transparent, anchor: Anchor = Center) = s1.gui.circle(diameter, color, backgroundColor, anchor)
  def emptyCanvas(width: Int, height: Int, color: Color = White) = s1.gui.emptyCanvas(width, height, color)
  def line(from: Pos, to: Pos, color: Color, backgroundColor: Color = Transparent, anchor: Anchor = Center) = s1.gui.line(from, to, color, backgroundColor, anchor)
  
  // XXX add
  val Black      = s1.gui.Black
  val Green      = s1.gui.Green
  val Blue       = s1.gui.Blue
  val LightBlue  = s1.gui.LightBlue
  val Gray       = s1.gui.Gray
  val LightGray  = s1.gui.LightGray
  val Pink       = s1.gui.Pink
  val White      = s1.gui.White
  val Red        = s1.gui.Red
  val Purple     = s1.gui.Purple
  val Yellow     = s1.gui.Yellow
  val Brown      = s1.gui.Brown
  val SandyBrown = s1.gui.SandyBrown
  
  val Transparent = s1.gui.Transparent
  
  type MouseMoved      = s1.gui.event.MouseMoved
  val  MouseMoved      = s1.gui.event.MouseMoved
  type MouseDragged    = s1.gui.event.MouseDragged
  val  MouseDragged    = s1.gui.event.MouseDragged
  type MouseExited     = s1.gui.event.MouseExited
  val  MouseExited     = s1.gui.event.MouseExited
  type MouseEntered    = s1.gui.event.MouseEntered
  val  MouseEntered    = s1.gui.event.MouseEntered
  type MouseWheelMoved = s1.gui.event.MouseWheelMoved
  val  MouseWheelMoved = s1.gui.event.MouseWheelMoved
  type MouseReleased   = s1.gui.event.MouseReleased
  val  MouseReleased   = s1.gui.event.MouseReleased
  type MousePressed    = s1.gui.event.MousePressed
  val  MousePressed    = s1.gui.event.MousePressed
  type MouseClicked    = s1.gui.event.MouseClicked
  val  MouseClicked    = s1.gui.event.MouseClicked
  type KeyPressed      = s1.gui.event.KeyPressed
  val  KeyPressed      = s1.gui.event.KeyPressed
  type KeyReleased     = s1.gui.event.KeyReleased
  val  KeyReleased     = s1.gui.event.KeyReleased
  type KeyTyped        = s1.gui.event.KeyTyped
  val  KeyTyped        = s1.gui.event.KeyTyped
  type InputEvent      = s1.gui.event.InputEvent
  type KeyEvent        = s1.gui.event.KeyEvent
  implicit def eventToConvenient(event: InputEvent) = new s1.gui.event.ConvenientInputEvent(event)
  
  
  // WORLD
  
  type Pos = s1.world.Pos
  val  Pos = s1.world.Pos
  
  type Bounds = s1.world.Bounds
  val  Bounds = s1.world.Bounds
  
  type Velocity = s1.world.Velocity
  val  Velocity = s1.world.Velocity
  
  type Direction = s1.world.Direction
  val  Direction = s1.world.Direction
  type HasVelocity   = s1.world.HasVelocity
  type HasPosition   = s1.world.HasPosition
  type HasSize       = s1.world.HasSize
  type HasAnchor     = s1.world.HasAnchor
  type HasEdges      = s1.world.HasEdges

}


