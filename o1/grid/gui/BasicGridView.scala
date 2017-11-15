package o1.grid.gui

import scala.swing._
import o1.grid.Grid
import scala.swing.Swing._
import o1.grid.Coords
import javax.swing.Icon
import scala.swing.event._
import java.awt.Color
import java.awt.image.BufferedImage

////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it's not necessary    
// that you understand or even look at the code in this file.
//////////////////////////////////////////////////////////////

/** A `BasicGridView` object is a graphics component that displays a rectangular grid 
  * as defined by a `Grid` object, and provides mouse controls for manipulating the grid.
  *
  * '''NOTE TO STUDENTS: In this course, you don't need to understand
  * how this class works or can be used.'''
  *
  * @param grid           the grid to be displayed
  * @param maxSquareSize  the maximum width of a single square in the grid, in pixels. The `BasiGridView` object
  *                       scales itself to use most of the available screen space, but won't go over this limit */
abstract class BasicGridView[GridType <: Grid[Element], Element](val grid: GridType, maxSquareSize: Int) extends Component {
  view =>

  val popup: Popup 
  val width = grid.width
  val height = grid.height
  val squareSize = this.determineSquareSize(maxSquareSize)    
  private val cells = this.createCells

  this.border = CompoundBorder(EmptyBorder(10, 10, 10, 10), EtchedBorder)
  this.preferredSize = new Dimension(squareSize * width, squareSize * height)

  this.listenTo(this.mouse.clicks)
  this.listenTo(this.mouse.moves)
  
  this.reactions += {
    case MouseReleased(_, point, modifiers, _, triggersPopup) => 
      if (triggersPopup) {
        popup.show(view, point.x, point.y)
      } else if ((modifiers & Key.Modifier.Meta) == 0) {
        for (element <- this.elementAt(point)) {
          this.elementClicked(element)
          this.update()
        }
      } 
    case MousePressed(_, point, _, _, triggersPopup) =>
      if (triggersPopup) {
        popup.show(view, point.x, point.y)
      } 
    case MouseMoved(_, point, _) =>
      this.tooltip = this.elementAt(point).flatMap( this.tooltipFor(_) ).orNull
  }
  
  
  
  
  def elementClicked(elemement: Element)
  

  private def determineSquareSize(upperLimit: Int) = {
    val screenSize = this.toolkit.getScreenSize
    val widthMax = (screenSize.width - 175) / this.width
    val heightMax = (screenSize.height - 175) / this.height
    widthMax.min(heightMax).min(upperLimit).max(1)
  }
    
  
  private def createCells = Array.tabulate(height, width) { new Cell(_, _) }

  
  override def paint(g: Graphics2D) = {
    for (row <- this.cells; cell <- row) {  
      if (g.getClip.intersects(cell.bounds)) {
        cell.render(g)
      }
    }
  }

  
  def update() = {
    for (row <- this.cells; cell <- row) {
      if (cell.updatePics()) {
        this.repaint(cell.bounds.x, cell.bounds.y, cell.bounds.width, cell.bounds.height)    
      }
    }
  }
  
  
  // N.B. null values allowed within return array
  def elementVisuals(element: Element): Array[BufferedImage] // N.B. null values allowed within return array
  def missingElementVisuals: Array[BufferedImage]            // N.B. null values allowed within return array
  
  
  
  def elementAt(point: Point) = this.coordsAt(point).map( this.grid(_) )
   
  
  def coordsAt(point: Point) = {
    val coords = new Coords(point.x / this.squareSize, point.y / this.squareSize)
    if (this.grid.contains(coords)) Some(coords) else None
  }
  
  
  def tooltipFor(element: Element): Option[String] = None


  def scale(original: BufferedImage): BufferedImage = this.scale(original, this.squareSize)
  
  
  def scale(original: BufferedImage, targetSize: Int): BufferedImage = {

    def nextStep(dim: Int, target: Int) = target.max(dim / 2) 
    
    import java.awt.image.BufferedImage._
    import java.awt.RenderingHints._
    import java.awt.Transparency._
    val width = nextStep(original.getWidth, targetSize)
    val height = nextStep(original.getHeight, targetSize)
    val scaled = new BufferedImage(width, height, if (original.getTransparency == OPAQUE) TYPE_INT_RGB else TYPE_INT_ARGB)
    val tempGraphics = scaled.createGraphics()
    tempGraphics.setRenderingHint(KEY_INTERPOLATION, VALUE_INTERPOLATION_BICUBIC) 
    tempGraphics.drawImage(original, 0, 0, width, height, null)
    tempGraphics.dispose()
    if (width == targetSize && height == targetSize) scaled else this.scale(scaled, targetSize)
    
  }
  
  
  private class Cell(row: Int, column: Int)  {

    val coords = new Coords(column, row)
    val bounds = new Rectangle(view.squareSize * coords.x, view.squareSize * coords.y, view.squareSize, view.squareSize)  
    private var pics = Array[BufferedImage]() 
    
    def element = Option(view.grid.elementAt(coords))

    def updatePics() = {
      val newPics = this.element.map(view.elementVisuals).getOrElse(view.missingElementVisuals)
      if (newPics.deep != this.pics.deep) { 
        this.pics = newPics 
        true
      } else {
        false
      }
    }
    
    def render(gridGraphics: Graphics2D) = {
      for (pic <- this.pics; if pic != null) {
        gridGraphics.drawImage(pic, 
                               this.bounds.x + ((this.bounds.width  - pic.getWidth)  / 2), 
                               this.bounds.y + ((this.bounds.height - pic.getHeight) / 2), 
                               null)
      }
    }
   
  }
  


  protected class Popup extends PopupMenu {

    protected var chosenCoords: Option[Coords] = None

    abstract class PopupAction(name: String) extends MenuItem(name) {
      this.action = Action(name) {
        for (c <- chosenCoords) { 
          PopupAction.this.apply(c)
          view.update() 
        } 
      }
      
      def isApplicable(coords: Coords): Boolean
      
      def apply(coords: Coords)
      
    }
    
    object PopupAction {
      val AlwaysApplicable = (a: Any) => true
    }
    
    class CoordsAction(name: String, val applies: Coords => Boolean)(val perform: Coords => Unit) extends PopupAction(name) {
      override def isApplicable(coords: Coords) = this.applies(coords)
      def apply(coords: Coords) = {
        this.perform(coords)
      }
    }
    
    class ElementAction(name: String, val applies: Element => Boolean)(val perform: Element => Unit) extends PopupAction(name) {
      override def isApplicable(coords: Coords) = this.applies(view.grid(coords))
      def apply(coords: Coords) = {
        this.perform(view.grid(coords))
      }
    }
    
    
    def +=(menuItem: Component) = {
      this.contents += menuItem
    }
    
    def ++=(menuActions: Seq[Component]) = {
      this.contents ++= menuActions
    }
    
    def chooseEnabled() = {  
      for (here <- this.chosenCoords; elem <- this.contents) {
        elem match {
          case popupAction: PopupAction =>
            elem.enabled = popupAction.isApplicable(here)
          case otherwise => // nothing 
        }        
      }
    }
    
    override def show(component: Component, x: Int, y: Int) = {
      this.chosenCoords = coordsAt(new Point(x, y))
      this.chooseEnabled()
      super.show(component, x, y)
    }

  }
  
  
}

