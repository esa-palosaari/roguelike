
////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it's not necessary    
// that you understand or even look at the code in this file. 
//////////////////////////////////////////////////////////////

package s1.gui


import s1.gui.event._
import s1.util.localURL
import s1.util.ConvenientDouble
import javax.swing.ImageIcon
import javax.imageio.ImageIO
import java.awt.{Dimension, Point}
import aalto.smcl.colors._
import scala.language.reflectiveCalls
import scala.collection._
import javax.swing.ToolTipManager
import s1.world.Anchor._


class ConvenientBitmap(val self: PicWithAnchor) extends s1.world.HasAnchor {
  /**
   * The height of the picture in pixels 
   */
  def height = self.heightInPixels
  
  /**
   * The width of the picture in pixels
   */
  def width = self.widthInPixels
  
  /**
   * Returns the picture's anchor.
   */
  def anchor = self.anchor 

  /**
   * Picture's dimensions as a Dimension object
   */
  def dimensions = new java.awt.Dimension(self.width, self.height)
 
  /**
   * Converts the picture into a java.awt.BufferedImage
   */
  def toImage = self.toRenderedRepresentation.awtBufferedImage

  /**
   * Creates a copy of the picture with the given anchor point
   */
  def anchorAt(anchor: Anchor) = this.self.copy(anchor = anchor)
  
  
  /**
   * Scales the image 
   * 
   * @param factor scaling factor
   * @return a scaled copy of the image
   */
  def scaleBy(factor: Double): PicWithAnchor = PicWithAnchor(self.pic.scale(factor), self.anchor) // XXX pwa
  
  /**
   * 
   */
  private[s1] def absoluteAnchor = this.anchor.toAbsoluteWithin(self)
  
  
  /**
   * Scales the image to the desired size
   * 
   * @param targetWidth desired width
   * @param targetHeight desired height
   * @return a scaled copy of the image
   */
  def scaleTo(targetWidth: Double, targetHeight: Double): PicWithAnchor = {
    val scaled = self.pic.scale(targetWidth / self.width, targetHeight / self.height, resizeCanvasBasedOnTransformation = true) // XXX should use: java.awt.Image.SCALE_AREA_AVERAGING ?? 
    PicWithAnchor(scaled, self.anchor) // XXX pwa
  }

  /**
   * Displays the picture in a window
   */
  def show(background: Color = White, border: Int = 1) = {
    Pic.show(self, background, border)
  } 

  /**
   * Hides the picture window
   */
  def hide() = {
    Pic.hide(self)
  }

  /**
   * Crops the picture to the given dimensions.
   * 
   * @param bounds the bounds of what to keep
   */
  
  def cropToSizeOf(bounds: Bounds) = { 
    if (bounds.top<= 0 && bounds.left <=0 && bounds.contains(self.width-1, self.height-1))
      self  
    else {
      val cropped = self.crop(bounds.left, bounds.top, bounds.left + bounds.width -1, bounds.top + bounds.height - 1)  // XXX -1 ei kiva
      PicWithAnchor(cropped, Center)    // XXX pwa
    }
  }
  
  /**
   * Crops the picture to the given dimensions.
   * 
   * @param background another picture providing the size of the area to keep
   * @param from the top left corner of the area to keep
   */  
  
  def cropToSizeOf(background: Pic, from: Pos) = { 
    //if (self.dimensions == background.dimensions) 
    //  self  
    //else {
      val (cropX, cropY) = (from.xInt, from.yInt)
      val bgWithCroppedPicOnTop = self.crop(cropX, cropY, cropX + background.width - 1, cropY + background.height - 1)  // XXX -1 ei kiva
      PicWithAnchor(bgWithCroppedPicOnTop, background.anchor)    // XXX pwa
    //}
  }

  /**
   * Creates a new picture by placing this image onto another. The parameters my and at are used to position the image.
   * 
   * @param backPic the background image
   * @param my which anchor point is used for this image...
   * @param at ... and the coordinates where that anchor should be in the composite image. 
   */
  def onto(backPic: Pic, my: Anchor, at: Pos): Pic = { 
    val myTopLeftRelativeToBG = at - my.internalPosWithin(self)
    PicWithAnchor(self.overlayOn(backPic, myTopLeftRelativeToBG.xInt, myTopLeftRelativeToBG.yInt), backPic.anchor) // XXX pwa
  }
  
  /**
   * Creates a new picture by placing this image onto another. The parameters my and atIts are used to position the image.
   * 
   * @param backPic the background image
   * @param my which anchor point is used for this image...
   * @param atIts ... and the anchor point on the background where 'my' anchor should be in the composite image. 
   */  
  def onto(backPic: Pic, my: Anchor, atIts: Anchor): Pic = self.onto(backPic, my, atIts.internalPosWithin(backPic))

  /**
   * Creates a new picture by placing this image onto another. The parameter at is used to position the image.
   * Uses the Pic:s existing anchor.
   *
   * 
   * @param backPic the background image
   * @param at where the achor point of this image should be placed on the background image
   * @return composite image
   */  
  def onto(backPic: Pic, at: Pos): Pic                   = self.onto(backPic, self.anchor, at)

  /**
   * Creates a new picture by placing this image onto another. The parameter atIts is used to position the image.
   * Uses the Pic:s existing which is placed by default in the center of the background.
   * 
   * @param backPic the background image
   * @param at where the center of this image should be placed on the background image
   * @return composite image
   */  
  def onto(backPic: Pic, atIts: Anchor = Center): Pic    = self.onto(backPic, self.anchor, atIts)

  // XXX document here and elsewhere: at (etc.) are local coordinates within the background

  /**
   * Creates a new picture by placing this image onto another and crops the picture to the size of the background.
   * That is, everything outside the original background is cropped out.
   * The parameters my and at are used to position the image.
   * 
   * @param background the background image
   * @param my which anchor point is used for this image...
   * @param at ... and the coordinates where that anchor should be in the composite image. 
   * @return composite image
   */
  def against(background: Pic, my: Anchor, at: Pos): Pic = { // XXX wasteful, and creates potentially ridiculously-sized Pics as an intermediate result
    val uncroppedCombination = self.onto(background, my, at)
    val myLeftRelativeToBG = at.x - my.internalXWithin(self)
    val myTopRelativeToBG  = at.y - my.internalYWithin(self)
    val bgXWithinCombo = if (myLeftRelativeToBG < 0) -myLeftRelativeToBG else 0
    val bgYWithinCombo = if (myTopRelativeToBG  < 0) -myTopRelativeToBG  else 0
    uncroppedCombination.cropToSizeOf(background, Pos(bgXWithinCombo, bgYWithinCombo))
  }

  /**
   * Creates a new picture by placing this image onto another and crops the picture to the size of the background.
   * That is, everything outside the original background is cropped out.
   * The parameters my and atIts are used to position the image.
   * 
   * @param background the background image
   * @param my which anchor point is used for this image...
   * @param atIts ... and the anchor point on the background where 'my' anchor should be in the composite image. 
   * @return composite image
   */  

  def against(background: Pic, my: Anchor, atIts: Anchor): Pic = self.against(background, my, atIts.internalPosWithin(background))

  /**
   * Creates a new picture by placing this image onto another and crops the picture to the size of the background.
   * That is, everything outside the original background is cropped out.
   * The parameter at is used to position the image.
   * 
   * @param background the background image
   * @param at ... and the coordinates where that anchor should be in the composite image. 
   * @return composite image
   */


  def against(background: Pic, at: Pos): Pic                   = self.against(background, self.anchor, at)

  /**
   * Creates a new picture by placing this image onto another and crops the picture to the size of the background.
   * The parameter atIts is used to position the image. Uses the Pic:s existing which is placed by default in the
   * center of the background.
   * 
   *
   * @param background the background image
   * @param atIts ... and the anchor point on the background where 'my' anchor should be in the composite image. 
   * @return composite image
   */  
  def against(background: Pic, atIts: Anchor = Center): Pic    = self.against(background, self.anchor, atIts)

  /**
   * Creates a new picture by placing another image onto this one and cropping the picture to to its original size.
   * That is, everything outside the original picture is cropped out.
 
   * @param its which anchor point is used for the foreground image...
   * @param at the position on this image where 'its' anchor should be in the composite image. 
   * @return composite image
   */    

  def place(foreground: Pic, its: Anchor, at: Pos) = {
    val foregroundAnchor = its
    foreground.against(self, foregroundAnchor, at) 
  }
  
  /**
   * Creates a new picture by placing another image onto this one and cropping the picture to to its original size.
   * That is, everything outside the original picture is cropped out.
   * @param its which anchor point is used for the foreground image...
   * @param atMy ... and the anchor point on this image where 'its' anchor should be in the composite image. 
   * @return composite image
   */    
  def place(foreground: Pic, its: Anchor, atMy: Anchor): Pic = foreground.against(self, its, atMy.internalPosWithin(self)) 

  /**
   * Creates a new picture by placing another image onto this one and cropping the picture to to its original size.
   * That is, everything outside the original picture is cropped out.
   * The parameter at is used to position the image.
   * 
   * @param foreground the foreground image
   * @param at the coordinates where the anchor of the foreground image should be in the composite image. 
   * @return composite image
   */
  def place(foreground: Pic, at: Pos): Pic                   = self.place(foreground, foreground.anchor, at)  

  /**
   * Creates a new picture by placing another image onto this one and cropping the picture to to its original size.
   * That is, everything outside the original picture is cropped out.
   * The parameter atMy is used to position the image.
   * 
   * @param foreground the foreground image
   * @param atMy the anchor in this image where the anchor of the foreground image should be in the composite image. 
   * @return composite image
   */
  def place(foreground: Pic, atMy: Anchor): Pic              = self.place(foreground, foreground.anchor, atMy)

  /**
   * Creates a new picture by placing another image onto this one and cropping the picture to to its original size.
   * That is, everything outside the original picture is cropped out.
   * 
   * @param foregroundPicAndPos the foreground image and the position where to put it as a tuple
   * @return composite image
   */
  def place(foregroundPicAndPos: (Pic, Pos)): Pic         = self.place(foregroundPicAndPos._1, foregroundPicAndPos._2)  

  /**
   * Creates a new picture by placing a sequence of images onto this one and cropping the picture to to its original size.
   * That is, everything outside the original picture is cropped out.
   * 
   * @param foregroundPics the foreground images and the position where to put them as sequence of tuples
   * @return composite image
   */
  def place(foregroundPics: Traversable[(Pic, Pos)]): Pic = foregroundPics.foldLeft(self)( _.place(_) )  

  /**
   * Creates a new picture by placing a sequence of images onto this one and cropping the picture to to its original size.
   * That is, everything outside the original picture is cropped out. This is a variable argument list version.
   * 
   * @param foregroundPics the foreground images and the position where to put them as sequence of tuples
   * @return composite image
   */
  def place(foregroundPics: (Pic, Pos)*): Pic             = this.place(foregroundPics) 

    /**
   * Creates a new picture by placing an image multiple times onto this one and cropping the picture to to its original size.
   * That is, everything outside the original picture is cropped out.
   * 
   * @param foregroundPic the foreground image used
   * @param at a sequence of positions where to put the copies of the image
   * @return composite image
   */

  def placeCopies(foregroundPic: Pic, at: Seq[Pos]) = this.place(at.map( _ => foregroundPic) zip at) 

  
  
  
  
  // XXX allow retaining of original anchor (pohjustava osin toimiva esimerkki alla) and customization of anchoring
  def leftOf(rightPic: Pic) = PicWithAnchor(self.appendOnRight (rightPic)(Vertical.Middle,   paddingInPixels = 0, backgroundColor = Transparent), self.anchor) // XXX pwa 
  def rightOf(leftPic: Pic) = PicWithAnchor(self.appendOnLeft  (leftPic) (Vertical.Middle,   paddingInPixels = 0, backgroundColor = Transparent), self.anchor) // XXX pwa 
  def below(abovePic: Pic)  = PicWithAnchor(self.appendOnTop   (abovePic)(Horizontal.Center, paddingInPixels = 0, backgroundColor = Transparent), self.anchor) // XXX pwa 
  def above(lowerPic: Pic, retainAnchor: Boolean = false)  = {
    val smclPic = self.appendOnBottom(lowerPic)(Horizontal.Center, paddingInPixels = 0, backgroundColor = Transparent) 
    if (retainAnchor) PicWithAnchor(smclPic, self.absoluteAnchor) else PicWithAnchor(smclPic, self.anchor) // XXX pwa
  }
  def rowOf(number: Int)    = Seq.fill(number)(self).reduceLeft( (res, next) => PicWithAnchor(res.leftOf(next), self.anchor )) // XXX pwa ja voi laittaa alaviivalla
  def columnOf(number: Int) = Seq.fill(number)(self).reduceLeft( (res, next) => PicWithAnchor(res.above(next), self.anchor) )  // XXX pwa ja voi laittaa alaviivalla
      
  def rotate(degrees: Double = 90.0, background: Color = Transparent) = this.clockwise(degrees, background)  
  def clockwise(degrees: Double = 90.0, background: Color = Transparent) =  // XXX pwa // XXX should rotare around pin
    if (degrees == 90.0) PicWithAnchor(self.rotate90DegsCw(true, background), self.anchor) 
    else                 PicWithAnchor(self.rotateDegs(-degrees, true, background), self.anchor)
  def counterclockwise(degrees: Double = 90.0, background: Color = Transparent) = // XXX pwa // XXX should rotare around pin 
    if (degrees == 90.0) PicWithAnchor(self.rotate90DegsCcw(true, background), self.anchor) 
    else                 PicWithAnchor(self.rotateDegs(degrees, true, background), self.anchor)

  /**
   * Compares the width of this pic to another
   * @param the other image
   */
  def isWiderThan(another: Pic)  = self.width  > another.width
  /**
   * Compares the height of this pic to another
   * @param the other image
   */
  def isTallerThan(another: Pic) = self.height > another.height
  /**
   * Compares the size of this pic to another. If this image fits inside the other, returns true.
   * @param the other image
   */
  def fitsOnto(another: Pic) = !self.isWiderThan(another) && !self.isTallerThan(another)

  /**
   * Creates a java Icon from this Pic.
   * 
   * @return a javax.swing.Icon
   */
  
  def toIcon = {
    import javax.swing.Icon
    import java.awt.{Component, Graphics}
    new Icon {
      def paintIcon(target: Component, graphics: Graphics, x: Int, y: Int) = { 
        graphics.drawImage(self.toImage, x, y, target) 
      }
      def getIconWidth  = self.width
      def getIconHeight = self.height
    }
  }
  
  /**
   * Creates a stream of images where in each image this picture is moved by the given step to the right / left.
   * 
   * @param step the number of pixels to move. Positive value moves the image right, negative to the left. 
   */
  def slidingHorizontally(step: Double): Stream[Pic] = self.slidingHorizontally(step, windowSize = self.width, wrap = true)

  /**
   * Creates a stream of images where in each image this picture is moved by the given step downwards / upwards.
   * 
   * @param step the number of pixels to move. Positive value moves the image down, negative up. 
   */
  def slidingVertically(step: Double): Stream[Pic]   = self.slidingVertically(step, windowSize = self.height, wrap = true)

  /**
   * Creates a stream of images where in each image this picture is moved by the given step to the right / left.
   * 
   * @param step the number of pixels to move. Positive value moves the image right, negative to the left.
   * @param windowSize the size after which the image wraps
   * @param wrap whether the image should wrap or not
   */
  def slidingHorizontally(step: Double, windowSize: Int, wrap: Boolean = true) = {
    val slide = if (step >= 0) new Slide.Right(step, windowSize) else new Slide.Left(step, windowSize) 
    if (wrap) slide.forever else slide.once
  }

    /**
   * Creates a stream of images where in each image this picture is moved by the given step downwards / upwards.
   * 
   * @param step the number of pixels to move. Positive value moves the image down, negative up.
   * @param windowSize the size after which the image wraps
   * @param wrap whether the image should wrap or not
   */
  def slidingVertically(step: Double, windowSize: Int, wrap: Boolean = true) = {
    val slide = if (step >= 0) new Slide.Down(step, windowSize) else new Slide.Up(step, windowSize) 
    if (wrap) slide.forever else slide.once
  }


  private object Slide {
    // XXX optional cache näille
    // XXX toteutus meni vähän turhan abstraktiokikkailuksi; voisi selkiyttää? 
    abstract class Slide(val step: Double, val windowSize: Int, val attachExtension: Pic => Pic, val farEdge: Int, val sliceWidth: Int,
                         val sliceHeight: Int, val slicePosition: Int => (Int, Int), val applyOnceTo: Pic => Stream[Pic]) {
      final def once: Stream[Pic] = {
        def sliceStarts(from: Double): Stream[Int] = {
          if (this.isPastLastOffset(from.closestInt)) Stream.empty else from.closestInt #:: sliceStarts(from + this.step)
        }
        def createSliceAt(start: Int): Pic = {
          val (x, y) = this.slicePosition(start) // XXX myöhemmin Pos
          PicWithAnchor(self.crop(x, y, x + this.sliceWidth - 1, y + this.sliceHeight - 1), self.anchor) // XXX -1 ei kiva // XXX pwa
        }
        sliceStarts(from = this.firstOffset).map(createSliceAt)
      }
      final def forever: Stream[Pic] = {
        val onceAcross = this.applyOnceTo(this.extendedForWraparound)
        def wrappingStream: Stream[Pic] = onceAcross #::: wrappingStream
        wrappingStream
      }
      private lazy val extendedForWraparound = {
        val (x, y, w, h) = this.extraForWraparound
        val extension = PicWithAnchor(self.crop(x, y, x + w - 1, y + h - 1), self.anchor) // XXX pwa // XXX -1 ei kiva
        this.attachExtension(extension)
      }
      protected lazy val highestOffset = this.farEdge - this.windowSize
      protected def firstOffset: Int 
      protected def isPastLastOffset(offset: Int): Boolean
      protected def extraForWraparound: (Int, Int, Int, Int)
    }  

    abstract class Horizontal(step: Double, windowSize: Int, attachExtension: Pic => Pic) extends Slide(step, windowSize, attachExtension, farEdge = self.width,  
        sliceWidth = windowSize, sliceHeight = self.height, slicePosition = (_, 0), applyOnceTo = _.slidingHorizontally(step, windowSize, wrap = false) )
    
    abstract class Vertical(step: Double, windowSize: Int, attachExtension: Pic => Pic) extends Slide(step, windowSize, attachExtension, farEdge = self.height,  
        sliceWidth = self.width, sliceHeight = windowSize, slicePosition = (0, _), applyOnceTo = _.slidingVertically(step, windowSize, wrap = false) ) 
    
    class Right(step: Double, windowSize: Int) extends Horizontal(step, windowSize, self.leftOf(_)) with Forward {
      protected def extraForWraparound = (0, 0, this.sliceWidth - step.closestInt, this.sliceHeight)
    }
    class Down(step: Double, windowSize: Int) extends Vertical(step, windowSize, self.above(_)) with Forward {
      protected def extraForWraparound = (0, 0, this.sliceWidth, this.sliceHeight - step.closestInt) 
    }
   class Up(step: Double, windowSize: Int) extends Vertical(step, windowSize, self.below(_)) with Backward {
      protected def extraForWraparound = (0, this.highestOffset, this.sliceWidth, this.windowSize + step.closestInt) 
    }
    class Left(step: Double, windowSize: Int) extends Horizontal(step, windowSize, self.rightOf(_)) with Backward {
      protected def extraForWraparound = (this.highestOffset, 0, this.windowSize + step.closestInt, this.sliceHeight)
    }
    trait Forward extends Slide {
      assert(this.step >= 0)
      protected lazy val firstOffset = 0
      protected def isPastLastOffset(offset: Int) = offset > this.highestOffset
    }
    
    trait Backward extends Slide {
      assert(this.step < 0)
      protected lazy val firstOffset = this.highestOffset 
      protected def isPastLastOffset(offset: Int) = offset < 0
    }
  }
}



object Pic {
  /**
   * Creates a Pic from an existing bitmap.
   * 
   * @param imagePath the path to the image to be loaded
   * @param anchor the anchor to be set (by default this is the Center anchor)
   */
  def apply(imagePath: String, anchor: Anchor = Center) = PicWithAnchor(aalto.smcl.bitmaps.Bitmap(imagePath).bitmaps(0), anchor) // XXX loading handling // XXX pwa  

  import scala.swing.{Frame, Panel, Swing, Graphics2D}
  import scala.collection.mutable.Map
  
  private val framesForPics = Map[Pic, PicFrame]()
  
  /**
   * Displays a picture in a frame.
   * 
   * @param pic the picture to show
   * @param backgroundColor the backgroundcolor to use
   * @param borderWidth the border width for the image frame
   */
  
  def show(pic: Pic, backgroundColor: Color, borderWidth: Int) = {

    def newFrame(pic: Pic) = new PicFrame(pic, backgroundColor, borderWidth)
    
    val frame = this.framesForPics.getOrElseUpdate(pic, newFrame(pic)) 
    if (!frame.visible) {
      frame.pack()
      frame.visible = true
      ToolTipManager.sharedInstance.setInitialDelay(150)
    }
    frame.backgroundColor = backgroundColor
    frame.borderWidth = borderWidth
    frame.contents.headOption.foreach( _.requestFocus() )
  }
  
  /**
   * Hides the frame showing a Pic.
   * 
   * @param the picture to hide
   */
  def hide(pic: Pic) = {
    for (frame <- this.framesForPics.remove(pic)) {
      frame.close()
    } 
  }
  
  /**
   * Hides all pictures displayed.
   */
  def hideAll() = {
    this.framesForPics.keys.foreach(this.hide)
  }
  
  private def visibleFrameCount = this.framesForPics.size 
  
  private class PicFrame(val pic: Pic, private var bgColor: Color, private var borders: Int) extends Frame {
    import java.awt.Color.{WHITE, BLACK}
    this.peer.setUndecorated(true)
    this.resizable = false
    this.location = new Point(150 + visibleFrameCount * 10, 150 + visibleFrameCount * 10)
    private val panel = new Panel {
      val image = pic.toImage
      preferredSize = pic.dimensions.wider(borderWidth * 2).higher(borderWidth * 2)
      tooltip = "Click or press Esc to close."
      override def paintComponent(myGraphics: Graphics2D) = {
        super.paintComponent(myGraphics)
        myGraphics.drawImage(this.image, borderWidth, borderWidth, null)
      }
    }
    this.backgroundColor = bgColor
    this.borderWidth = borders
    this.contents = this.panel
    this.listenTo(panel.mouse.clicks)
    this.listenTo(panel.keys)
    this.reactions += {
      case KeyPressed(_, Key.Escape, _, _) => 
        Pic.hide(this.pic)
      case press: MousePressed =>
        Pic.hide(this.pic)
    }

    def borderWidth = this.borders
    def borderWidth_=(newWidth: Int) = {
      this.borders = newWidth
      this.panel.border = Swing.LineBorder(BLACK, this.borderWidth)
      this.panel.repaint()
    }

    def backgroundColor = this.bgColor
    def backgroundColor_=(color: Color) = {
      this.bgColor = color
      this.panel.background = new java.awt.Color(color.red, color.green, color.blue) // XXX helpompi?
      this.panel.repaint()
    }
  }

} 
  
