
////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it's not necessary    
// that you understand or even look at the code in this file. 
//////////////////////////////////////////////////////////////

package s1


package object util {

  
  ///// ALIASES FOR CONVENIENCE ///// 
  
  type Try[T]     = scala.util.Try[T]
  val  Try        = scala.util.Try
  type Success[T] = scala.util.Success[T]
  val  Success    = scala.util.Success
  type Failure[T] = scala.util.Failure[T]
  val  Failure    = scala.util.Failure

  type Random  = scala.util.Random
  val  Random  = scala.util.Random
  

  ///// UTILITIES FOR I/O /////

  import scala.io.Source
  import java.net.URL
  import java.io.FileNotFoundException
  import scala.language.reflectiveCalls
  
  def useAndClose[Resource <: Closeable, Result](resource: Resource)(operation: Resource => Result) = {
    try {
      operation(resource)
    } finally {
      resource.close()
    }
  }

  type Closeable = { def close(): Unit }

  private def source(url: URL): Source = Source.fromURL(url, "UTF-8") 
  
  def localURL(id: String)    = Option(this.getClass.getResource("/" + id))
  def localSource(id: String) = localURL(id).map(source)
  
  private def tryForResource[Result](id: String, transform: URL => Result) = {
    def withinWorkingDir(filename: String) = Try { localURL(filename).map(transform).getOrElse(throw new FileNotFoundException) }
    def url(url: String, prefix: String)   = Try { transform(new URL(prefix + url)) }
    withinWorkingDir(id) orElse url(id, "") orElse url(id, "http://") orElse url(id, "https://")
  }
  def tryForURL(id: String)    = tryForResource(id, identity) 
  def tryForSource(id: String) = tryForResource(id, source)

  
  
  ///// CONVENIENCE METHODS /////
  
  implicit class ConvenientCollection[Element](val self: Traversable[Element]) {
    def mapify[Key, Value](formKey: Element => Key)(formValue: Element => Value): Map[Key, Value] =
      self.map( elem => formKey(elem) -> formValue(elem) )(collection.breakOut)
  }
  
  implicit class ConvenientInt(val value: Int) {
    def atLeast(minimum: Int) = this.value.max(minimum)
    def atMost(maximum: Int)  = this.value.min(maximum)
    def isBetween(low: Int, high: Int) = this.value >= low && this.value < high
  }
  implicit class ConvenientDouble(val value: Double) {
    def atLeast(minimum: Double) = this.value.max(minimum)
    def atMost(maximum: Double)  = this.value.min(maximum)
    def isBetween(low: Double, high: Double) = this.value >= low && this.value < high
    def closestInt = {
      val longValue = value.round
      if (longValue > Int.MaxValue || longValue < Int.MinValue) throw new IllegalArgumentException("Integer out of bounds: " + longValue)
      longValue.toInt
    }
  }
  implicit class ConvenientFloat(val value: Float) {
    def atLeast(minimum: Float) = this.value.max(minimum)
    def atMost(maximum: Float)  = this.value.min(maximum)
    def isBetween(low: Float, high: Float) = this.value >= low && this.value < high
  }
  implicit class ConvenientLong(val value: Long) {
    def atLeast(minimum: Long) = this.value.max(minimum)
    def atMost(maximum: Long)  = this.value.min(maximum)
    def isBetween(low: Long, high: Long) = this.value >= low && this.value < high
  }
  
  implicit class RegexContext(interpolated: StringContext) {
    import scala.util.matching.Regex
    def r = new Regex(interpolated.parts.mkString, interpolated.parts.tail.map( _ => "unnamedGroup" ): _*)
  }
  
  

  ///// TAGGED TYPES /////

  // based on http://etorreborre.blogspot.fi/2011/11/practical-uses-for-unboxed-tagged-types.html
  type Tagged[TagType] = { type Tag = TagType }
  type @@[BaseType, TagType] = BaseType with Tagged[TagType]

  trait MSTag
  type Millisec = Int @@ MSTag
  def ms(timeInMilliseconds: Int) = timeInMilliseconds.asInstanceOf[Millisec]

  
  
  ///// TIMED EVENTS /////
  
  import javax.swing.{Timer,Action,AbstractAction}
  import java.awt.event.ActionEvent

  class Ticker private(initialDelay: Millisec, val interval: Millisec, private val timedAction: Action) {
    
    def this(initialDelay: Millisec, interval: Millisec)(timedBlock: =>Unit) = 
      this(initialDelay, interval, new AbstractAction { def actionPerformed(tick: ActionEvent) = timedBlock })
  
    private[this] val javaTimer = {
      val timer = new Timer(this.interval, this.timedAction)
      timer.setRepeats(true)
      timer.setInitialDelay(initialDelay)
      timer
    }
      

    def start() = {
      this.javaTimer.start()
    }

    def stop() = {
      this.javaTimer.stop()
    }
    
    def isRunning = this.javaTimer.isRunning
  }
  
  def repeatEvery(interval: Millisec)(timedBlock: =>Unit) = {
    new Ticker(interval, interval)(timedBlock).start()
  }

  
  ///// MISCELLANEOUS /////

  def editDistance(text1: String, text2: String, threshold: Int): Int =
    if (text1.isEmpty)
      if (text2.length <= threshold) text2.length else Int.MaxValue
    else if (text2.isEmpty)
      if (text1.length <= threshold) text1.length else Int.MaxValue
    else if (text1.head == text2.head)
      editDistance(text1.tail, text2.tail, threshold)
    else if (threshold == 0) 
      Int.MaxValue 
    else { 
      val deletion     = editDistance(text1.tail, text2     , threshold - 1)
      val insertion    = editDistance(text1,      text2.tail, threshold - 1)
      val substitution = editDistance(text1.tail, text2.tail, threshold - 1)
      val shortest = Seq(deletion, insertion, substitution).min
      if (shortest == Int.MaxValue) Int.MaxValue else shortest + 1
    }

  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global

  class DiscardingThrottler {
    private[this] var isIdle = true 
    def ifIdle[Result](possiblyHeavyComputation: =>Result) = {
      if (this.isIdle) {
        this.isIdle = false
        val worker = Future(possiblyHeavyComputation)
        worker.onComplete { _ => this.isIdle = true } 
        worker
      } else {
        Future.failed(DiscardingThrottler.Busy)
      }
    } 
  }
  object DiscardingThrottler {
    object Busy extends RuntimeException("discarding throttler busy; ignoring work request")
  }
  
  
  object Program { // not entirely reliable, but good enough for some purposes
    lazy val isRunningInScalaREPL = Try(Class.forName("scala.tools.nsc.interpreter.IMain")).isSuccess 
  }
  
}
