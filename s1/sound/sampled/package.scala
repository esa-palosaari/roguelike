
////////////////// NOTE TO STUDENTS //////////////////////////
// For the purposes of our course, it's not necessary    
// that you understand or even look at the code in this file. 
//////////////////////////////////////////////////////////////

package s1.sound

import s1.util.{tryForURL,ConvenientFloat} 
import javax.sound.sampled.{Clip,FloatControl,AudioSystem}

package object sampled {

  // Supported formats: WAV, AIFF, AU, AIFC, SND

  /**
   * Constant for endless repeat
   */
  val KeepRepeating = Clip.LOOP_CONTINUOUSLY
  
  /**
   * Constant for muting the sound
   */
  val Mute = Float.MinValue
  
  /**
   * Loads a sound from a file
   * @param peer the name of the file
   * @param volume amplification in decibels (0.0 is the original volume)
   */

  case class Sound(val peer: Clip, val volume: Float) {
    
    /**
     * Plays this sound
     * @param repeats how many times to repeat
     * @param volume amplification in decibels (0.0 is the original volume)
     */
    def play(repeats: Int = 0, volume: Float = this.volume) = {
      if (this.peer.isRunning) {
        this.peer.stop()
      }
      if (volume != Mute) {
        val gain = this.peer.getControl(FloatControl.Type.MASTER_GAIN).asInstanceOf[FloatControl]
        gain.setValue(volume atLeast gain.getMinimum atMost gain.getMaximum)
        this.peer.setFramePosition(0)
        this.peer.loop(repeats)
      }
    }
    
    /**
     * Changes the volume
     */  
    def withVolume(differentVolume: Float) = this.copy(volume = differentVolume)
    
    /**
     * Stops the sound from playing
     */
    def stop() = {
      this.peer.stop()
    }
  }
  
  
  object Sound {
    /**
     * Loads a sound from a file
     * @param id the name of the file
     * @param volume amplification in decibels (0.0 is the original volume)
     */
    
    def apply(id: String, volume: Float = 0): Sound = {
      val audioInput = AudioSystem.getAudioInputStream(tryForURL(id).get)
      val clip = AudioSystem.getClip()
      clip.open(audioInput)
      Sound(clip, volume)
    }
  }
 
  /**
   * Loads a sound from a file and plays it
   * @param id the name of the file
   * @param volume amplification in decibels (0.0 is the original volume)
   * @param repeats the number of repeats
   */
  
  def playRecording(id: String, volume: Float = 0, repeats: Int = 0) = {
    val sound = this.loadRecording(id, volume)
    sound.play(repeats)
    sound
  }

 /**
  * Loads a sound from a file
  * @param id the name of the file
  * @param volume amplification in decibels (0.0 is the original volume)
  */  
  def loadRecording(id: String, volume: Float = 0) = Sound(id, volume)
  
  
}

