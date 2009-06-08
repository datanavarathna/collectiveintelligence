import scala.swing._
import java.awt.Color

object RegionButton {
    val Empty: Int =0
    val Agent: Int = 1
    val Obstacle: Int = 2

    val LowestStatus = Empty
    val HighestStatus = 3
}

class RegionButton(val x: Int, val y: Int) extends Button {
    var status = RegionButton.Empty
    var obstacleType: Int = _
    val defaultBackground = background

    var sensorRange: Double = 5
    var sensorDeltaRange: Double = 0.5
    var sensorDeltaAngle: Double = 0.2 //degree


    override def toString = "RegionButton - Status: " + status

    def incrementStatus {
        status += 1
        if(status > RegionButton.HighestStatus)
            status = RegionButton.LowestStatus
    }

    def update() {
       if(status == RegionButton.Empty){
            background = defaultBackground
            text = ""
        }
        else if (status == RegionButton.Agent){
            background = new Color(4,115,131)
            text = "A"
        }
        else if(status >= RegionButton.Obstacle){
            background = new Color(224,61,61)
            obstacleType = (status - 1)
            text = obstacleType.toString
        }
    }//end update
    
}
