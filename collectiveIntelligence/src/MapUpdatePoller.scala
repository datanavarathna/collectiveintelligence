import scala.actors._
import Actor._

class MapUpdatePoller(val agent: Actor, val map: Actor) extends Actor {
    val lastUpdateThreshold : Long = 15000 //milliseconds
    map ! "lastUpdate"

    def act()
	{
		println("mapUpdate Poller running")
        loop
		{
			Thread.sleep(300)
            map ! "lastUpdate"
            
            react
			{/*
              case TimeSinceLastUpdate(lastUpdate) => {
                   if(lastUpdate > lastUpdateThreshold){
                       agent ! "Stop Exploring"
                       //println("Stopped exploring after " + (lastUpdateThreshold/1000) + " sec")
                   }
              }*/
              case "Exit" => {
                 println("MapUpdatePoller Exiting")
                 this.exit
              }
			}//end react
		}//end loop
    }
}
