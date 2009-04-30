import scala.actors._
import Actor._

class MapUpdatePoller(agent: Actor, val map: Actor) extends Actor {
    val lastUpdateThreshold = 5000 //milliseconds
    map ! "lastUpdate"

    def act()
	{
		println("GUI Poller running")
        loop
		{
			Thread.sleep(300)
            if((map ! "lastUpdate") > lastUpdateThreshold)
                agent ! "Stop Exploring"
            react
			{
              case "Exit" => {
                 println("MapUpdatePoller Exiting")
                 this.exit
              }
			}//end react
		}//end loop
    }
}
