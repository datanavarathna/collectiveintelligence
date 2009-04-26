import scala.actors._
import Actor._

class GuiAlivePoller(val scalaGui: Actor) extends Actor {
    var gui: GUI = _

    def act()
	{
		println("GUI Poller running")
        loop
		{
			react
			{
              case javaGui(guiInstance) =>{
                  println("Received guiInstance")
                  gui = guiInstance
                  this ! "Poll"
                  
              }
              case "Poll" => {
                      if(!gui.isRunning){
                          println("Exiting")
                          scalaGui ! "Exit"
                          gui.smite
                      }
                      else{
                          Thread.sleep(300)
                          this ! "Poll"
                      }
              }
              case "Exit" => {
                 println("GuiAlivePoller Exiting")
                 this.exit
              }
			}//end react
		}//end loop
    }
}
