package collective2

import scala.actors._
import Actor._

class ScalaGUIhelperActor(gui: AbstractScalaGUI) extends Actor{
 def act()
    {
	 	println("scalaGUIhelperActor started")
        loop
		{
			react
			{
                case "Exit" => {
                    
                    println("scalaGui Exiting")
                    this.exit
                }
                case AgentUpdate(oldX, oldY, newX, newY) =>{
                        //update relevant button
                        println("GUI UPDATE: (" +oldX+","+oldY+")->("+newX+","+newY+")")
                        val environmentX = gui.getEnvironmentX
                        val oldLoc = gui.getWorldButton(oldY*environmentX+oldX)
                        val newLoc = gui.getWorldButton(newY*environmentX+newX)
                        //println(oldLoc.status +" -> "+ newLoc.status)
                        oldLoc.status = RegionButton.Empty
                        newLoc.status = RegionButton.Agent
                        oldLoc.update
                        newLoc.update
                        //oldLoc.repaint
                        //newLoc.repaint
                }
            }//end react
        }//end loop
    }//end act
}
