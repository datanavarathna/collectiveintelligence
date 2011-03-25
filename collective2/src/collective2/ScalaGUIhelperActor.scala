package collective2

import scala.actors._
import Actor._

import collective2.definitions._

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
                        val oldLoc = gui.getWorldButton(xyToIndex(oldX,oldY,environmentX-1))
                        val newLoc = gui.getWorldButton(xyToIndex(newX,newY,environmentX-1))
                        //println(oldLoc.status +" -> "+ newLoc.status)
                        oldLoc.status = RegionButton.Empty
                        newLoc.status = RegionButton.Agent
                        oldLoc.update
                        newLoc.update
                        //gui.repaint
                        //oldLoc.repaint
                        //newLoc.repaint
                }
            }//end react
        }//end loop
    }//end act
}
