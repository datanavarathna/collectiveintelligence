package collective

import scala.actors._
import Actor._

class ScalaGUIhelperActor(val world: Actor, val map: Actor, gui: ScalaGUI) extends Actor{
 def act()
    {
        loop
		{
			react
			{
                case "Exit" => {
                    world ! "Exit"
                    map ! "Exit"
                    println("scalaGui Exiting")
                    this.exit
                }
                case AgentUpdate(oldX, oldY, newX, newY) =>{
                        //update relevant button
                        //println("GUI UPDATE: (" +oldX+","+oldY+")->("+newX+","+newY+")")
                        val oldLoc = gui.worldButtons(oldY*gui.environmentX+oldX)
                        val newLoc = gui.worldButtons(newY*gui.environmentX+newX)
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