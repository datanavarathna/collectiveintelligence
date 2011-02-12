package collective2

import scala.swing._
import scala.swing.event._
import scala.actors._
import Actor._

import definitions._

case class AgentDestoyed(b: RegionButton) extends Event
case class ClickedAgentButton(agentButton: RegionButton) extends Event
case class Started() extends Event

//environmentX - number of columns in environment
//environmentY - number of rows in environment
//width - frame width in pixels
//height - frame height in pixels
class ScalaGUI(val environmentX: Int, val environmentY: Int,
               val width: Int, val height: Int) extends FlowPanel{
     require(width > 60)
     require(height > 10)
     val sideBarWidth = 170;
     val worldWidth = width - sideBarWidth
     val worldHeight = height - 5;
     //println("ScalaGUI envX: " + environmentX + " envY: " + environmentY)

    //initial primary actors
    val helpActor: ScalaGUIhelperActor = new ScalaGUIhelperActor( this)
    val map: Actor = new CollectiveMap
    val world: Environment = new Environment( environmentX-1, environmentY-1, helpActor)
     
    //initialize lists
    var agentsWithLocations: List[AgentWithLocation] = Nil
    var obstacles: List[Obstacle] = Nil

    //initialize new world of RegionButtons 
    var worldButtons = new Array[RegionButton](environmentX*environmentY)
     for(i <- 0 until (environmentX*environmentY)){
        val (x,y) = indexToXY(environmentX,i)
        worldButtons(i) = new RegionButton(x,y)
     }

     val worldView = new GridPanel(environmentX,environmentY){
        //register worldButtons for worldView
    	 for(button <- worldButtons){
            contents += button
            listenTo(button) 
        }
        reactions += {
            case ButtonClicked(b) => {
            	b match {
            		case rb: RegionButton => {
            			rb.incrementStatus
            			if(rb.status == RegionButton.Agent){
            				publish(ClickedAgentButton(rb))
            			}else if(rb.status == RegionButton.Obstacle ){
            				publish(AgentDestoyed(rb))
            			}
            		}
            		case _ => println("Not region button")//Should never execute
            	}   

            }
            case Started() => {
            	//when simulation starts, change GUI from control mode to display
                for(button <- worldButtons){
                    deafTo(button)
                }
            }
        }//end reactions

        listenTo(this)
        preferredSize = new Dimension(worldWidth,worldHeight)
     }

    //create sidebar buttons
    val initializeButton = new Button{
        text = "Initialize"
    }
    val startButton = new Button{
        text = "Start"
    }
    val killButton = new Button{
        text = "Kill Program"
    }
    
    //create sidebar fields
    val sensorRangeLabel = new Label("Sensor Range")
    val sensorRangeTextfield = new TextField{

    }
    val sensorDeltaRangeLabel = new Label("Sensor Delta Range (deg)")
    val sensorDeltaRangeTextfield = new TextField{

    }
    val DeltaAngleLabel = new Label("Delta Angle")
    val DeltaAngleTextfield = new TextField{

    }
    var agentXLabel = new Label
    var agentYLabel = new Label
    var updateButton = new Button{
        text = "Update"
    }
    
    //agentSetup sidebar initialization
    var agentSetup = new BoxPanel(Orientation.Vertical){
        contents += sensorRangeLabel
        contents += sensorRangeTextfield
        contents += sensorDeltaRangeLabel
        contents += sensorDeltaRangeTextfield
        contents += DeltaAngleLabel
        contents += DeltaAngleTextfield
        contents += agentXLabel
        contents += agentYLabel
        contents += updateButton

        listenTo(worldView)
        listenTo(updateButton)

        visible = false
        var agentButtonVar: RegionButton = _

        reactions += {
            case ClickedAgentButton(agentButton) =>{
            	//autofill sidebar fields with button values
                 sensorRangeTextfield.text = agentButton.sensorRange.toString
                 sensorDeltaRangeTextfield.text = agentButton.sensorDeltaRange.toString
                 DeltaAngleTextfield.text = agentButton.sensorDeltaAngle.toString
                 agentXLabel.text = "x: " + (agentButton.x+1)
                 agentYLabel.text = "y: " + (agentButton.y+1)
                 agentButtonVar = agentButton//supposed to make fields appear
                 //make agentStatusUpdate visible
                 visible = true 
                 contents.foreach(_.visible_=(true))
            }
            case AgentDestoyed(b) => {
            	contents.foreach(_.visible_=(false))//make agentStatusUpdate disappear
            }
            case ButtonClicked(b) => {
                try{
                	//attempt to parse text
                    agentButtonVar.sensorRange = sensorRangeTextfield.text.toDouble
                    agentButtonVar.sensorDeltaRange = sensorDeltaRangeTextfield.text.toDouble
                    agentButtonVar.sensorDeltaAngle = DeltaAngleTextfield.text.toDouble
                    //make agentStatusUpdate disappear
                    contents.foreach(_.visible_=(false))
                }
                catch{
                    case ex: Exception => println("Invalid values entered")
                }
            }
        }
    }

    val sideBar = new BoxPanel(Orientation.Vertical){
        contents += initializeButton
        contents += startButton
        contents += killButton
        contents += agentSetup
    }

    
    //add world and sidebar to scalaGUI contents
    contents += worldView
    contents += sideBar
    //register sidebar button listeners
    listenTo(initializeButton)
    listenTo(startButton)
    listenTo(startButton)

    reactions +={
        case ButtonClicked(b) =>{
            if(b eq initializeButton){
                //initialize arrays
                println("Initialize clicked")
                for(button <- worldButtons){
                    if(button.status == RegionButton.Agent){
                    	//append new agent to agentsWithLocations list
                        
                         agentsWithLocations = AgentWithLocation(
                            new Agent(world, map, button.sensorRange,
                                  button.sensorDeltaAngle, button.sensorDeltaRange),
                                    button.x,button.y) :: agentsWithLocations
                        
                    }
                    else if(button.status > RegionButton.Agent){
                       obstacles = new Obstacle(button.obstacleType,button.x,button.y) :: obstacles
                    }
                }//end for
            }
            else if(b eq startButton){
                //start simulation
                publish(Started())
                
                map.start
                world.start
                world ! obstacles //send obstacles to world
                world ! agentsWithLocations //send agentsWithLocations to world
                helpActor.start
                
            }
            else{
                //run kill statements
                helpActor ! "Exit"
            	//world ! "Exit"
                map ! "Exit"
            }
        }
    }

    preferredSize = new Dimension(width,height)

   
}
