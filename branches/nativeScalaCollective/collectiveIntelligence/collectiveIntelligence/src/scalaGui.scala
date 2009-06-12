import scala.swing._
import scala.swing.event._
import scala.actors._
import Actor._

case class ClickedAgentButton(agentButton: RegionButton) extends Event
case class Started() extends Event

class ScalaGUI(val environmentX: Int, val environmentY: Int,
               val width: Int, val height: Int) extends FlowPanel{
     require(width > 60)
     require(height > 10)
     val sideBarWidth = 170;
     val worldWidth = width - sideBarWidth
     val worldHeight = height - 5;
     //println("ScalaGUI envX: " + environmentX + " envY: " + environmentY)


    val helpActor: ScalaGUIhelperActor = new ScalaGUIhelperActor(map, world, this)
    val map: Actor = new CollectiveMap
    val world: Environment = new Environment( environmentX-1, environmentY-1, helpActor)

    var agentsWithLocations: List[AgentWithLocation] = Nil
    var obstacles: List[Obstacle] = Nil

     var worldButtons = new Array[RegionButton](environmentX*environmentY)
     for(i <- 0 until (environmentX*environmentY)){
        val x: Int = i % environmentX
        val y: Int = i / environmentX
        worldButtons(i) = new RegionButton(x,y)
     }

     val worldView = new GridPanel(environmentX,environmentY){
        for(button <- worldButtons){
            contents += button
            listenTo(button) 
        }
        reactions += {
            case ButtonClicked(b) => {
                if(b.isInstanceOf[RegionButton]){
                    var rb = b.asInstanceOf[RegionButton]
                    rb.incrementStatus
                    rb.update
                    if(rb.status == RegionButton.Agent)
                        publish(ClickedAgentButton(rb))
                }
                else
                    println("Not region button")

            }
            case Started() => {
                for(button <- worldButtons){
                    deafTo(button)
                }
            }
        }//end reactions

        listenTo(this)
        preferredSize = (worldWidth,worldHeight)
     }

    val initializeButton = new Button{
        text = "Initialize"
    }
    val startButton = new Button{
        text = "Start"
    }
    val killButton = new Button{
        text = "Kill Program"
    }

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
                 sensorRangeTextfield.text = agentButton.sensorRange.toString
                 sensorDeltaRangeTextfield.text = agentButton.sensorDeltaRange.toString
                 DeltaAngleTextfield.text = agentButton.sensorDeltaAngle.toString
                 agentXLabel.text = "x: " + (agentButton.x+1)
                 agentYLabel.text = "y: " + (agentButton.y+1)
                 agentButtonVar = agentButton
                 visible = true
            }
            case ButtonClicked(b) => {
                try{
                    agentButtonVar.sensorRange = sensorRangeTextfield.text.toDouble
                    agentButtonVar.sensorDeltaRange = sensorDeltaRangeTextfield.text.toDouble
                    agentButtonVar.sensorDeltaAngle = DeltaAngleTextfield.text.toDouble
                    visible = false
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

    

    contents += worldView
    contents += sideBar

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
                world ! obstacles
                world ! agentsWithLocations
                helpActor.start
            }
            else{
                //run kill statements
                helpActor ! "Exit"
            }
        }
    }

    preferredSize = (width,height)

   
}
