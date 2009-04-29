

import scala.actors._
import Actor._
//import CollectiveMap
//import RelationshipIdentfier
//import TopologicalElementGenerator
//import gui.GUI
//import gui.ObjectSpecs
//import gui.AgentSpecs


//case class Obstacle(obstacleType: Int, x: Int, y: Int) 
case class AgentWithLocation(agent: Agent, x: Int, y: Int){
	override def toString = "AgentWithLocation: agent="+agent+" x="+x+" y="+y
}

case class javaGui(gui:GUI)

class scalaGui(squareX: Int, squareY: Int, windowX: Int, windowY: Int) extends Actor{
  def  this() = this(25,25,600,600)


  def act()
  {
        println ("Running")
		val map: Actor = new CollectiveMap
		val guiInstance: GUI = new GUI(squareX,squareY,windowX,windowY)//blockX,blockY,windoxX,windowY

        map.start

		while(!guiInstance.isReadyForRuntime())
		{
			Thread.sleep(300)
		}
		var obstacleList = Nil.asInstanceOf[List[Obstacle]]
		var agentList = Nil.asInstanceOf[List[AgentWithLocation]]
		val javaObstacleList: List[ObjectSpecs] = guiInstance.getOs().toList.asInstanceOf[List[ObjectSpecs]]
		println("JavaObstacleList="+javaObstacleList)
		val javaAgentList: List[AgentSpecs] = guiInstance.getAs().toList.asInstanceOf[List[AgentSpecs]]
		println("javaAgentList="+javaAgentList)
		//guiInstance.getOS().foreach((objectSpec: ObjectSpecs) => obstacleList = Obstacle(objectSpec.getType(),objectSpec.getX(),objectSpec.getY())::obstacleList)
		for((objectSpec: ObjectSpecs) <- javaObstacleList)
		{
			println("objectSpec="+objectSpec)
			val tempObstacle = Obstacle(objectSpec.getType(),objectSpec.getX(),objectSpec.getY())
			println(tempObstacle)
			obstacleList = tempObstacle::obstacleList
		}
		println(obstacleList)
		val world: Environment = new Environment( guiInstance.getX()-1, guiInstance.getY()-1, this)
		world.start()
		for( agentSpec <- javaAgentList)
		{
			println("agentSpec="+agentSpec)
			val agent = new Agent(world, map,
                     agentSpec.getSensorRange(),agentSpec.getDeltaAngle(),agentSpec.getSensorDeltaRange())
			println(agent)
			val tempAgentWithLocation = AgentWithLocation(agent,agentSpec.getX(),agentSpec.getY())
            agentList=tempAgentWithLocation::agentList
  
		}
		println(agentList)
		println ("Transmitting obstacleList")
		world ! obstacleList
		println ("Transmitting agentList")
		world ! agentList
        val guiAlivePoller = new GuiAlivePoller(this)
        println("Create guiAlivePoller")
        guiAlivePoller.start
        guiAlivePoller ! javaGui(guiInstance)
        loop
		{
			react
			{
                case "Exit" => {
                    world ! "Exit"
                    map ! "Exit"
                    guiAlivePoller ! "Exit"
                    println("scalaGui Exiting")
                    this.exit
                }
                case AgentUpdate(x, y, present) =>{
                        guiInstance.updateCellAgentStatus(x, y, present)
                }
            }
        }
  }
}
