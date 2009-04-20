

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

class scalaGui extends Actor{



  def act()
  {
        println ("Running")
		val topologicalElementGenerator: Actor = new TopologicalElementGenerator
		val relationshipIdentfier: Actor = new RelationshipIdentfier
		val map: Actor = new CollectiveMap
		val guiInstance: GUI = new GUI(25,25,600,600)//blockX,blockY,windoxX,windowY
		
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
			val agent = new Agent(world, topologicalElementGenerator, relationshipIdentfier, map,
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
        loop
		{
			react
			{
                case "Exit" => this.exit
                case AgentUpdate(x, y, present) =>{
                        guiInstance.updateCellAgentStatus(x, y, present)
                    }
            }
        }
  }
}
