

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

object scalaGui extends Actor{

  def act()
  {
    loop
		{
			react
			{
                case "Exit" => this.exit
            }
        }
  }
  
	def main(args : Array[String]) = {
		println ("Running")
		val topologicalElementGenerator: Actor = new TopologicalElementGenerator
		val relationshipIdentfier: Actor = new RelationshipIdentfier
		val map: Actor = new CollectiveMap
		val guiInstance: GUI = new GUI()
		
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
		val world: Environment = new Environment( 0, 0, guiInstance.getX(), guiInstance.getY())
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
		//call guiInstance updateCellAgentStatus( int x, int y, boolean agentInSquare ) 
	}
}
