package collective.gui

import scala.actors._
import Actor._
import agents._
import collectivemap._
import collectivemap.CollectiveMap
import collectivemap.RelationshipIdentfier
import collectivemap.TopologicalElementGenerator
//import gui.GUI
//import gui.ObjectSpecs
//import gui.AgentSpecs


case class Obstacle(obstacleType: Int, x: Int, y: Int) 
case class AgentWithLocation(agent: Agent, x: Int, y: Int)

object scalaGui extends Actor{

  def act()
  {
    
  }
  
	def main(args : Array[String]) = {
		println ("Running")
		val topologicalElementGenerator: Actor = new TopologicalElementGenerator
		val relationshipIdentfier: Actor = new RelationshipIdentfier
		val map: Actor = new CollectiveMap
		val guiInstance: GUI = new GUI()
		
		while(guiInstance.isReadyForRuntime())
		{
			Thread.sleep(300)
		}
		var obstacleList = Nil.asInstanceOf[List[Obstacle]]
		var agentList = Nil.asInstanceOf[List[AgentWithLocation]]
		val javaObstacleList: List[ObjectSpecs] = guiInstance.getOs().toArray().toList.asInstanceOf[List[ObjectSpecs]]
		val javaAgentList: List[AgentSpecs] = guiInstance.getAs().toArray().toList.asInstanceOf[List[AgentSpecs]]
		//guiInstance.getOS().foreach((objectSpec: ObjectSpecs) => obstacleList = Obstacle(objectSpec.getType(),objectSpec.getX(),objectSpec.getY())::obstacleList)
		for((objectSpec: ObjectSpecs) <- javaObstacleList)
		{
			obstacleList = Obstacle(objectSpec.getType(),objectSpec.getX(),objectSpec.getY())::obstacleList
		}
		val world: Environment = new Environment( 0, 0, guiInstance.getX(), guiInstance.getY())
		
		for( agentSpec <- javaAgentList)
		{
			val agent = new Agent(world, topologicalElementGenerator, relationshipIdentfier, map,
                     agentSpec.getSensorRange(),agentSpec.getDeltaAngle(),agentSpec.getSensorDeltaRange())
            AgentWithLocation(agent,agentSpec.getX(),agentSpec.getY())::agentList
  
		}
		
		world ! obstacleList
		world ! agentList
	}
}