package collective.gui

import scala.actors._
import Actor._
import agents._
import collectivemap._
import gui.GUI
import gui.ObjectSpecs
import gui.AgentSpecs


case class Obstacle(obstacleType: Int, x: Int, y: Int) 
case class AgentWithLocation(agent: Agent, x: Int, y: Int)

object scalaGui extends Actor{
	def main(args: Array[String]) {
		println ("Running")
		val topologicalElementGenerator: Actor = new TopologicalElementGenerator
		val relationshipIdentfier: Actor = new RelationshipIdentfier
		val map: Actor = new CollectiveMap
		val guiInstance: GUI = new GUI()
		
		while(guiInstance.isReadyForRuntime())
		{
			Thread.sleep(300)
		}
		var obstacleList = Nil
		var agentList = Nil
		//guiInstance.getOS().foreach((objectSpec: ObjectSpecs) => obstacleList = Obstacle(objectSpec.getType(),objectSpec.getX(),objectSpec.getY())::obstacleList)
		for(objectSpec <- guiInstance.getOs())
		{
			obstacleList = Obstacle(objectSpec.getType(),objectSpec.getX(),objectSpec.getY())::obstacleList
		}
		val world: Environment = new Environment( 0, 0, guiInstance.getX(), guiInstance.getY())
		
		for( agentSpec <- guiInstance.getOs())
		{
			val agent = new Agent(world, topologicalElementGenerator, relationshipIdentfier, map,
                     agentSpec.getSensorRange(),agentSpec.getDeltaAngle(),getDeltaSensorRange())
            AgentWithLocation(agent,agentSpec.getX(),agentSpec.getY())::agentList
  
		}
		
		world ! obstacleList
		world ! agentList
	}
}