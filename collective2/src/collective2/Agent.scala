package collective2

import scala.actors._
import Actor._
import uncertaintyMath.Measurement
import uncertaintyMath.Measurement._

import scala.util.Random

case class detectedObstacles()

class Agent(val environment: Actor, val collectiveMap: Actor,
            val sensorRange: Double, val sensorDeltaAngle: Double, val sensorDeltaRange: Double) extends Actor
{
	link(environment)
	private[this] var exploredArea = new QuadBitSet
	
	def explored(x: Int, y: Int): Boolean = {
		exploredArea.contains(x,y)
	}
	
	//agents are detected from sensor readings
	//obstacles are obtained from a (x,y) indexed data structure
	
	//scan
	//obtain possible matches from collectiveMap
	//explore to eliminate matches
	//add detected elements to map
	//navigate, then return to scan
	
	def act() {
		
	}
	
}