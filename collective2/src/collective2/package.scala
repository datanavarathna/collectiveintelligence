package collective2

package object definitions {
	def indexToXY(sizeX: Int, index: Int): (Int, Int) = {
		require (sizeX > 0)
		val x: Int = index % sizeX
        val y: Int = index / sizeX
        return (x,y)
	}
	
	def xyToIndex(x: Int, y: Int, sizeX: Int): Int = {
		require (sizeX > 0)
		return y*sizeX+x
	}
	
	import uncertaintyMath.Measurement
	import uncertaintyMath.Measurement._
	def PolarToCartesian(angle: Measurement, distance: Measurement): Displacement =  {
      Displacement(distance * cos(angle), distance * sin(angle))
  }
}

import scala.actors._
import Actor._
import uncertaintyMath.Measurement
import uncertaintyMath.Measurement._

import scala.collection.mutable

/*
case class Pheromone(locationX: Int,LocationY: Int,targetX: Int, targetY: Int)

*/
case class Displacement(x: Measurement, y: Measurement) {
    def canEqual(other: Any): Boolean = { other.isInstanceOf[Displacement] }
	override def equals(other:Any):Boolean =
	{
		other match {
		  case other: Displacement =>
		    {
		      //the "2 *" is for 95% certainty
		      (other canEqual this) &&
              this.x == other.x && this.y == other.y
		    }
          case _ => false
		}
	}

    def - (that: Displacement): Displacement = {
        new Displacement(this.x - that.x,this.y-that.y)
    }

    def inverse(): Displacement = {
        Displacement(-1*this.x, -1*this.y)
    }
    
    def toIntInt(): (Int,Int) = {
    	(x.value.toInt,y.value.toInt)
    }
}
case class Coordinate(x: Int, y: Int) 

case class CollectiveObstacle(val obstacleType: Int,private var relations: mutable.Map[Displacement,CollectiveObstacle], sensorArea: List[Coordinate])
{
	private[this] var exploredArea = new QuadBitSet
	addExploredArea(sensorArea)
	
	def insideSavedBoundaries(vector: Displacement): Boolean = {
		insideSavedBoundaries(vector.x.value.toInt,vector.y.value.toInt  )
	}
	
	def insideSavedBoundaries(x: Int, y: Int): Boolean = {
		exploredArea.contains(x,y)
	}
	
	def addExploredArea(area: Seq[Coordinate]){
		for(exploredSquare <- area){
			val Coordinate(x,y) = exploredSquare
			exploredArea.add(x, y)
		}
	}
	
	def explored(x: Int, y: Int): Boolean = {
		exploredArea.contains(x,y)
	}
	
	def addRelations(newRelations: (Displacement,CollectiveObstacle) *){
		relations ++= newRelations
	}
	
	def update(newRelations: mutable.Map[Displacement,CollectiveObstacle], area: Seq[Coordinate]){
		addRelations(newRelations.toSeq: _*)
		addExploredArea(area)
	}
	
	def containsRelation(vector: Displacement, relationObjectType: Int): Boolean = {
		relations.get(vector) match {
			case None => return false
			case Some(collectiveObject) => {collectiveObject.obstacleType == relationObjectType
				
			}
		}
	}
	
	def possibleMatch(relationsToCheck: List[(Displacement,Int)], insideDetectedBoundaries: => Boolean): Boolean = {
		relationsToCheck.foreach(relation => {
				val (vector,obstacleType) = relation
				if(insideSavedBoundaries(vector)){
					if(containsRelation(vector,obstacleType))
						return false
				}
			}//end function
		)//end foreach
		return true
	}
}

case class AgentWithLocation(agent: Agent, x: Int, y: Int){
	override def toString = "AgentWithLocation: agent="+agent+" x="+x+" y="+y
}

case class AgentUpdate(oldX: Int, oldY: Int, newX: Int, newY: Int){
    override def toString = " x=" +oldX+ " y=" +oldY+ " x=" +newX+ " y=" +newY
}

case class Obstacle(obstacleType: Int, x: Double, y: Double){
	override def toString = "Obstacle: obstacleType="+obstacleType+" x="+x+" y="+y
}

case class MoveCommand(sender: Agent, x: Int, y: Int )
case class UpdateSensor(sender: Agent, range: Double, sensorDeltaAngle: Double, sensorRange: Double)
case class ObjectReading(angle: Measurement, distance: Measurement, obstacleType:Int)
case class AgentReading(angle: Measurement, distance: Measurement) 
case class Scan(scannedArea: QuadBitSet,detectedObstacles: List[ObjectReading],detectedAgents: List[AgentReading])

case class Goal(goal:Obstacle)
/*


case class TopologicalEntry(obstacle1Type: Int,obstacle2Type: Int,
                            deltaX: Measurement, deltaY: Measurement){
    def inverse() : TopologicalEntry = {
        new TopologicalEntry(obstacle2Type,obstacle1Type,-1*deltaX,-1*deltaY)
    }
}
case class IdentifiedObject(identifier1: Int, identifier2: Int, 
                            //obstacle1Type: Int,obstacle2Type: Int,
                            vector: Displacement){
    def inverse: IdentifiedObject = {
        IdentifiedObject(identifier2,identifier1,vector.inverse)
    }
}
 

case class Move(agent: Actor, x: Measurement, y: Measurement) 
 


case class FindGoal(obstacleType: Int,rootIdentifier: Int)
case class GoalNotFound()

case class Relationship(identifier1Temp: Int, identifier2Temp: Int, vector: Displacement )
case class InternalIdentifiedObject(name: Int, objectType: Int, vectorFromAgent: Displacement)
case class IdentifiedObjects(lastUpdate: Long, identifiedObjects: List[IdentifiedObject])

case class CollectiveMapSize(size: Int, lastUpdate: Long)

case class PossibleMatches(	lastUpdate: Long,matches : List[IdentifiedStored], entries: List[Relationship],
							cartesianObjectReadings: List[InternalIdentifiedObject], sensorRange: Double)
case class PickName(identifier: Int, obstacleType: Int)
case class RemoveName(identifier: Int)
case class MapSize()
case class Size(size: Int)
case class GetIdentifierType(identifier: Int)
case class Add(	lastUpdate: Long,identifiedObjects: List[IdentifiedObject],
				cartesianObjectReadings: List[InternalIdentifiedObject], sensorRange: Double)
case class Contains(identifiers: Identifiers)
case class Matches(entries: TopologicalEntry *)
case class IdentifierType(identifier: Int,objectType: Int)
case class noType(identifier: Int)

case class TargetDisplacement(x: Measurement, y: Measurement)
case class TimeSinceLastUpdate(time: Long)
case class Identifiers(a: Int, b: Int){
    def inverse: Identifiers = Identifiers(b,a)
}
case class MatchRelationships(identifier1Type: Int,identifier2Type: Int,
	relationshipsDisplacements: List[Displacement],entries: List[Relationship],
	cartesianObjectReadings: List[InternalIdentifiedObject], sensorRange: Double)
case class IdentifiedStored(idObject:IdentifiedObject) extends Ordered[IdentifiedStored]{
    private val dispX: Measurement = idObject.vector.x
    private val dispY: Measurement = idObject.vector.y
    val dSquared = dispX*dispX + dispY*dispY

    def compare(that : IdentifiedStored) : Int ={
        val thisX = this.idObject.vector.x
        val thisY = this.idObject.vector.y
        val thatX = that.idObject.vector.x
        val thatY = that.idObject.vector.y
        if( (thisX == thatX) && (thisY == thatY) )
            return 0
        else if(this.dSquared < that.dSquared)
            return -1
        else
            return 1
    }

}
case class RelationshipStored(vector: Displacement) extends Ordered[RelationshipStored]
{
    private val x = vector.x
    private val y = vector.y
    val dSquared = x*x + y*y
    
    override def toString = "RelationshipStored("+ vector + "," + dSquared + ") "
    
    def compare(that : RelationshipStored) : Int ={
        val thatX = that.vector.x
        val thatY = that.vector.y
        if( (x == thatX) && (y == thatY) )
            return 0
        else if(this.dSquared < that.dSquared)
            return -1
        else
            return 1
    }
    
    def inverse(): RelationshipStored = RelationshipStored(vector.inverse)
}

case class GetRelationsForIdentifier(identifier: Int)
case class SensorReadings(sensorRange: Double, sensorReadings: List[ObjectReading])
case class ObjectReadingCartesian(obstacleType: Int, sensorVector: Displacement)
case class RecheckRelationships(reconstructedTopEntries: List[TopologicalEntry],entries: List[Relationship],
								cartesianObjectReadings: List[InternalIdentifiedObject], sensorRange: Double)
case class SensorRelations(sensorReadings: List[ObjectReading],cartesianReadings: List[InternalIdentifiedObject], entries: List[Relationship],sensorRange: Double)
case class RecheckObjects(identifiedObjects: List[IdentifiedObject],
                          cartesianObjectReadings: List[InternalIdentifiedObject], sensorRange: Double)
case class NewIdentifiedObjects(lastUpdate: Long, identifiedObjects: List[IdentifiedObject],
								cartesianObjectReadings: List[InternalIdentifiedObject], sensorRange: Double)
case class MapIdentifiedObjects(lastUpdate: Long, identifiedObjects: List[IdentifiedObject],
                                cartesianObjectReadings: List[InternalIdentifiedObject],sensorRange: Double)
{
	override def toString = "MapIdentifiedObjects: lastUpdate= " + 
			lastUpdate + "identifiedObjects= " + identifiedObjects
}
*/