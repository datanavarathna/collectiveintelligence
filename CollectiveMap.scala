import scala.actors._
import Actor._
import java.util.ArrayList
import Measurement._

/*//The following commented code is for reference only
case class TopologicalEntry(obstacle1Type: Int,obstacle2Type: Int,
                            deltaX: Measurement, deltaY: Measurement)
case class IdentifiedObject(identifier1: Int, identifier2: Int,
                            //obstacle1Type: Int,obstacle2Type: Int,
                            vector: Displacement)
case class ObjectReading(angle: Measurement, distance: Measurement, obstacleType:Int)
case class Displacement(x: Measurement, y: Measurement)
*/
case class Add(identifiedObject: IdentifiedObject)
case class Contains(entries: TopologicalEntry *)
case class TargetDisplacement(x: Measurement, y: Measurement)
case class TimeSinceLastUpdate(time: Long)
case class Identifiers(a: Int, b: Int)
case class IdentifiedStored(idObject:IdentifiedObject, dSquared: Measurement) extends Ordered[IdentifiedStored]{
    def this(idObject:IdentifiedObject) = {
        this(idObject,
            {
                val dispX: Measurement = idObject.vector.x
                val dispY: Measurement = idObject.vector.y
                dispX*dispX + dispY*dispY
            })
    }//end auxillary constructor, doesn't seem to be detected

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

class TopologicalElementGenerator(val map: Actor) extends Actor{
	
    def PolarToCartesian(angle: Measurement, distance: Measurement): Displacement =  {
      Displacement(distance * cos(angle), distance * sin(angle))
    }
    
    def topologicalGenerator(readingA: ObjectReading,readingB: ObjectReading): TopologicalEntry = {
        val aVector: Displacement = PolarToCartesian(readingA.angle,readingA.distance)
        val bVector: Displacement = PolarToCartesian(readingB.angle,readingB.distance)
        val baVector: Displacement = aVector - bVector
        new TopologicalEntry(readingB.obstacleType, readingA.obstacleType,
                baVector.x, baVector.y
            )
    }

    def act()
	{
		println("TopologicalElementGenerator Running")
		loop
		{
			react
			{
              case sensorReadings @ List(ObjectReading,_*) =>{
                      //convert to topological entries and reply(entries)
                      var entries: List[TopologicalEntry] = Nil
                      var readings: List[ObjectReading]  = sensorReadings.asInstanceOf[List[ObjectReading]]
                      while(!readings.isEmpty){
                          val readingA = readings.head
                          readings = readings.tail
                          for(readingB <- readings){
                              entries = topologicalGenerator(readingA,readingB) :: entries
                          }
                      }//end while
                      reply(entries)
              }//end case

                case "Exit" => {
                 println("TopologicalElementGenerator Exiting")
                 this.exit
              }
			}//end react
		}//end loop
	}//end act
}

class RelationshipIdentfier(val map: Actor) extends Actor{


    def act()
	{
		println("RelationshipIdentfier Running")
		loop
		{
			react
			{
              case topologicalEntries @ List(TopologicalEntry, _*) => {
                      //reply()//convert topological entries to identified objects and send in reply
              }
              case "Exit" => {
                 println("RelationshipIdentfier Exiting")
                 this.exit
              }
			}//end react
		}//end loop
	}//end act
}

class GoalFinder(val agent: Actor, val map: Actor) extends Actor
{
     def act()
	{
		println("GoalFinder Running")
		loop
		{
			react
			{
              case Goal(goal) => {
                 //find goal
                 val xDisplacementToGoal = 2//temporary
                 val yDisplacementToGoal = 1//temporary
                 var foundGoal: Boolean = true
                 if(foundGoal){
                      //agent ! TargetDisplacement(xDisplacementToGoal,yDisplacementToGoal)
                      agent ! List(Coordinate(xDisplacementToGoal,yDisplacementToGoal))
                 }
                 else
                    agent ! GoalNotFound()
              }
              case "Exit" => {
                 println("Goalfinder Exiting")
                 this.exit
              }
			}//end react
		}//end loop
	}//end act
}

class CollectiveMap extends Actor{
	private var updateTime = System.currentTimeMillis()

    import scala.collection.mutable.Map
    import scala.collection.jcl.TreeMap
    import scala.collection.jcl.TreeSet
    private var relationshipsLookup = Map.empty[Identifiers,Displacement]//key,value
    private var obstacle1Map = Map.empty[Int,TreeMap[Int,TreeSet[IdentifiedStored]]]//key,value [obstacleType1]

    /*
    private def findRelationships(identifier1: Int, identifier2: Int):TreeSet[IdentifiedStored] = {
        var obstacle2Map = obstacle1Map.getOrElse(identifier1,
            new TreeMap[Int,TreeSet[IdentifiedStored]])
        obstacle2Map.getOrElse(identifier2,
            new TreeSet[IdentifiedStored])
    }*/
    //might not handle inverse vectors properly
    def add(relationship: IdentifiedObject): Boolean = {
        updateTime = System.currentTimeMillis()

        val identifier1 = relationship.identifier1
        val identifier2 = relationship.identifier2
        val identifiers = Identifiers(identifier1,identifier2)

        var obstacle2Map = obstacle1Map.getOrElse(identifier1,
            new TreeMap[Int,TreeSet[IdentifiedStored]])
        var relationships = obstacle2Map.getOrElse(identifier2,
            new TreeSet[IdentifiedStored])

        val dispX: Measurement = relationship.vector.x
        val dispY: Measurement = relationship.vector.y
        val addItem = IdentifiedStored(relationship,dispX*dispX + dispY*dispY)
        
        relationships += addItem
        if(relationships.has(addItem))//boolean to check if successful
        {
            obstacle2Map.put(identifier2,relationships)
            if(obstacle2Map.contains(identifier2))
            {
                obstacle1Map.put(identifier1,obstacle2Map)
                if(obstacle2Map.contains(identifier1))
                {
                    relationshipsLookup += (identifiers -> relationship.vector)
                    if(contains(identifiers)){
                        return true
                    }
                    else{
                        println("Failed to add identifiers to relationshipsLookup")
                        return false
                    }//end add to relationshipsLookup check
                }
                else{
                    println("Failed to add element to obstacle1Map")
                    return false
                }//end add to obstacle1Map check
            }
            else{
                println("Failed to add element to obstacle2Map")
                return false
            }//end add to obstacle2Map check
        }
        else{
            println("Failed to add element to relationships")
            return false
        }//end add to relationships check
    }//end add
    //might not handle inverse vectors properly
    def delete(relationship: IdentifiedObject): Boolean = {
       val identifier1 = relationship.identifier1
        val identifier2 = relationship.identifier2
        val identifiers = Identifiers(identifier1,identifier2)

        if(contains(identifiers))
        {
           var obstacle2Map = obstacle1Map.getOrElse(identifier1,
            new TreeMap[Int,TreeSet[IdentifiedStored]])
           var relationships = obstacle2Map.getOrElse(identifier2,
            new TreeSet[IdentifiedStored])

            val dispX: Measurement = relationship.vector.x
            val dispY: Measurement = relationship.vector.y
            val addItem = IdentifiedStored(relationship,dispX*dispX + dispY*dispY)

            relationships -= addItem
            if(!relationships.has(addItem))//boolean to check if successful
            {
                obstacle2Map.put(identifier2,relationships)
                if(!obstacle2Map.contains(identifier2))
                {
                    obstacle1Map.put(identifier1,obstacle2Map)
                    if(!obstacle2Map.contains(identifier1))
                    {
                        relationshipsLookup -= (identifiers)
                        if(!contains(identifiers)){
                            return true
                        }
                        else{
                            println("Failed to remove identifiers to relationshipsLookup")
                            return false
                        }//end add to relationshipsLookup check
                    }
                    else{
                        println("Failed to remove element to obstacle1Map")
                        return false
                    }//end add to obstacle1Map check
                }
                else{
                    println("Failed to remove element to obstacle2Map")
                    return false
                }//end add to obstacle2Map check
            }
            else{
                println("Failed to remove element to relationships")
                return false
            }//end add to relationships check
        }
        else
            return true//nothing to delete
    }//end delete

    def contains(identifiers: Identifiers):Boolean = {
        if(relationshipsLookup.contains(identifiers))
            return true
        else{
            val inverseIdentifiers = Identifiers(identifiers.b,identifiers.a)
            return relationshipsLookup.contains(inverseIdentifiers)
        }
    }

    def matches(entries: TopologicalEntry *):List[IdentifiedObject] = {
        Nil.asInstanceOf[List[IdentifiedObject]]
    }

    //always check contains before calling
    def getRelationship(relationship: Identifiers):IdentifiedObject = {
    	relationshipsLookup.get(relationship) match {
            case Some(vector) => {
                    IdentifiedObject(relationship.a,relationship.b,vector)
            }
            case None => {
                   relationshipsLookup.get(relationship) match {
                      case Some(vector) => {
                        IdentifiedObject(relationship.a,relationship.b,vector.inverse)
                        }
                        case None => {
                            println("No relationship found")
                            IdentifiedObject(0,0,Displacement(0,0))
                        }
                   }//end 2nd match
            }//end case None
        }
    }

    //if the map is empty, objects need to be assigned random identifiers and added to map

    def act()
	{
		println("CollectiveMap Running")
		loop
		{
			react
			{
              case Add(identifiedObject) => {
                    if(add(identifiedObject))
                        print("Failed to add object")
              }
/*              case Contains(entries) => {
                   reply(matches(entries))
              }
*/              case "lastUpdate" =>
                  reply(TimeSinceLastUpdate(System.currentTimeMillis()-updateTime))
              case "Exit" => {
                 println("CollectiveMap Exiting")
                 this.exit
              }//end case Exit
			}//end react
		}//end loop
	}//end act
}