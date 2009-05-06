import scala.actors._
import Actor._
//import java.util._

/*//The following commented code is for reference only
case class TopologicalEntry(obstacle1Type: Int,obstacle2Type: Int,
                            deltaX: Measurement, deltaY: Measurement)
case class IdentifiedObject(identifier1: Int, identifier2: Int,
                            obstacle1Type: Int,obstacle2Type: Int,
                            deltaX: Measurement, deltaY: Measurement)
*/
case class Add(identifiedObject: IdentifiedObject)
case class Contains(entries: TopologicalEntry *)
case class TargetDisplacement(x: Measurement, y: Measurement)
case class TimeSinceLastUpdate(time: Long)

class TopologicalElementGenerator(val map: Actor) extends Actor{
	def act()
	{
		println("TopologicalElementGenerator Running")
		loop
		{
			react
			{
              case sensorReadings @ List(ObjectReading,_*) =>{
                      //convert to topological entries and reply(entries)
              }

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
                 val xDisplacementToGoal = new Measurement(0)//temporary
                 val yDisplacementToGoal = new Measurement(0)//temporary
                 var foundGoal: Boolean = true
                 if(foundGoal)
                    agent ! TargetDisplacement(xDisplacementToGoal,yDisplacementToGoal)
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
	var updateTime = System.currentTimeMillis()
    private var primitiveDataStructure: List[IdentifiedObject] = Nil.asInstanceOf[List[IdentifiedObject]]

    def add(relationship: IdentifiedObject):Boolean = {
        updateTime = System.currentTimeMillis()
        //add relationship to data structure
        return contains(relationship)
    }

    def contains(relationship: IdentifiedObject):Boolean = {
        return true//check that relationship exists in data structure
    }

    def matches(entries: TopologicalEntry *):List[IdentifiedObject] = {
        return primitiveDataStructure//really return the part of the data structure that matches all the topological entry constrains
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
              case Contains(entries) => {
                   reply(matches(entries))
              }
              case "lastUpdate" =>
                  reply(TimeSinceLastUpdate(System.currentTimeMillis()-updateTime))
              case "Exit" => {
                 println("CollectiveMap Exiting")
                 this.exit
              }//end case Exit
			}//end react
		}//end loop
	}//end act
}