import scala.actors._
import Actor._
import java.util.ArrayList

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
	var updateTime = System.currentTimeMillis()
    private var primitiveDataStructure: List[IdentifiedObject] = Nil.asInstanceOf[List[IdentifiedObject]]
    var collective = new CollectiveMapJava();
    var idNum = 1;

    def add(relationship: IdentifiedObject):Boolean = {
        updateTime = System.currentTimeMillis()
        val o2 = new IdentifiedObjectJava();
        o2.id1 = relationship.identifier1;
        o2.id2 = relationship.identifier2;
        o2.obstacle1Type = relationship.obstacle1Type;
        o2.obstacle2Type = relationship.obstacle2Type;
        o2.deltaX = new MeasurementJava();
        o2.deltaX.meas = relationship.deltaX.value;
        o2.deltaX.uncertainty = relationship.deltaX.uncertainty;
        o2.deltaY = new MeasurementJava();
        o2.deltaY.meas = relationship.deltaY.value;
        o2.deltaY.uncertainty = relationship.deltaY.uncertainty;
        collective.add(o2);
        id++;
        return contains(relationship)
    }

    def contains(relationship: IdentifiedObject):Boolean = {
        val o2 = new IdentifiedObjectJava();
	o2.id1 = relationship.identifier1;
	o2.id2 = relationship.identifier2;
	o2.obstacle1Type = relationship.obstacle1Type;
	o2.obstacle2Type = relationship.obstacle2Type;
	o2.deltaX = new MeasurementJava();
	o2.deltaX.meas = relationship.deltaX.value;
	o2.deltaX.uncertainty = relationship.deltaX.uncertainty;
	o2.deltaY = new MeasurementJava();
	o2.deltaY.meas = relationship.deltaY.value;
        o2.deltaY.uncertainty = relationship.deltaY.uncertainty;
       	return collective.contains(o2);
    }

    def matches(entries: TopologicalEntry *):List[IdentifiedObject] = {
        val o2 = new IdentifiedObjectJava();
	o2.obstacle1Type = relationship.obstacle1Type;
	o2.obstacle2Type = relationship.obstacle2Type;
	o2.deltaX = new MeasurementJava();
	o2.deltaX.meas = relationship.deltaX.value;
	o2.deltaX.uncertainty = relationship.deltaX.uncertainty;
	o2.deltaY = new MeasurementJava();
	o2.deltaY.meas = relationship.deltaY.value;
        o2.deltaY.uncertainty = relationship.deltaY.uncertainty;
        var al  = collective.relationshipSearch(o2);
        var i = 0;
        while(i < al.size()){
        	var io: IdentifiedObject = IdentifiedObjectJavatoIdentifiedObject(al.Item(i));
        	primativeDataStructure.add(io);
        	i += 1;
        }
        return primitiveDataStructure;
    }
    
    def IdentifiedObjectJavatoIdentifiedObject(relationship: IdentifiedObjectJava):IdentifiedObject = {
    	var iO2 = new IdentifiedObject();
    	iO2.identifier1 = relationship.id1;
    	iO2.identifier2 = relationship.id2;
    	iO2.obstacle1Type = relationship.obstacle1Type;
    	iO2.obstacle2Type = relationship.obstacle2Type;
    	iO2.deltaX = new Measurement();
    	iO2.deltaX.value = relationship.deltaX.meas;
    	iO2.deltaX.uncertainty = relationship.deltaX.uncertainty;
    	iO2.deltaY = new Measurement();
	iO2.deltaY.value = relationship.deltaY.meas;
    	iO2.deltaY.uncertainty = relationship.deltaY.uncertainty;
    	return iO2;
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