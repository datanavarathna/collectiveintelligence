import scala.actors._
import Actor._
import java.util.ArrayList
import Measurement._
//import CaseClasses._

class TopologicalElementGenerator(relationshipIdentfier: RelationshipIdentfier) extends Actor{
	
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
              case sensorReadings @ List(ObjectReading(_,_,_),_*) =>{
            	  	  println("Received OjectReadings from Agent")
                
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
                      relationshipIdentfier ! entries //send List[TopologicalEntry] to relationshipIdentfier
              }//end case

                case "Exit" => {
                 println("TopologicalElementGenerator Exiting")
                 this.exit
              }
			}//end react
		}//end loop
	}//end act
}
