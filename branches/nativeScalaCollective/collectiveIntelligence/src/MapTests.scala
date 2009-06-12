import scala.actors._
import Actor._


object MapTests {
	def main(args : Array[String]) = 
   {
		val agent = actor {
			loop {
				println("Running")
				react {
				case msg =>
				println("received message: "+ msg)
				}//end receive
			}//end while
		}//end actor
     
     var sensorRange: Double = 10
     var collectiveMap = new CollectiveMap
     var goalFinder = new GoalFinder( agent, collectiveMap)
     collectiveMap.start
     goalFinder.start
     //agent.start
     
     Thread.sleep(300)//msec
     var updateTime: Long = System.currentTimeMillis();
     
     var identifierdObjects: List[IdentifiedObject] = Nil
     var cartesianReadings: List[InternalIdentifiedObject] = Nil
     
     identifierdObjects = IdentifiedObject(1,2,Displacement(1,1)) :: identifierdObjects
     identifierdObjects = IdentifiedObject(1,3,Displacement(0,8)) :: identifierdObjects
     identifierdObjects = IdentifiedObject(1,4,Displacement(-3,8)) :: identifierdObjects
     identifierdObjects = IdentifiedObject(2,3,Displacement(-1,7)) :: identifierdObjects
     identifierdObjects = IdentifiedObject(2,4,Displacement(-4,7)) :: identifierdObjects
     identifierdObjects = IdentifiedObject(3,4,Displacement(-3,0)) :: identifierdObjects
     
     identifierdObjects = IdentifiedObject(5,4,Displacement(3,4)) :: identifierdObjects
     identifierdObjects = IdentifiedObject(5,1,Displacement(6,-4)) :: identifierdObjects
     
     cartesianReadings = InternalIdentifiedObject(1,1,Displacement(0,-2)) :: cartesianReadings
     cartesianReadings = InternalIdentifiedObject(2,1,Displacement(-1,-1)) :: cartesianReadings
     cartesianReadings = InternalIdentifiedObject(3,1,Displacement(0,6)) :: cartesianReadings
     cartesianReadings = InternalIdentifiedObject(4,1,Displacement(-3,6)) :: cartesianReadings
     
     cartesianReadings = InternalIdentifiedObject(5,2,Displacement(-6,2)) :: cartesianReadings
     
     actor {
          collectiveMap ! PickName(1,1)
          collectiveMap ! PickName(2,1)
          collectiveMap ! PickName(3,1)
          collectiveMap ! PickName(4,1)
          collectiveMap ! PickName(5,2)
          
          Thread.sleep(2000)//msec
          
          collectiveMap ! Add(updateTime,identifierdObjects,cartesianReadings,sensorRange)
          
          Thread.sleep(2000)//msec
          import scala.collection.mutable.Map
          //println(collectiveMap)
          /*
          collectiveMap !? GetRelationsForIdentifier(2)match 
		  {
		     case relations: Map[_,_]=> println(relations)
              case catchall => println("failed: "+ catchall)
          }*/
          goalFinder ! FindGoal(2,2)
          Thread.sleep(20000)//msec
          collectiveMap ! "Exit"
          goalFinder ! "Exit"
          agent.exit
          }
   }
}
