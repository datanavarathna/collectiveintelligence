import scala.actors._
import Actor._
import java.util.ArrayList
import Measurement._

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
                 if(foundGoal){//retrun list of displacements as instructions on path to goal
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