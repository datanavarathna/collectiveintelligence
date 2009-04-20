import scala.actors._
import Actor._

class TopologicalElementGenerator extends Actor{
	def act()
	{
		println("TopologicalElementGenerator Running")
		loop
		{
			react
			{
              case "Exit" => {
                 println("TopologicalElementGenerator Exiting")
                 this.exit
              }
			}//end react
		}//end loop
	}//end act
}

class RelationshipIdentfier extends Actor{
	def act()
	{
		println("RelationshipIdentfier Running")
		loop
		{
			react
			{
              case "Exit" => {
                 println("RelationshipIdentfier Exiting")
                 this.exit
              }
			}//end react
		}//end loop
	}//end act
}

class CollectiveMap extends Actor{
	def act()
	{
		println("CollectiveMap Running")
		loop
		{
			react
			{
              case "Exit" => {
                 println("CollectiveMap Exiting")
                 this.exit
              }
			}//end react
		}//end loop
	}//end act
}