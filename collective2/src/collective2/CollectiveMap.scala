package collective2

import scala.actors._
import Actor._
import java.util.ArrayList
import uncertaintyMath.Measurement
import uncertaintyMath.Measurement._
import timestampConcurrency._

class CollectiveMap extends Actor with TimeStampConcurrency
{
	
	private var updateTime: Long = System.currentTimeMillis()
	private var size: Int = 0

    import scala.collection.mutable.Map
    //import scala.collection.mutable.HashMap
    //import scala.collection.mutable.HashSet
    //import scala.collection.jcl.TreeMap
    //import scala.collection.jcl.TreeSet
    //private var relationshipsLookup = Map.empty[Identifiers,Displacement]//key,value
    private var identifierType = Map.empty[Int,Int]// (identifier -> type)
    //private var obstacle1TypeMap = Map.empty[Int,HashMap[Int,UncertaintyMap[Identifiers]]]//key,value obstacleType1->obstacleType2->relationship->Identifiers
    //private var identifierRelationshipsGraph = Map.empty[Int,UncertaintyMap[Int]]//identifier1->relationships->identifier2
    
    override def toString = {
    		var result = "Collective Map \n"
    		result += "\n  identifierType\n" + identifierType
    		result
    }
    
    private def getSize(): CollectiveMapSize =
    {
        new CollectiveMapSize(size,updateTime)
    }
    
    private def pickName(identifier: Int, obstacleType: Int): Boolean =
    {
        if(!identifierType.contains(identifier))
        {
        	identifierType += (identifier -> obstacleType)
        	return identifierType.contains(identifier)
        }
        else
        {
          println("Name exists for :" + identifier)
          return false
        }
    }
    
    private def removeName(identifier: Int): Boolean = {
    	identifierType -= (identifier)
    	return !identifierType.contains(identifier)
    }
    
    private def getIdentifierType(identifier: Int): Option[Int] = 
    {
    		identifierType.get(identifier)
    }

    private def add(): Boolean = 
    {
    	false
    }

    
    def possibleMatchesGraph():  Any = 
    {
    	
    }
    
    //if the map is empty, objects need to be assigned random identifiers and added to map

    def act()
	{
		println("CollectiveMap Running")
		
		loop
		{
			react{
				case PickName(identifier,obstacleType) => {
				  if(pickName(identifier,obstacleType))
				  {
					  println("Added name " + identifier + " with type " + obstacleType)
					  reply("Added name")
				  }
				  else
				  {
					  println("Failed to add name")
					  reply("Failed to add name")
				  }
			  }
			  case RemoveName(identifier) => {
				  if(removeName(identifier))
					  println("Successfully removed name")
				  else
					  println("Failed to remove " + identifier)
			  }
			  case MapSize => {
				  println("Map size: " + size)
				  reply(Size(size))
			  }
			  case GetIdentifierType(identifier: Int) => {
				  getIdentifierType(identifier) match {
				    case Some(objectType) => reply(IdentifierType(identifier,objectType))
				    case None => reply(noType(identifier))
				  }
			  }
			}
		}
		
	}//end act
	
}