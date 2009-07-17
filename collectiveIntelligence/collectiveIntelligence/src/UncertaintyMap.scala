//import Measurement._
import scala.collection.jcl.TreeMap
import scala.collection.mutable.Map
import scala.collection.mutable.HashSet

class UncertaintyMap[E] {
  
	var lowestMeasurements = new TreeMap[Double,RelationshipStored]
    var highestMeasurements = new TreeMap[Double,RelationshipStored]
    var map = Map.empty[RelationshipStored,E]

    override def toString = 
    {
        "UncertaintyMap:\n" + map.toString + 
          "\n lowest: " + lowestMeasurements.toString +
          "\n highest: " + highestMeasurements.toString
    	
    }
    
    def += (kv: (RelationshipStored, E))
    {
		val relationStored: RelationshipStored = kv._1
		val e: E = kv._2
		val value: Double = relationStored.dSquared.value
		val omega: Double = 2 * relationStored.dSquared.uncertainty
		var key: Double = value - omega
		lowestMeasurements += ( key -> relationStored )
		key = value + omega
		highestMeasurements += ( key -> relationStored )
		map += (relationStored -> e)
    }
    
    def keys() : Iterator[RelationshipStored] =
    {
       map.keys 
    }
    
    def contains(relationStored: RelationshipStored): Boolean = 
    {
        map.contains(relationStored)
    }
    
    def put(relationStored: RelationshipStored, e: E): Option[E] = 
    {
        map.put(relationStored,e)
    }

    def get(relationStored: RelationshipStored): Option[E] =
    {
        map.get(relationStored)
    }
    
    def getAllEquals(relationStored: RelationshipStored): List[E] = 
    {
        var results = new HashSet[E]
    	val value: Double = relationStored.dSquared.value
		val omega: Double = 2 * relationStored.dSquared.uncertainty
		println("value: " + value + " omega: " + omega)
		var lowerKey: Double = value - omega
		var higherKey: Double = value + omega+ 0.0000001//constant added because rangeImpl is upperBound exclusive
		println("Getting lowestMeasurements from " + lowerKey + " to " + higherKey)
		var lowestKeys = lowestMeasurements.rangeImpl(Some(lowerKey),Some(higherKey))
		println("lowestKeys: " + lowestKeys)
		//key = value + omega + 0.0000001//constant added because rangeImpl is upperBound exclusive
		println("Getting highestMeasurements from " + lowerKey + " to " + higherKey)
		var highestKeys = highestMeasurements.rangeImpl(Some(lowerKey), Some(higherKey))
		println("highestKeys: " + highestKeys)
		var lowerIterator = lowestKeys.values
		println("while(lowerIterator.hasNext)")
		while(lowerIterator.hasNext)
		{
			val element: RelationshipStored = lowerIterator.next
			println("element: " + element)
			if(element == relationStored)//should be true in most cases
			{
				println("element == relationStored")
			    //Add element to results
				map.get(element) match
				{
				   case Some(e) => { results += e }
				   case None => { println("Error: " + element + " not found in UncertaintyMap")}//Should never execute
				}
			}//end if element == relationStored
		}//end while lowestIterator.hasNext
		var higherList: List[RelationshipStored] = highestKeys.values.toList
		higherList = higherList.reverse
		println("reversed higherList: " + higherList)
		while(!higherList.isEmpty)
		{
			var element = higherList.head
			if(element == relationStored)//should be true in most cases
			{
				println("higherList.head == relationStored")
					println("element: " + element)
					map.get(element) match
					{
						case Some(e) => { results += e }
						case None => { println("Error: " + element + " not found in UncertaintyMap")}//Should never execute
					}//end match
			}//end if (higherList.head == relationStored)
			//println("Removing first element from list")
			higherList = higherList.tail
		}//end while iterating through higherList
    	return results.toList
    }
}
