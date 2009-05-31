import scala.actors._
import Actor._

object UnitTests {
	def main(args : Array[String]) = 
   {	
     //Test Measurement
     	println("Int to Measurement Test")
        var temp1: Measurement = -1
        var temp2: Measurement = -1
        if(temp1 != temp2)
            println(temp1 +" != " + temp2 +" failed")
        if(temp1 == temp2)
            println(temp1 +" == " + temp2 +" correct")
        println("Double to Measurement Test")
        temp1 = -1.0
        temp2 -1.0
        if(temp1 != temp2)
            println(temp1 +" != " + temp2 +" failed")
        if(temp1 == temp2)
            println(temp1 +" == " + temp2 +" correct")
        
        println("Equality Test")
        temp1 = new Measurement(8,1)
        temp2 = new Measurement(10,1)
        if(temp1 != temp2)
            println(temp1 +" != " + temp2 +" failed")
        if(temp1 == temp2)
            println(temp1 +" == " + temp2 +" correct")
        print("Equality Test")
        temp1 = 3
        if(temp1 != 3)
        	println(" Failed: temp1 != 3")
        else
            print(" Correct \n")
        print("Equality Test")
        temp1 = 3.0
        if(temp1 != 3.0)
        	println(" Failed: temp1 != 3.0")
        else
            print(" Correct \n")
        println("ln Test")
        temp1 = Measurement.ln(3.0)
        temp2 = Math.log(3.0) / Math.log(Math.E)
        if(temp1 != temp2)
            println(temp1 +" != " + temp2 +" failed")
        if(temp1 == temp2)
            println(temp1 +" == " + temp2 +" correct")
        println("sqrt Test")
        temp1 = Measurement.sqrt(3.0)
        temp2 = Math.sqrt(3.0)
        if(temp1 != temp2)
            println(temp1 +" != " + temp2 +" failed")
        if(temp1 == temp2)
            println(temp1 +" == " + temp2 +" correct")
        println("sin Test")
        temp1 = Measurement.sin(3.0)
        temp2 = Math.sin(3.0)
        if(temp1 != temp2)
            println(temp1 +" != " + temp2 +" failed")
        if(temp1 == temp2)
            println(temp1 +" == " + temp2 +" correct")
        println("cos Test")
        temp1 = Measurement.cos(3.0)
        temp2 = Math.cos(3.0)
        if(temp1 != temp2)
            println(temp1 +" != " + temp2 +" failed")
        if(temp1 == temp2)
            println(temp1 +" == " + temp2 +" correct")
        println("tan Test")
        temp1 = Measurement.tan(3.0)
        temp2 = Math.tan(3.0)
        if(temp1 != temp2)
            println(temp1 +" != " + temp2 +" failed")
        if(temp1 == temp2)
            println(temp1 +" == " + temp2 +" correct")
        println("asin Test")
        temp1 = Measurement.asin(0.7)
        temp2 = Math.asin(0.7)
        if(temp1 != temp2)
            println(temp1 +" != " + temp2 +" failed")
        if(temp1 == temp2)
            println(temp1 +" == " + temp2 +" correct")
        println("acos Test")
        temp1 = Measurement.acos(0.7)
        temp2 = Math.acos(0.7)
        if(temp1 != temp2)
            println(temp1 +" != " + temp2 +" failed")
        if(temp1 == temp2)
            println(temp1 +" == " + temp2 +" correct")
        println("atan Test")
        temp1 = Measurement.atan(3.0)
        temp2 = Math.atan(3.0)
        if(temp1 != temp2)
            println(temp1 +" != " + temp2 +" failed")
        if(temp1 == temp2)
            println(temp1 +" == " + temp2 +" correct")
        println("atan2 Test")
        temp1 = Measurement.atan2(
        	new Measurement(0),new Measurement(1)
        )
        temp2 = Math.atan2(0,1)
        if(temp1 != temp2)
            println(temp1 +" != " + temp2 +" failed")
        if(temp1 == temp2)
            println(temp1 +" == " + temp2 +" correct")
        println("atan2 Test")
        temp1 = Measurement.atan2(
        	new Measurement(0),new Measurement(-1)
        )
        temp2 = Math.atan2(0,-1)
        if(temp1 != temp2)
            println(temp1 +" != " + temp2 +" failed")
        if(temp1 == temp2)
            println(temp1 +" == " + temp2 +" correct")
        println("atan2 Test")
        temp1 = Measurement.atan2(
        	new Measurement(1),new Measurement(0)
        )
        temp2 = Math.atan2(1,0)
        if(temp1 != temp2)
            println(temp1 +" != " + temp2 +" failed")
        if(temp1 == temp2)
            println(temp1 +" == " + temp2 +" correct")
        println("atan2 Test")
        temp1 = Measurement.atan2(
        	new Measurement(-1),new Measurement(0)
        )
        temp2 = Math.atan2(-1,0)
        if(temp1 != temp2)
            println(temp1 +" != " + temp2 +" failed")
        if(temp1 == temp2)
            println(temp1 +" == " + temp2 +" correct")
        println("atan2 Test")
        temp1 = Measurement.atan2(
        	new Measurement(1),new Measurement(1)
        )
        temp2 = Math.atan2(1,1)
        if(temp1 != temp2)
            println(temp1 +" != " + temp2 +" failed")
        if(temp1 == temp2)
            println(temp1 +" == " + temp2 +" correct")
        
        var a: Double = 3
        var b: Double = 2
        
        print(" - Test")
        temp1 = a
        temp2 = b
        if((temp1 - temp2) != a - b)
        	println("Failed: " + (temp1 - temp2)+" != " + (a - b) )
        else
            print(" Correct \n")
        print(" + Test")
        if((temp1 + temp2) != a + b)
        	println("Failed: " + (temp1 + temp2)+" != "+ (a + b) )
        else
            print(" Correct \n")
        print(" * Test")
        if((temp1 * temp2) != a * b)
        	println("Failed: " + (temp1 * temp2)+" != "+ (a * b) )
        else
            print(" Correct \n")
        print(" / Test")
        if((temp1 / temp2) != a / b)
        	println("Failed: " + (temp1 / temp2)+" != "+ (a / b) )
        else
            print(" Correct \n")
        print(" ^ Test")
        if((temp1 ^ temp2) != Math.pow(a,b))
        	println("Failed: " + (temp1 ^ temp2)+" != "+ Math.pow(a,b) )
        else
            print(" Correct \n")
        print(" < Test:")
        if(temp1 < temp2)
        	println("Failed")
        else
            print(" Correct \n")
        print(" > Test:")
        if(temp2 > temp1)
        	println("Failed")
        else
            print(" Correct \n")
        /*
        import scala.collection.mutable.Map
        import scala.collection.mutable.HashMap
        import scala.collection.jcl.TreeMap
        import scala.collection.jcl.TreeSet
        var map = Map.empty[Int,String]
        map += (1 -> "One")
        map += (2 -> "Two")
        println("Map")
        println(map)
        var hashmap = new HashMap[Int,String]
        hashmap += (1 -> "One")
        hashmap += (2 -> "Two")
        println("HashMap")
        println(hashmap)
        var treemap = new TreeMap[Int,String]
        treemap += (1 -> "One")
        treemap += (2 -> "Two")
        println("TreeMap")
        println(treemap)
        var treeset = new TreeSet[Int]
        treeset += 1
        treeset += 2
        println("TreeSet")
        println(treeset)
        */
        import scala.collection.jcl.TreeMap
        var uncertaintyTreeMap = new TreeMap[RelationshipStored,Int]
        uncertaintyTreeMap +=  (RelationshipStored(new Displacement(new Measurement(8,1),new Measurement(2,1))) -> 1)
        uncertaintyTreeMap +=  (RelationshipStored(new Displacement(new Measurement(10,1),new Measurement(2,1))) -> 1)
        uncertaintyTreeMap +=  (RelationshipStored(new Displacement(new Measurement(15,1),new Measurement(2,1))) -> 1)
        uncertaintyTreeMap +=  (RelationshipStored(new Displacement(new Measurement(6,1),new Measurement(2,1))) -> 1)
        println("uncertaintyTreeMap: " + uncertaintyTreeMap)
        var lowerBound: Option[RelationshipStored] = Some(RelationshipStored(new Displacement(new Measurement(9,0),new Measurement(0,0)) ) )
        var upperBound: Option[RelationshipStored] = Some(RelationshipStored(new Displacement(new Measurement(11,0),new Measurement(0,0)) ))
        println("uncertaintyTreeMap between 9 and 11 " 
                + uncertaintyTreeMap.rangeImpl(lowerBound,upperBound)
        )
        
        println("Creating UncertaintyMap")
	  	var uncertaintyTree = new UncertaintyMap[Int]
        println(uncertaintyTree)
        println("Adding 8+-1,0+-0 -> 1")
        uncertaintyTree +=  (RelationshipStored(new Displacement(new Measurement(8,1),new Measurement(0,0))) -> 1)
        println(uncertaintyTree)
        println("Adding 10+-1,0+-0 -> 2")
        uncertaintyTree +=  (RelationshipStored(new Displacement(new Measurement(10,1),new Measurement(0,0))) -> 2)
        println(uncertaintyTree)
        println("Adding 15+-1,0+-0 -> 3")
        uncertaintyTree +=  (RelationshipStored(new Displacement(new Measurement(15,1),new Measurement(0,0))) -> 3)
        println(uncertaintyTree)
        println("Adding 7+-1,0+-0 -> 4")
        uncertaintyTree +=  (RelationshipStored(new Displacement(new Measurement(7,1),new Measurement(0,0))) -> 4)
        println(uncertaintyTree)
        println("Adding 11+-1,0+-0 -> 5")
        uncertaintyTree +=  (RelationshipStored(new Displacement(new Measurement(11,1),new Measurement(0,0))) -> 5)
        println(uncertaintyTree)
        println("Adding 6+-1.2,0+-0 -> 6")
        uncertaintyTree +=  (RelationshipStored(new Displacement(new Measurement(6,1.2),new Measurement(0,0))) -> 6)
        println(uncertaintyTree)
        println("Adding 10+-0,0+-0 -> 7")
        uncertaintyTree +=  (RelationshipStored(new Displacement(new Measurement(12,1),new Measurement(0,0))) -> 7)
        println(uncertaintyTree + "\n")
        println("uncertaintyTree for RelationshipStored(Displacement(9+-1,0+-3)) " )
        println(uncertaintyTree.getAllEquals(RelationshipStored(new Displacement(new Measurement(9,1),new Measurement(0,0)))))
        
        var collectiveMap = new CollectiveMap
        collectiveMap.start
        actor {
          println("Picking Name (1->1)")
          collectiveMap ! PickName(1,1)
          println("Picking Name (1->1)")
          collectiveMap ! PickName(1,1)
          println("Picking Name (2->1)")
          collectiveMap ! PickName(2,1)
          println("Picking Name (3->1)")
          collectiveMap ! PickName(3,1)
          println("Picking Name (4->1)")
          collectiveMap ! PickName(4,1)
          collectiveMap ! MapSize
          println("Add(IdentifiedObject(1,2,Displacement(2,3)))")
          collectiveMap ! Add(IdentifiedObject(1,2,Displacement(2,3)))
          collectiveMap ! MapSize
          println("Add(IdentifiedObject(1,3,Displacement(2,1)))")
          collectiveMap ! Add(IdentifiedObject(1,3,Displacement(2,1)))
          collectiveMap ! MapSize
          println("Add(IdentifiedObject(1,4,Displacement(1,-1)))")
          collectiveMap ! Add(IdentifiedObject(1,4,Displacement(1,-1)))
          collectiveMap ! MapSize
          println("Add(IdentifiedObject(3,4,Displacement(-1,-2)))")
          collectiveMap ! Add(IdentifiedObject(3,4,Displacement(-1,-2)))
          collectiveMap ! MapSize
          Thread.sleep(2000)//msec
          println(collectiveMap)
          collectiveMap ! "Exit"
          /*
			  case GetIdentifierType(identifier: Int) => {
				  getIdentifierType(identifier) match {
				    case Some(objectType) => reply(IdentifierType(identifier,objectType))
				    case None => reply(noType(identifier))
				  }
			  }
			  case Contains(identifiers: Identifiers) => {}
			  case Matches(entries) => {}
              case Add(identifiedObject) => {
                    if(add(identifiedObject))
                        print("Failed to add object")
              }
           */
        }
        
   }
}
