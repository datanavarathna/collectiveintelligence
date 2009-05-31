object UnitTests2 {

  def main(args : Array[String]) = 
   {
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
   }
}
