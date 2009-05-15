

object Test
{
    def main(args : Array[String]) = {
		/*
        val environment = new Environment(10,10)
        var obstacleList = List(Obstacle(1,1,1),Obstacle(1,2,2))

        val topologicalElementGenerator = new TopologicalElementGenerator
		val relationshipIdentfier = new RelationshipIdentfier
		val map = new CollectiveMap
        val agent1 = new Agent(environment, topologicalElementGenerator, relationshipIdentfier, map, 2, 1, 1)
        val agent2 = new Agent(environment, topologicalElementGenerator, relationshipIdentfier, map, 2, 1, 1)

        var agentList = AgentWithLocation(agent1, 3, 3)::AgentWithLocation(agent1, 0, 0)::Nil

        environment.start
        environment ! agentList
        environment ! obstacleList
        environment ! "Exit"
        */

        //Test Measurement equality
        var temp1 = new Measurement(-1)
        var temp2 = new Measurement(-1)
        if(temp1 != temp2)
            println(temp1 +" != " + temp2 +"failed")
        if(temp1 == temp2)
            println(temp1 +" == " + temp2 +"correct")

      //Test atan
      var x = 0
      var y = 0
      println("("+x+","+y+")")
      println(Math.atan2(y,x))
      x = 1
      y = 0
      println("("+x+","+y+")")
      println(Math.atan2(y,x))
      x = 0
      y = 1
      println("("+x+","+y+")")
      println(Math.atan2(y,x))
      x = -1
      y = 0
      println("("+x+","+y+")")
      println(Math.atan2(y,x))
      x = 0
      y = -1
      println("("+x+","+y+")")
      println(Math.atan2(y,x))
      //Test Quad Tree

      val quadTree = new QuadTreeGateway
      if(quadTree.add(Obstacle(1, 0, 0)))
        println("Added obstacle at (0,0)")
      else
        println("Failed to register add")
      if(quadTree.contains(0,0))
        println("Correctly found (0,0)")
      else
        println("Failed")
      if(quadTree.add(Obstacle(1, 1, 1)))
        println("Added obstacle at (1,1)")
      else
        println("Failed to register add")
      if(quadTree.add(Obstacle(1, 8, 3)))
        println("Added obstacle at (8,3)")
      else
        println("Failed to register add")
      if(quadTree.add(Obstacle(1, 7, 2)))
        println("Added obstacle at (7,2)")
      else
        println("Failed to register add")
      
      if(quadTree.contains(1,1))
        println("Correctly found (1,1)")
      else
        println("Failed")
      if(quadTree.contains(8,3))
        println("Correctly found (8,3)")
      else
        println("Failed")
      if(quadTree.contains(7,2))
        println("Correctly found (7,2)")
      else
        println("Failed to find (7,2)")
      if(!quadTree.contains(2,2))
        println("Correctly failed to find an obstacle at (2,2)")
      else
        println("Failed")
      if(quadTree.range(2,0,0)==List(Obstacle(1,0,0),Obstacle(1,1,1)))
        println("Correctly found Obstacles with radius 2 of 0,0")
      else
        println("Failed")
      //println("Should only contain 0,0 and 1,1")
      //println(quadTree.range(2,0,0))
      if(quadTree.range(9,0,0)==List(Obstacle(1,0,0),Obstacle(1,1,1),Obstacle(1, 8, 3),Obstacle(1, 7, 2)))
        println("Correctly found Obstacles with radius 9 of 0,0")
      else
        println("Failed")
      if(quadTree.range(4,3,3)==List(Obstacle(1,0,0),Obstacle(1,1,1),Obstacle(1, 7, 2)))
        println("Correctly found Obstacles with radius 4 of 3,3")
      else
        println("Failed" + quadTree.range(4,3,3))
      if(quadTree.range(1,3,3)==List())
        println("Correctly found no Obstacles within radius 1 of 3,3")
      else
        println("Failed")
      //passed all tests

        
      //run program
       (new scalaGuiWrapper(50,50,1900,1000)).start
       println("Done")
       
    }
}
