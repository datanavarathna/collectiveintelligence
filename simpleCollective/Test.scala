

object Test
{
    def main(args : Array[String]) = {
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
    }
}
