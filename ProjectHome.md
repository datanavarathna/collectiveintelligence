A swarm will be programmed in Scala using the actor library.

The swarm simulation will consist of agents that can create their own internal map of their environment. The swarm will explore their environment and generate a topological map inside of a distributed file system. The topological map does not store locations, rather it stores the distances between objects.

Each agent will keep an up-to-date record of its own location in relation to objects on the map. When a sufficient map is created, two or more agents will rendezvous using the following technique.

An agent (agent 1) will ask an agent that is out of sight (agent 2) for its location. Agent 2 will transmit its location to agent 1, which will compute where agent 2 is in relation to agent 1 using the generated map. Agent 1 will then move to Agent 2. All calculations will use propagation of uncertainty to prevent uncertainty from causing false negative number matches.