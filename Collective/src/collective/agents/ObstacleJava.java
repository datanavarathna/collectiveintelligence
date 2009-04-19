package collective.agents;

public class ObstacleJava {
	int x;
	int y;
	int Otype;
	
	
	
	public ObstacleJava(int otype, int x, int y) {
		super();
		Otype = otype;
		this.x = x;
		this.y = y;
	}
	
	public ObstacleJava(){
		this.x = 0;
		this.y = 0;
		this.Otype = 0;
	}
	
}
