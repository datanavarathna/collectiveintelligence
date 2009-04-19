

public class AgentSpecs {
	
	int sensorRange;
	int sensorDeltaRange;
	int deltaAngle;
	int deltaSensorRange;
	
	int x;
	int y;
	
	public AgentSpecs(){
		this.x = 0;
		this.y = 0;
		this.sensorRange      = 0;
		this.sensorDeltaRange = 0;
		this.deltaAngle       = 0;
	}
	
	public int getSensorRange() {
		return sensorRange;
	}
	public void setSensorRange(int sensorRange) {
		this.sensorRange = sensorRange;
	}
	public int getSensorDeltaRange() {
		return sensorDeltaRange;
	}
	public void setSensorDeltaRange(int sensorDeltaRange) {
		this.sensorDeltaRange = sensorDeltaRange;
	}
	public int getDeltaAngle() {
		return deltaAngle;
	}
	public void setDeltaAngle(int deltaAngle) {
		this.deltaAngle = deltaAngle;
	}
	public int getX() {
		return x;
	}
	public void setX(int x) {
		this.x = x;
	}
	public int getY() {
		return y;
	}
	public void setY(int y) {
		this.y = y;
	}
	
}
