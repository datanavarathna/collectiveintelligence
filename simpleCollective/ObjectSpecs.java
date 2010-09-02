

public class ObjectSpecs {
	int x;
	int y;
    int type;

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

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}
	
	ObjectSpecs() {
		this.x    = 0;
		this.y    = 0;
		this.type = 1;
	}

    @Override
	public String toString() {
		return " ObjectSpec: type=" +type+ " x=" + x+ " y=" + y;
	}
}
