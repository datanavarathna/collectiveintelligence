public class IdentifiedObjectJava {
	public int id1;
	public int id2;
	public int obstacle1Type;
	public int obstacle2Type;
	public MeasurementJava deltaX;
	public MeasurementJava deltaY;

	public IdentifiedObjectJava(){
		super();
	}

	public int compareTo(boolean 1Or2, IdentifiedObjectJava ioj){
		int output = 0;
		if(1Or2){
			if(id1-ioj.id1>0)
				output+=2;
			else if(id1-ioj.id1<0)
				output-=2;
			if(id2-ioj.id2>0)
				output+=1;
			else if(id2-ioj.id2<0)
				output-=1;
		} else {
			if(obstacle1Type-ioj.obstacle1Type>0)
				output += 8;
			else if(obstacle1Type-ioj.obstacle1Type<0)
				output -= 8;
			if(obstacle2Type-ioj.obstacle2Type>0)
				output += 4;
			else if(obstacle2Type-ioj.obstacle2Type<0)
				output -= 4;
			if(deltaX.canEqual)
				output += 0;
			else
				output += 2;
			if(deltaY.canEqual)
				output += 0;
			else
				output += 1;
		}
		return output;
	}
}