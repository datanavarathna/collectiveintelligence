public class IdentifiedObjectJava {
	int id1;
	int id2;
	int obstacle1Type;
	int obstacle2Type;
	MeasurementJava deltaX;
	MeasurementJava deltaY;

	public int compareTo(boolean 1Or2, IdentifiedObjectJava ioj){
		if(1Or2){
			int output = 0;
			if(id1-ioj.id1>0)
				output+=2;
			else if(id1-ioj.id1<0)
				output-=2;
			if(id2-ioj.id2>0)
				output+=1;
			else if(id2-ioj.id2<0)
				output-=1;
		} else {
			int output = 0;
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
	}
}