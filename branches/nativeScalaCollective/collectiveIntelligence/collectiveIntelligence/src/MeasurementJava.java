public class MeasurementJava {
	int meas;
	int uncertainty;

	public boolean canEqual(MeasurementJava mj){
		if(upperBound() <= mj.upperBound() && upperBound() >= mj.lowerBound())
			return true;
		if(lowerBound() <= mj.lowerBound() && lowerBound() >= mj.upperBound())
			return true;
		if(upperBound() >= mj.upperBound() && lowerBound() <= mj.lowerBound())
			return true;
		return false;
	}

	public int upperBound(){
		return meas + uncertainty;
	}
	public int lowerBound(){
		return meas - uncertainty;
	}
}