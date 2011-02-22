package timestampConcurrency

trait TimeStampConcurrency {
	private var writeTimeStamp: Long = 0
	private var readTimeStamp: Long = 0
	
	//System.nanoTime() //.currentTimeMillis()
	
	def read(t: Transaction): Boolean = {
		val timestamp = t.timestamp
		if(timestamp >= writeTimeStamp){
			println("Successful read of "+super.toString+" by "+t)
			readTimeStamp = math.max(timestamp, readTimeStamp)
			true
		}else{
			println("Attempted read of overwritten value in "+super.toString+" by "+t)
			false
		}
	}
	
	def write(t: Transaction): Boolean = {
		val timestamp = t.timestamp
		if(timestamp<readTimeStamp){
			println("Attempted write of a previously needed value in "+super.toString+" by "+t)
			false
		}
		if(timestamp < writeTimeStamp){
			println("Attempted write of an obsolete value by "+t+" to "+super.toString)
			false
		}else{
			println("Successful write to "+super.toString+" by "+t)
			true
		}
	}
}