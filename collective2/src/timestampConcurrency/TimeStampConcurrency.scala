package timestampConcurrency

case class OperationResult(success: Boolean,result: Object)

trait TimeStampConcurrency {
	private[this] var writeTimeStamp: Long = 0
	private[this] var readTimeStamp: Long = 0
	
	//System.nanoTime() //.currentTimeMillis()
	
	def lastRead = readTimeStamp
	def lastWrite = writeTimeStamp
	
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
			return false
		}
		if(timestamp < writeTimeStamp){
			println("Attempted write of an obsolete value by "+t+" to "+super.toString)
			return false
		}else{
			println("Successful write to "+super.toString+" by "+t)
			return true
		}
	}
}