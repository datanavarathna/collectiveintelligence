package timestampConcurrency

class Transaction(val name: String,maxRetries: Int) {
	//all writes in 'operations' should happen at the end in an atomic operation
	private[this] var timeStamp: Long = _
	def timestamp = timeStamp
	
	private[this] var retries: Int = 0
	private[this] var successful = false
	
	override def toString = "Transaction( "+name+")"
	
	def setOperations(operations: => Boolean){
		do{
		timeStamp = System.nanoTime()
		retries += 1
		if(retries>1)
			println("Retrying transaction "+name)
		successful = operations
		println("Finished executing operation in "+name+" with result: "+successful)
		
	}while(!successful && retries < maxRetries)
	if(successful)
		println("Successfully completed transaction "+name)
	else
		println("Unable to complete transaction"+name)
	}	
}