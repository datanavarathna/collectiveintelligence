package timestampConcurrency

class Transaction(name: String,maxRetries: Int,operations: => Boolean) {
	//all writes in 'operations' should happen at the end in an atomic operation
	private[this] var timeStamp = System.nanoTime()
	
	def timestamp = timeStamp
	
	private[this] var retries: Int = 0
	private[this] var successful = false
	do{
		timeStamp = System.nanoTime()
		retries += 1
		if(retries>1)
			println("Retrying transaction "+name)
		successful = operations
		
	}while(!successful && retries < maxRetries)
	if(successful)
		println("Successfully completed transaction "+name)
	else
		println("Unable to complete transaction"+name)
		
}