package timestampConcurrencyControl

class TransactionObject {
	var readTime: Long = _ //time last used by transaction
	var writeTime: Long = _ //time last updated by transaction
	//all public methods that read/write to object
	//must update the relevant var above
}

class Action(val transactionObject: TransactionObject,operation: => Unit){
	
}

class Transaction{
	var timestamp: Long = _
	var dependencies: List[Transaction] = Nil
	var actions: List[Action] = Nil //functions
	var oldTransactionObjects: List[TransactionObject] = Nil
	/*
	def commit: Boolean = {
		for(action <- actions)
		{
			val actionObject = action.transactionObject
			if(actionObject.writeTime > timestamp)
				abort
			else{
				dependencies = actionObject :: dependencies
				
			}
					
		}
		true
	}
	
	def abort*/
}