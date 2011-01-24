package datalogging

import xml._

class XMLlogger (val file: String){
	//private var saved: Boolean = false
	private val recordBuffer = new NodeBuffer
	
	def saveLog = {
		if(!recordBuffer.isEmpty)
		{
			val rootNode: Elem = XML.loadString("<log>"+recordBuffer.mkString("<record>","</record><record>","</record>")+"</log>")
			try {
				XML.save(file,rootNode, "UTF-8",true,null)
			}catch{
				case _ => println("Failed to save to file "+file)
			}
		}
		
	}
	
	def saveRecord(node: Elem) = {
		recordBuffer += node
	}
}

class XMLlogReader (val file: String){
	private var loaded: Boolean = false
	
	private val queue = new scala.collection.mutable.Queue[Node]
	
	def readLog: Unit = {
		try {
			XML.loadFile(file) match {
				case <log>{records @ _*}</log> => {
					for(<record>{record @ _}</record> <- records){
						queue += record
					}
				}
				case element @ _ => println("Incorrect element in file: " + element)
			}//end match
		}catch{
			case ex: java.io.FileNotFoundException => println("File "+file+" not found")
		}
	}//end def
	private def checkIfLoaded:Unit = {
		if(!loaded)
		{
			readLog
			loaded=true
		}
	}
	def hasNext = {
		checkIfLoaded
		!queue.isEmpty
	}
	
	def getRecord = {
		checkIfLoaded
		if(!queue.isEmpty)
			queue.dequeue
		else
			println("End of Log")
	}
	
}

object XMLlogger {
	def main(args : Array[String]) = 
	{
		val xml1 = <a>first</a>
		val xml2 = <a>second</a>
		val xml3 = <b>third</b>

		val file = """E:\Programming\xmlTest.txt"""
		val logger = new XMLlogger(file)
		logger.saveRecord(xml1)
		logger.saveRecord(xml2)
		logger.saveRecord(xml3)
		logger.saveLog
		
		val logReader = new XMLlogReader(file)
		while(logReader.hasNext)
			println(logReader.getRecord)
		
		/*
		import scala.xml.pull._
		import scala.io.Source
		val src = Source.fromString("<root>"+xml1+xml2+xml3+"</root>")
		val er = new XMLEventReader(src)
		while (er.hasNext)
			Console.println(er.next)
		*/
	}
}