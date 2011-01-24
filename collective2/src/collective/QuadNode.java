package collective;
public class QuadNode {
	Object obj;
	QuadNode next;
	public QuadNode(Object o){
		obj = o;
	}
	
	public String toString(){
		return obj.toString();
	}
}