package collective.agents;

public class QuadTree{
  QuadNode head = null;
  int length = 0;
  public QuadTree(){
    
  }
  
  public QuadTree(Object o){
    head = new QuadNode(o);
    length = 1;
  }
  
  public QuadTree(Object[] os){
    for(int i = os.length-1; i >= 0; i++){
      QuadNode added = new QuadNode(os[i]);
      added.next = head;
    }
    length = os.length;
  }
  
  public void add(Object o){
	  addAtBeg(o);
  }
  
  public void addAtBeg(Object o){
    QuadNode added = new QuadNode(o);
    added.next = head;
    length++;
  }
  
  public Object find(Object o){
    QuadNode ln = head;
    while(ln.next!=null){
    	ln = ln.next;
    	if (ln.obj.toString().equals(o.toString()))
        	return o;
    }
    if(ln.obj.toString().equals(o.toString()))
    	return o;
    return null;
  }
  
  public Object[] returnAll(){
	  Object[] op = new Object[length];
	  QuadNode ln = head;
	  for(int i = 0; i < length; i++){
		  op[i]=ln;
		  ln = ln.next;
	  }
	  return op;
  }
  
  public int remove(Object o){
	  QuadNode ln = head;
	  if(ln.obj.toString().equals(o.toString())){
		  head = ln.next;
		  ln.next = null;
		  return 1;
	  }
	  while(ln.next!=null){
	    if(ln.next.obj.toString().equals(o.toString())){
	    	ln.next = ln.next.next;
	    	return 1;
	    }
	    ln = ln.next;	
	  }
	  return 0;
  }
}
