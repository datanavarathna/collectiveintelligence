

public class QuadTree{
  QuadNode root = null;
  int length = 0;
  public QuadTree(){
    
  }
  
  public QuadTree(Object o){
    root = new QuadNode(o);
    length = 1;
  }
  
  public QuadTree(Object[] os){
    for(int i = os.length-1; i >= 0; i++){
      QuadNode added = new QuadNode(os[i]);
      added.next = root;
    }
    length = os.length;
  }
  
  public void add(Object o){
	  addAtBeg(o);
  }
  
  public void addAtBeg(Object o){
    QuadNode added = new QuadNode(o);
    added.next = root;
    length++;
  }
  
  public Object find(Object o){
    QuadNode ln = root;
    if(root!=null){
        while(ln.next!=null){
            ln = ln.next;
            if ((ln.obj != null) && ln.obj.toString().equals(o.toString()))
                return o;
        }
        if((ln.obj != null) && ln.obj.toString().equals(o.toString()))
                return o;
   }
   return null;
  }
  
  public boolean contains(Object o)
  {
	  return find(o) != null;
  }
  
  public Object[] returnAll(){
	  Object[] op = new Object[length];
	  QuadNode ln = root;
	  for(int i = 0; i < length; i++){
		  op[i]=ln;
		  ln = ln.next;
	  }
	  return op;
  }
  
  public Boolean remove(Object o){
	  QuadNode ln = root;
	  if(ln.obj.toString().equals(o.toString())){
		  root = ln.next;
		  ln.next = null;
		  return true;
	  }
	  while(ln.next!=null){
	    if(ln.next.obj.toString().equals(o.toString())){
	    	ln.next = ln.next.next;
	    	return true;
	    }
	    ln = ln.next;	
	  }
	  return false;
  }
}
