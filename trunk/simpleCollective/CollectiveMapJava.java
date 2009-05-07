public class CollectiveMapJava {
	CollectiveMapTree ids = new CollectiveMapTree(true);
	CollectiveMapTree relationships = new CollectiveMapTree(false);
	public CollectiveMapJava(){
		super();
	}
	public void add(IdentifiedObjectJava ioj){
		ids.add(ioj);
		relationships.add(ioj);
	}
	public boolean contains(IdentifiedObjectJava ioj){
		return ids.find(ioj);
	}
	public ArrayList<IdentifiedObjectJava> relationshipSearch(IdentifiedObjectJava ioj){
		return relationships.multipleFind(ioj);
	}
	public IdentifiedObjectJava idSearch(IdentifiedObjectJava ioj){
		return ids.find(ioj);
	}
}