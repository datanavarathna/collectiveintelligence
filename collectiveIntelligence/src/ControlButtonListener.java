

import java.awt.event.ActionEvent;
import java.util.ArrayList;


public class ControlButtonListener implements java.awt.event.ActionListener {

	
	GUI g;
	
	public ControlButtonListener( GUI g ) {
		this.g = g;
	}
	
	@Override
	public void actionPerformed(ActionEvent e) {
		String a = e.getActionCommand();
		
		if ( a.equals("kill" ) ) {
			this.kill();
		} else if ( a.equals("initialize" ) ) {
			this.init();
		} else if ( a.equals("start" ) ) {
			this.start();
		} else {
			// System.err.println("Unknown Signal from View. Ignoring Output");
		}
	}
	
	private void kill() {
		// System.out.println("God said I need to kill.");
		// System.out.println("Shutting down Process. Aborting Children With Gusto");
		// de comment the above for a more tasteful Application. Add Curse words to taste.
		System.out.println("Caught a signal to Kill. Calling GUI Close method.");
		this.g.close();
	}
	
	private void start() {
		if ( this.g.loaded ) {
			this.g.primed = true;
			// System.out.println("Tis all good in the hood. Primed and Ready for the Controller");
			// de comment the above for a more tasteful Application. Add Curse words to taste.
			
			System.out.println("Changing status to Primed. Ready for Main Logic takeover.");
			System.out.println("");
		} else {
			System.err.println("Error: You need to initialize the lists First.");
			// System.err.println("We could call init for you, but you know what. We won't. Burn in Hell.");
			// System.err.println("Poopie");
			// de comment the above for a more tasteful Application. Add Curse words to taste.
		}
		System.out.println("");
	}
	
	private void init() {
		// System.out.println("Creating Passable ArrayList of Agents!");
		ArrayList<AgentSpecs>  as = new ArrayList<AgentSpecs>();
		ArrayList<ObjectSpecs> os = new ArrayList<ObjectSpecs>();
		
		ArrayList<GUIObject>  go = this.g.objects;
		
		for ( int i = 0; i < go.size(); ++i ) {
			if ( go.get(i).getStatus() == 1 ) {
				as.add(go.get(i).a);
				//	System.out.println("Agent at " + go.get(i).a.getX() + " " + go.get(i).a.getY() );
				//	System.out.println("Sensor Range: " + go.get(i).a.getSensorRange() );
			}
			if ( go.get(i).getStatus() == 2 ) {
				os.add(go.get(i).o);
				//	System.out.println("Object at " + go.get(i).o.getX() + " " + go.get(i).o.getY() );
				//	System.out.println("Object Status: " + go.get(i).o.getType() );
			}
		}
		
		this.g.as = as;
		this.g.os = os;
		
		this.g.loaded = true;
		
		System.out.println("Created " + as.size() + " Agents"  );
		System.out.println("Created " + os.size() + " Objects" );
		System.out.println("Waiting to Start!");
		// System.out.println("Objects are Written to GUI. Ready for teh Bay Boy to werk.");
		System.out.println("");
	}
}
