package collective.gui;

import java.awt.GridLayout;
import java.util.ArrayList;
import javax.swing.JButton;
import javax.swing.JFrame;

public class GUI {

	int x, y;
	JFrame frame;
	JFrame ctrl;
	
	ArrayList <GUIObject> objects;
	ArrayList<ObjectSpecs> os;
	ArrayList <AgentSpecs> as;
	
	ControlButtonListener cl;
	ButtonListener bl;
	
	boolean primed;
	
	public GUI() {
		this.setup( 5, 5, 500, 500 );
	}	
	public GUI( int x, int y ) {
		this.setup( x, y, 500, 500 );
	}
	public GUI( int x, int y, int windowX, int windowY ) {
		this.setup( x, y, windowX, windowY );
	}
	
	
	public ArrayList<ObjectSpecs> getOs() {
		return os;
	}
	public void setOs(ArrayList<ObjectSpecs> os) {
		this.os = os;
	}
	public ArrayList<AgentSpecs> getAs() {
		return as;
	}
	public void setAs(ArrayList<AgentSpecs> as) {
		this.as = as;
	}

	public boolean isReadyForRuntime() {
		return this.primed;
	}
	
	private void setup( int x, int y, int windowX, int windowY ) {
		
		this.objects = new ArrayList<GUIObject>();
		this.as      = null;
		
		this.primed  = false;
		
		this.x = x;
		this.y = y;

		this.frame = new JFrame("Collective Status Pane");
		this.frame.setLayout( new GridLayout ( this.x, this.y ) ) ;

		this.ctrl = new JFrame("Controller");
		this.ctrl.setLayout( new GridLayout ( 1, 5 ) ) ;
		
		this.bl = new ButtonListener();
		this.cl = new ControlButtonListener( this );
		
		this.fill();
		
		this.frame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
		this.ctrl.setDefaultCloseOperation(  JFrame.EXIT_ON_CLOSE );
		
		this.frame.setSize(windowX, windowY);
		this.ctrl.setSize(300, 75);
		
		frame.setVisible(true);
		ctrl.setVisible(true);
		
	}
	
	private void fill() {
		
		int count = this.x * this.y;

		for ( int i = 0; i < count; ++i ) {
			GUIObject o = new GUIObject( 0 );
			
			o.addActionListener( this.bl );
			
//			int index = ( this.y * y ) + x;
			
			o.a.setX(0);
			o.a.setY(0);
			
			this.objects.add(o);
			o.setStatus( 0 );
			
			frame.getContentPane().add(o);
		}
		
		JButton jb = new JButton();
		jb.setText("initialize");
		jb.addActionListener(this.cl);
		this.ctrl.getContentPane().add(jb);
		
		
		jb = new JButton();
		jb.setText("start");
		jb.addActionListener(this.cl);
		this.ctrl.getContentPane().add(jb);
		
		jb = new JButton();
		jb.setText("kill");
		jb.addActionListener(this.cl);
		this.ctrl.getContentPane().add(jb);
	}

	public synchronized GUIObject getByXY( int x, int y ) {
		GUIObject o = null;
		int index = ( this.y * y ) + x;
//		System.out.println(index + " out of " + this.objects.size() );
		if ( index <= this.objects.size() ) {
			o = this.objects.get(index);
		}
		return o;
	}
	
	public void setGUISize( int x, int y ) {
		this.frame.setSize(x, y);
	}
	
	public synchronized void updateCellAgentStatus( int x, int y, boolean agentInSquare ) {
		GUIObject o = this.getByXY( x, y );
		if ( o != null ) {
			if ( agentInSquare ) {
				o.setStatus(1);
			} else {
				o.setStatus(0);
			}
			this.refresh();
		} else {
			System.err.println("Invalid Index!");
		}
	}
	
	public void refresh() {
		this.frame.repaint();
	}

	public static void main ( String[] args ) {

		GUI g = new GUI( 5, 5, 100, 100 );
		
		g.setGUISize(600, 600);
		
		g.updateCellAgentStatus(0, 0, true);
		g.updateCellAgentStatus(1, 1, true);
		g.updateCellAgentStatus(2, 2, true);
		g.updateCellAgentStatus(3, 3, true);
		g.updateCellAgentStatus(4, 4, true);

		boolean foo = false;

		while ( true ) {
//			System.out.println( "Online: " + g.getByXY(0, 0).getStatus() );
//			g.updateCellAgentStatus(0, 0, foo);
			foo = !foo;

			try {
				Thread.sleep(500);
			} catch (InterruptedException e) {
				System.out.println(e);
			}
		}
	}
}
