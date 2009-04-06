import java.awt.GridLayout;
import java.util.ArrayList;
import javax.swing.JFrame;

public class gui {

	int x, y;
	JFrame frame;
	ArrayList <GUIObject>objects = new ArrayList<GUIObject>();

	public gui( int x, int y ) {
		this.x = x;
		this.y = y;

		this.frame = new JFrame("Collective Status Pane");
		this.frame.setLayout( new GridLayout ( this.x, this.y ) ) ;
		
		mousey  ml = new mousey();
		keyz0r  kz = new keyz0r();
		
		this.frame.addMouseListener(ml);
		this.frame.addKeyListener(kz);
		
		this.fill();
		
		this.frame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
		this.frame.setSize(600, 600);
		frame.setVisible(true);
	}

	private void fill() {
		int count = this.x * this.y;

		for ( int i = 0; i < count; ++i ) {
			GUIObject o = new GUIObject(true);
			this.objects.add(o);

			o.setStatus(false);
			frame.getContentPane().add(o);
		}
	}

	public GUIObject getByXY( int x, int y ) {
		GUIObject o = null;
		int index = ( this.y * y ) + x;
		System.out.println(index + " out of " + this.objects.size() );
		if ( index <= this.objects.size() ) {
			o = this.objects.get(index);
		}
		return o;
	}

	public void updateCellAgentStatus( int x, int y, boolean agentInSquare ) {
		GUIObject o = this.getByXY( x, y );
		if ( o != null ) {
			o.setStatus(agentInSquare);
			this.refresh();
		} else {
			System.out.println("Invalid Index!");
		}
	}
	public void refresh() {
		this.frame.repaint();
	}

	public static void main ( String[] args ) {

		gui g = new gui( 25, 25 );
		
		g.updateCellAgentStatus(0, 0, true);
		g.updateCellAgentStatus(1, 1, true);
		g.updateCellAgentStatus(2, 2, true);
		g.updateCellAgentStatus(3, 3, true);
		g.updateCellAgentStatus(4, 4, true);

		boolean foo = false;

		while ( true ) {
			System.out.println( "Online: " + g.getByXY(0, 0).getStatus() );
			g.updateCellAgentStatus(0, 0, foo);
			foo = !foo;

			try {
				Thread.sleep(500);
			} catch (InterruptedException e) {
				System.out.println(e);
			}
		}
	}
}
