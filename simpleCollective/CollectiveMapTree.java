    // BinarySearchTree class
    //
    // CONSTRUCTION: with no initializer
    //
    // ******************PUBLIC OPERATIONS*********************
    // void insert( x )       --> Insert x
    // void remove( x )       --> Remove x (unimplemented)
    // IdentifiedObjectJava find( x )   --> Return item that matches x
    // IdentifiedObjectJava findMin( )  --> Return smallest item
    // IdentifiedObjectJava findMax( )  --> Return largest item
    // boolean isEmpty( )     --> Return true if empty; else false
    // void makeEmpty( )      --> Remove all items
    // void printTree( )      --> Print tree in sorted order

    /**
     * Implements an AVL tree.
     * Note that all "matching" is based on the compareTo method.
     * @author Mark Allen Weiss
     */
    public class CollectiveMapTree
    {
        /**
         * Construct the tree.
         */
        public CollectiveMapTree(boolean fOs )
        {
            firstOrSecond = fOs;
            root = null;
        }

        /**
         * Insert into the tree; duplicates are ignored.
         * @param x the item to insert.
         */
        public void add( IdentifiedObjectJava x )
        {
            root = add( x, root );
        }

        /**
         * Remove from the tree. Nothing is done if x is not found.
         * @param x the item to remove.
         */
        public void remove( IdentifiedObjectJava x )
        {
            System.out.println( "Sorry, remove unimplemented" );
        }

        /**
         * Find an item in the tree.
         * @param x the item to search for.
         * @return the matching item or null if not found.
         */
        public IdentifiedObjectJava find( IdentifiedObjectJava x )
        {
            return elementAt( find( x, root ) );
        }

        public ArrayList<IdentifiedObjectJava> multipleFind(IdentifiedObjectJava x)
        {
			ArrayList<IdentifiedObjectJava> al = new ArrayList<IdentifiedObjectJava>();
			al = multipleFind(x, root, al);
			return al;
		}

        private ArrayList<IdentifiedObjectJava> multipleFind( IdentifiedObjectJava x, CollectiveMapTreeNode t, ArrayList<IdentifiedObjectJava> al )
		        {
		            while( t != null )
		                if( x.compareTo(firstOrSecond,  t.element ) < 0 )
		                    t = t.left;
		                else if( x.compareTo(firstOrSecond,  t.element ) > 0 )
		                    t = t.right;
		                else {
		                    al.add(t.element);
		                    al = multipleFind(x,t.left,al);
		                    al = multipleFind(x,t,left.al);
		                    return al;
						}

		            return al;   // No match
        }

        /**
         * Make the tree logically empty.
         */
        public void makeEmpty( )
        {
            root = null;
        }

        /**
         * Test if the tree is logically empty.
         * @return true if empty, false otherwise.
         */
        public boolean isEmpty( )
        {
            return root == null;
        }

        /**
         * Print the tree contents in sorted order.
         */
        public void printTree( )
        {
            if( isEmpty( ) )
                System.out.println( "Empty tree" );
            else
                printTree( root );
        }

        /**
         * Internal method to get element field.
         * @param t the node.
         * @return the element field or null if t is null.
         */
        private IdentifiedObjectJava elementAt( CollectiveMapTreeNode t )
        {
            return t == null ? null : t.element;
        }

        /**
         * Internal method to insert into a subtree.
         * @param x the item to insert.
         * @param t the node that roots the tree.
         * @return the new root.
         */
        private CollectiveMapTreeNode insert( IdentifiedObjectJava x, CollectiveMapTreeNode t )
        {
            if( t == null )
                t = new CollectiveMapTreeNode( x, null, null );
            else if( x.compareTo(firstOrSecond,  t.element ) < 0 )
            {
                t.left = insert( x, t.left );
                if( height( t.left ) - height( t.right ) == 2 )
                    if( x.compareTo(firstOrSecond,  t.left.element ) < 0 )
                        t = rotateWithLeftChild( t );
                    else
                        t = doubleWithLeftChild( t );
            }
            else if( x.compareTo(firstOrSecond,  t.element ) > 0 )
            {
                t.right = insert( x, t.right );
                if( height( t.right ) - height( t.left ) == 2 )
                    if( x.compareTo(firstOrSecond,  t.right.element ) > 0 )
                        t = rotateWithRightChild( t );
                    else
                        t = doubleWithRightChild( t );
            }
            else
                ;  // Duplicate; do nothing
            t.height = max( height( t.left ), height( t.right ) ) + 1;
            return t;
        }

        /**
         * Internal method to find the smallest item in a subtree.
         * @param t the node that roots the tree.
         * @return node containing the smallest item.
         */
        private CollectiveMapTreeNode findMin( CollectiveMapTreeNode t )
        {
            if( t == null )
                return t;

            while( t.left != null )
                t = t.left;
            return t;
        }

        /**
         * Internal method to find the largest item in a subtree.
         * @param t the node that roots the tree.
         * @return node containing the largest item.
         */
        private CollectiveMapTreeNode findMax( CollectiveMapTreeNode t )
        {
            if( t == null )
                return t;

            while( t.right != null )
                t = t.right;
            return t;
        }

        /**
         * Internal method to find an item in a subtree.
         * @param x is item to search for.
         * @param t the node that roots the tree.
         * @return node containing the matched item.
         */
        private CollectiveMapTreeNode find( IdentifiedObjectJava x, CollectiveMapTreeNode t )
        {
            while( t != null )
                if( x.compareTo(firstOrSecond,  t.element ) < 0 )
                    t = t.left;
                else if( x.compareTo(firstOrSecond,  t.element ) > 0 )
                    t = t.right;
                else
                    return t;    // Match

            return null;   // No match
        }

        /**
         * Internal method to print a subtree in sorted order.
         * @param t the node that roots the tree.
         */
        private void printTree( CollectiveMapTreeNode t )
        {
            if( t != null )
            {
                printTree( t.left );
                System.out.println( t.element );
                printTree( t.right );
            }
        }

        /**
         * Return the height of node t, or -1, if null.
         */
        private static int height( CollectiveMapTreeNode t )
        {
            return t == null ? -1 : t.height;
        }

        /**
         * Return maximum of lhs and rhs.
         */
        private static int max( int lhs, int rhs )
        {
            return lhs > rhs ? lhs : rhs;
        }

        /**
         * Rotate binary tree node with left child.
         * For AVL trees, this is a single rotation for case 1.
         * Update heights, then return new root.
         */
        private static CollectiveMapTreeNode rotateWithLeftChild( CollectiveMapTreeNode k2 )
        {
            CollectiveMapTreeNode k1 = k2.left;
            k2.left = k1.right;
            k1.right = k2;
            k2.height = max( height( k2.left ), height( k2.right ) ) + 1;
            k1.height = max( height( k1.left ), k2.height ) + 1;
            return k1;
        }

        /**
         * Rotate binary tree node with right child.
         * For AVL trees, this is a single rotation for case 4.
         * Update heights, then return new root.
         */
        private static CollectiveMapTreeNode rotateWithRightChild( CollectiveMapTreeNode k1 )
        {
            CollectiveMapTreeNode k2 = k1.right;
            k1.right = k2.left;
            k2.left = k1;
            k1.height = max( height( k1.left ), height( k1.right ) ) + 1;
            k2.height = max( height( k2.right ), k1.height ) + 1;
            return k2;
        }

        /**
         * Double rotate binary tree node: first left child
         * with its right child; then node k3 with new left child.
         * For AVL trees, this is a double rotation for case 2.
         * Update heights, then return new root.
         */
        private static CollectiveMapTreeNode doubleWithLeftChild( CollectiveMapTreeNode k3 )
        {
            k3.left = rotateWithRightChild( k3.left );
            return rotateWithLeftChild( k3 );
        }

        /**
         * Double rotate binary tree node: first right child
         * with its left child; then node k1 with new right child.
         * For AVL trees, this is a double rotation for case 3.
         * Update heights, then return new root.
         */
        private static CollectiveMapTreeNode doubleWithRightChild( CollectiveMapTreeNode k1 )
        {
            k1.right = rotateWithLeftChild( k1.right );
            return rotateWithRightChild( k1 );
        }

          /** The tree root. */
        private CollectiveMapTreeNode root;

        /** Which contains method is being used */
        boolean firstOrSecond;


            // Test program
        public static void main( String [ ] args )
        {
            AvlTree t = new AvlTree( );
            final int NUMS = 4000;
            final int GAP  =   37;

            System.out.println( "Checking... (no more output means success)" );

            for( int i = GAP; i != 0; i = ( i + GAP ) % NUMS )
                t.insert( new MyInteger( i ) );

            if( NUMS < 40 )
                t.printTree( );
            if( ((MyInteger)(t.findMin( ))).intValue( ) != 1 ||
                ((MyInteger)(t.findMax( ))).intValue( ) != NUMS - 1 )
                System.out.println( "FindMin or FindMax error!" );

            for( int i = 1; i < NUMS; i++ )
                 if( ((MyInteger)(t.find( new MyInteger( i ) ))).intValue( ) != i )
                     System.out.println( "Find error1!" );
    }
}
