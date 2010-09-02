    // Basic node stored in AVL trees
    // Note that this class is not accessible outside
    // of package DataStructures

    public class CollectiveMapTreeNode
    {
            // Constructors
        CollectiveMapTreeNode( IdentifiedObjectJava theElement )
        {
            this( theElement, null, null );
        }

        CollectiveMapTreeNode( IdentifiedObjectJava theElement, CollectiveMapTreeNode lt, CollectiveMapTreeNode rt )
        {
            element  = theElement;
            left     = lt;
            right    = rt;
            height   = 0;
        }

            // Friendly data; accessible by other package routines
        IdentifiedObjectJava element;      // The data in the node
        CollectiveMapTreeNode    left;         // Left child
        CollectiveMapTreeNode    right;        // Right child
        int        height;       // Height
    }