package org.komodo.shell;

import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceContextVisitor;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;

/**
 * Utility class for finding a node within a node structure with the given node path
 * @author blafond
 *
 */
public class FindWorkspaceNodeVisitor implements WorkspaceContextVisitor, StringConstants {

    private final String nodePath;
    
    private KomodoObject node;
    private WorkspaceContext context;

    /**
     * @param nodePath the target node path
     */
    public FindWorkspaceNodeVisitor(String nodePath) {
		super();
		this.nodePath = nodePath;
	}



	@Override
    public String visit(WorkspaceContext workspaceContext) throws Exception {
		String startpath = workspaceContext.getFullName();
		
		if( node != null )
		    return startpath;

		if( startpath.equals(nodePath) ) {
			node = workspaceContext.getKomodoObj();
			context = workspaceContext;
			return startpath;
		}

		if (! nodePath.startsWith(startpath))
            return null;

        for (WorkspaceContext child : workspaceContext.getChildren()) {
            String childPath = (String)child.visit(this);
            
            if( node != null ) return childPath;
            
            if( nodePath.equals(childPath)) {
            	node = child.getKomodoObj();
            	context = child;
            	return childPath;
            }
        }

        return null;
    }
	
	/**
	 * @return node the target node (if found)
	 */
	public KomodoObject getNode() {
		return node;
	}
	
	/**
	 * @return the corresponding context of the node (if found)
	 */
	public WorkspaceContext getNodeContext() {
		return context;
	}
}