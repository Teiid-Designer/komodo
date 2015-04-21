package org.komodo.shell.commands.core;

import java.util.ArrayList;
import java.util.List;

import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

/**
 * Deletes the referenced node from the repository
 * @author blafond
 *
 */
public class DeleteCommand extends BuiltInShellCommand implements StringConstants {

    /**
     * Constructor.
     * @param name the command name
     * @param wsStatus the workspace status
     */
    public DeleteCommand(String name, WorkspaceStatus wsStatus) {
        super(name, wsStatus);
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#execute()
     */
    @Override
    public boolean execute() throws Exception {
        String objNameArg = requiredArgument(0, Messages.getString("DeleteCommand.InvalidArgMsg_ObjectName")); //$NON-NLS-1$

        try {
            delete(objNameArg);
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("DeleteCommand.ObjectDeleted", objNameArg)); //$NON-NLS-1$
            if (getWorkspaceStatus().getRecordingStatus())
                recordCommand(getArguments());
        } catch (Exception e) {
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("DeleteCommand.Failure", objNameArg)); //$NON-NLS-1$
            print(CompletionConstants.MESSAGE_INDENT, TAB + e.getMessage());
            return false;
        }
        return true;
    }

    private void delete(String objName) throws Exception {
        WorkspaceStatus wsStatus = getWorkspaceStatus();

        WorkspaceContext currentContext = wsStatus.getCurrentContext();
        Repository repository = wsStatus.getCurrentContext().getRepository();
        WorkspaceManager wkspManager = WorkspaceManager.getInstance(repository);
        KomodoObject parent = currentContext.getKomodoObj();
        
        KomodoObject child = parent.getChild(null, objName);
        if( child != null ) {
        	wkspManager.delete(null, child);
        } else {
        	throw new Exception(Messages.getString("DeleteCommand.cannotDelete_objectDoesNotExist", objName)); //$NON-NLS-1$
        }
    }
    
    /**
     * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion(String lastArgument, List<CharSequence> candidates) throws Exception {
        if (getArguments().isEmpty()) {
			// List of potential completions
			List<String> potentialsList = new ArrayList<String>();
			List<WorkspaceContext> children = getWorkspaceStatus().getCurrentContext().getChildren();
			for(WorkspaceContext wsContext : children) {
				potentialsList.add(wsContext.getName());
			}
    		// --------------------------------------------------------------
    		// No arg - offer children relative to current context.
    		// --------------------------------------------------------------
    		if(lastArgument==null) {
    			candidates.addAll(potentialsList);
    		// --------------------------------------------------------------
    		// One arg - determine the completion options for it.
    		// --------------------------------------------------------------
    		} else {
    			for (String item : potentialsList) {
    				if (item.toUpperCase().startsWith(lastArgument.toUpperCase())) {
    					candidates.add(item);
    				}
    			}
    		}
            return 0;
        }
        return -1;
    }
        
}
