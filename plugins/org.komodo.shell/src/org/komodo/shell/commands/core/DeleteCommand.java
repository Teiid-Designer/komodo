package org.komodo.shell.commands.core;

import java.util.List;

import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

/**
 * Deletes the referenced node from the repository
 * @author blafond
 *
 */
public class DeleteCommand extends BuiltInShellCommand implements StringConstants {

    private static final String DELETE = "delete"; //$NON-NLS-1$
    
    /**
     * Constructor.
     * @param wsStatus the workspace status
     */
    public DeleteCommand(WorkspaceStatus wsStatus) {
        super(DELETE, wsStatus);
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#execute()
     */
    @Override
    public boolean execute() throws Exception {
        String objPathArg = requiredArgument(0, Messages.getString("DeleteCommand.InvalidArgMsg_ObjectPath")); //$NON-NLS-1$

		if (!this.validate(objPathArg)) {
			return false;
		}
		
        try {
            delete(objPathArg);
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("DeleteCommand.ObjectDeleted", objPathArg)); //$NON-NLS-1$
            if (getWorkspaceStatus().getRecordingStatus())
                recordCommand(getArguments());
        } catch (Exception e) {
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("DeleteCommand.Failure", objPathArg)); //$NON-NLS-1$
            print(CompletionConstants.MESSAGE_INDENT, TAB + e.getMessage());
            return false;
        }
        return true;
    }

	protected boolean validate(String... args) throws Exception {
		String pathArg = args[0];
		
		// Validate path is a valid absolute or relative path
		if (!validatePath(pathArg)) {
			return false;
		}
		
		// Requested context to delete
		WorkspaceContext contextToDelete = ContextUtils.getContextForPath(getWorkspaceStatus(), pathArg);
		
		int contextLevel = ContextUtils.getContextLevel(contextToDelete);
		// Cannot delete the root or workspace!
		if(contextLevel<=1) {
            print(CompletionConstants.MESSAGE_INDENT,Messages.getString("DeleteCommand.cantDeleteReserved",contextToDelete.getFullName())); //$NON-NLS-1$
			return false;
		}
		
		// The context for the delete must be *below* the current context.
		if(!ContextUtils.isContextBelow(getWorkspaceStatus().getCurrentContext(),contextToDelete)) {
            print(CompletionConstants.MESSAGE_INDENT,Messages.getString("DeleteCommand.contextMustBeBelowCurrent",contextToDelete.getFullName())); //$NON-NLS-1$
			return false;
		}
		
		return true;
	}
	
    private void delete(String objPath) throws Exception {
    	WorkspaceStatus wsStatus = getWorkspaceStatus();
        Repository repository = wsStatus.getCurrentContext().getRepository();
        WorkspaceManager wkspManager = WorkspaceManager.getInstance(repository);
        
        // Get the Komodo object to delete
        WorkspaceContext contextToDelete = ContextUtils.getContextForPath(wsStatus, objPath);
        KomodoObject objToDelete = contextToDelete.getKomodoObj();

        if( objToDelete != null ) {
        	wkspManager.delete(null, objToDelete);
        } else {
        	throw new Exception(Messages.getString("DeleteCommand.cannotDelete_objectDoesNotExist", objPath)); //$NON-NLS-1$
        }
    }
    
	/**
	 * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
	 */
	@Override
	public int tabCompletion(String lastArgument, List<CharSequence> candidates) throws Exception {
		if (getArguments().isEmpty()) {
			WorkspaceContext currentContext = getWorkspaceStatus().getCurrentContext();
			
			// The arg is expected to be a path
			updateTabCompleteCandidatesForPath(candidates, currentContext, false, lastArgument);

			// Do not put space after it - may want to append more to the path
			return CompletionConstants.NO_APPEND_SEPARATOR;
		}
		return -1;
	}
        
}
