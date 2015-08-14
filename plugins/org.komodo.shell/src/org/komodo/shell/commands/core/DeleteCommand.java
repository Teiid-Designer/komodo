package org.komodo.shell.commands.core;

import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * Deletes the referenced node from the repository
 * @author blafond
 *
 */
public class DeleteCommand extends BuiltInShellCommand implements StringConstants {

    /**
     * The command name.
     */
    public static final String NAME = "delete"; //$NON-NLS-1$

    /**
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     */
    public DeleteCommand( final WorkspaceStatus wsStatus ) {
        super( wsStatus, NAME, "rm" ); //$NON-NLS-1$
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#execute()
     */
    @Override
    public boolean execute() throws Exception {
        String objPathArg = requiredArgument(0, Messages.getString(Messages.DeleteCommand.InvalidArgMsg_ObjectPath));

		if (!this.validate(objPathArg)) {
			return false;
		}

        try {
        	// Delete
            delete(objPathArg);
            // Commit transaction
            if ( isAutoCommit() ) {
                getWorkspaceStatus().commit( DeleteCommand.class.getSimpleName() );
            }
            // Print message
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.DeleteCommand.ObjectDeleted, objPathArg));
        } catch (Exception e) {
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.DeleteCommand.Failure, objPathArg));
            print(CompletionConstants.MESSAGE_INDENT, TAB + e.getMessage());
            return false;
        }
        return true;
    }

	protected boolean validate(String... args) throws Exception {
		String pathArg = args[0];

		// Requested context to delete
		WorkspaceContext contextToDelete = ContextUtils.getContextForPath(getWorkspaceStatus(), pathArg);
		// Error if could not locate context
		if (contextToDelete==null) {
            print(CompletionConstants.MESSAGE_INDENT,Messages.getString(Messages.DeleteCommand.cannotDelete_objectDoesNotExist, pathArg));
			return false;
		}

		int contextLevel = ContextUtils.getContextLevel(contextToDelete);
		// Cannot delete the workspace!
		if(contextLevel<=0) {
            print(CompletionConstants.MESSAGE_INDENT,Messages.getString(Messages.DeleteCommand.cantDeleteReserved,contextToDelete.getFullName()));
			return false;
		}

		return true;
	}

    private void delete(String objPath) throws Exception {
    	WorkspaceStatus wsStatus = getWorkspaceStatus();
        WorkspaceManager wkspManager = wsStatus.getCurrentContext().getWorkspaceManager();
        UnitOfWork transaction = wsStatus.getTransaction();

        // Get the Komodo object to delete
        WorkspaceContext contextToDelete = ContextUtils.getContextForPath(wsStatus, objPath);
        KomodoObject objToDelete = contextToDelete.getKomodoObj();

        if( objToDelete != null ) {
            // If teiid object is being deleted, check if it is set as the default server.  Unset default if necessary
            Teiid teiid = wkspManager.resolve(transaction, objToDelete, Teiid.class);
            if( teiid != null ) {
                Teiid defaultTeiid = wsStatus.getTeiid();
                if(defaultTeiid!=null && defaultTeiid.getName(transaction).equals(teiid.getName(transaction))) {
                    wsStatus.setTeiid(null);
                }
            }
            
            wkspManager.delete(transaction, objToDelete);
        } else {
        	throw new Exception(Messages.getString(Messages.DeleteCommand.cannotDelete_objectDoesNotExist, objPath));
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
