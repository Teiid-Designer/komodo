package org.komodo.shell.commands.core;

import java.util.ArrayList;
import java.util.List;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;

/**
 * renames the referenced node
 *
 * @author blafond
 *
 */
public class RenameCommand extends BuiltInShellCommand implements StringConstants {

    private static final String RENAME = "rename"; //$NON-NLS-1$

    /**
     * Constructor.
     * @param wsStatus the workspace status
     */
    public RenameCommand(WorkspaceStatus wsStatus) {
        super(RENAME, wsStatus);
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#execute()
     */
    @Override
    public boolean execute() throws Exception {
        String objNameArg = requiredArgument(0, Messages.getString("RenameCommand.InvalidArgMsg_ObjectName")); //$NON-NLS-1$
        String newName = requiredArgument(1, Messages.getString("RenameCommand.InvalidArgMsg_NewName")); //$NON-NLS-1$

        // Validate that specified object exists
        if (!validateChildExists(objNameArg,getWorkspaceStatus().getCurrentContext())) {
            return false;
        }
        // Validate the new name
        String childType = getWorkspaceStatus().getCurrentContext().getChild(objNameArg).getType();
        KomodoType kType = KomodoType.getKomodoType(childType);
        if (!validateObjectName(newName,kType)) {
            return false;
        }

        // Validate that the rename would not create a duplicate of same type
        if (!validateNotDuplicateType(objNameArg,newName,getWorkspaceStatus().getCurrentContext())) {
            return false;
        }

        try {
        	// Rename
            rename(objNameArg, newName);
            // Commit transaction
            getWorkspaceStatus().commit("RenameCommand"); //$NON-NLS-1$
            // Print message
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("RenameCommand.ObjectRenamed", objNameArg, newName)); //$NON-NLS-1$
        } catch (Exception e) {
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("RenameCommand.Failure", objNameArg)); //$NON-NLS-1$
            print(CompletionConstants.MESSAGE_INDENT, TAB + e.getMessage());
            return false;
        }
        return true;
    }

    /**
     * Validate a child with the specified name exists
     * @param name the child name
     * @param context the parent context
     * @return 'true' if exists, 'false' if not.
     */
    private boolean validateChildExists(String name, WorkspaceContext context) throws Exception {
    	WorkspaceContext childContext = context.getChild(name);
    	if(childContext==null) {
        	print(CompletionConstants.MESSAGE_INDENT,Messages.getString("RenameCommand.cannotRename_objectDoesNotExist", name)); //$NON-NLS-1$
    		return false;
    	}
        return true;
    }

    /**
     * Validates whether another child of the same name and type already exists
     * @param oldName the name of the object being renamed
     * @param newName the new child name
     * @param context the parent context
     * @return 'true' if exists, 'false' if not.
     */
    private boolean validateNotDuplicateType(String oldName, String newName, WorkspaceContext context) throws Exception {
    	// The child being renamed.
    	WorkspaceContext childToRename = context.getChild(oldName);
    	// Determine if child with specifed name already exists
    	WorkspaceContext existingChild = context.getChild(newName);
    	// If child exists, check the type
    	if(existingChild!=null) {
    		if(existingChild.getType().equals(childToRename.getType())) {
    			print(CompletionConstants.MESSAGE_INDENT,Messages.getString("RenameCommand.cannotRename_wouldCreateDuplicate", newName)); //$NON-NLS-1$
    			return false;
    		}
    	}
        return true;
    }

    private void rename(String objName, String newName) throws Exception {
        WorkspaceStatus wsStatus = getWorkspaceStatus();

        KomodoObject parent = wsStatus.getCurrentContext().getKomodoObj();

        KomodoObject child = parent.getChild(wsStatus.getTransaction(), objName);
        if( child != null ) {
        	child.rename(wsStatus.getTransaction(), newName);
        } else {
        	throw new Exception(Messages.getString("RenameCommand.cannotRename_objectDoesNotExist", objName)); //$NON-NLS-1$
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
