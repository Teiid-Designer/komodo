package org.komodo.shell.commands.core;

import java.util.ArrayList;
import java.util.List;

import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.ContextUtils;
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

    /**
     * The command name.
     */
    public static final String NAME = "rename"; //$NON-NLS-1$

    /**
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     */
    public RenameCommand( final WorkspaceStatus wsStatus ) {
        super( wsStatus, NAME, "mv" ); //$NON-NLS-1$
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#execute()
     */
    @Override
    public boolean execute() throws Exception {
        String objNameArg = requiredArgument(0, Messages.getString("RenameCommand.InvalidArgMsg_ObjectName")); //$NON-NLS-1$
        String newName = requiredArgument(1, Messages.getString("RenameCommand.InvalidArgMsg_NewName")); //$NON-NLS-1$

        // Get the context for current object and target object since they can be supplied with a path
        WorkspaceContext objContext = ContextUtils.getContextForPath(getWorkspaceStatus(), objNameArg);
        
        // objContext null - Object could not be located
    	if(objContext==null) {
        	print(CompletionConstants.MESSAGE_INDENT,Messages.getString("RenameCommand.cannotRename_objectDoesNotExist", objNameArg)); //$NON-NLS-1$
    		return false;
    	}

    	String[] pathSegs = ContextUtils.getPathSegments(newName);
    	String targetParentPath = ContextUtils.getPath(pathSegs, pathSegs.length-1);
        WorkspaceContext targetContext = ContextUtils.getContextForPath(getWorkspaceStatus(), targetParentPath);
        // only allow move to an existing context
    	if(targetContext==null) {
        	print(CompletionConstants.MESSAGE_INDENT,Messages.getString("RenameCommand.cannotRename_targetContextDoesNotExist", targetParentPath)); //$NON-NLS-1$
    		return false;
    	}
    	
        // Check validity of the new object name
    	String newShortName = pathSegs[pathSegs.length-1];
    	KomodoType kType = objContext.getKomodoObj().getTypeIdentifier(getWorkspaceStatus().getTransaction());
//        if (!validateObjectName(newShortName,kType)) {
//            return false;
//        }

        // make sure type is valid for the target context
        if ( !validateChildType( kType.getType(), targetContext ) ) {
        	return false;
        }
        
        // Validate that the rename would not create a duplicate of same type
        if (!validateNotDuplicateType(objContext,newShortName,targetContext)) {
            return false;
        }

        try {
        	// Rename
            rename(objContext, newShortName, targetContext);
            // Commit transaction
            if ( isAutoCommit() ) {
                getWorkspaceStatus().commit( RenameCommand.class.getSimpleName() );
            }
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
     * Validates whether another child of the same name and type already exists
     * @param objToRename the object being renamed
     * @param newName the new child name
     * @param targetContext the parent context
     * @return 'true' if exists, 'false' if not.
     */
    private boolean validateNotDuplicateType(WorkspaceContext objToRename, String newName, WorkspaceContext targetContext) throws Exception {
    	// Determine if child with new name and original object type already exists
    	WorkspaceContext existingChild = targetContext.getChild(newName,objToRename.getType());
    	// If child exists, check the type
    	if(existingChild!=null) {
    		print(CompletionConstants.MESSAGE_INDENT,Messages.getString("RenameCommand.cannotRename_wouldCreateDuplicate", newName)); //$NON-NLS-1$
    		return false;
    	}
    	return true;
    }

    private void rename(WorkspaceContext objContext, String newShortName, WorkspaceContext targetContext) throws Exception {
        // Original Object to rename
        KomodoObject origObject = objContext.getKomodoObj();
        // 
        if( origObject != null ) {
        	String parentAbsPath = targetContext.getKomodoObj().getAbsolutePath();
        	String newChildPath = parentAbsPath + FORWARD_SLASH + newShortName;
        	origObject.rename(getWorkspaceStatus().getTransaction(), newChildPath);
        } else {
        	throw new Exception(Messages.getString("RenameCommand.cannotRename_objectDoesNotExist", origObject)); //$NON-NLS-1$
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
