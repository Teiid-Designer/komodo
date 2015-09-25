package org.komodo.relational.commands;

import java.util.ArrayList;
import java.util.List;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.Messages;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.utils.StringUtils;

/**
 * renames the referenced node
 *
 * @author blafond
 *
 */
public class RenameCommand extends BuiltInShellCommand {

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
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        String objNameArg = null;

        try {
            objNameArg = requiredArgument( 0, Messages.getString( Messages.RenameCommand.InvalidArgMsg_ObjectName ) );
            String newName = requiredArgument( 1, Messages.getString( Messages.RenameCommand.InvalidArgMsg_NewName ) );

            WorkspaceStatus wsStatus = getWorkspaceStatus();
            // Get the context for current object and target object since they can be supplied with a path
            KomodoObject objContext = ContextUtils.getContextForPath( wsStatus, objNameArg );

            // objContext null - Object could not be located
            if ( objContext == null ) {
                return new CommandResultImpl( false,
                                              Messages.getString( Messages.RenameCommand.cannotRename_objectDoesNotExist,
                                                                  objNameArg ),
                                              null );
            }

            String[] pathSegs = ContextUtils.getPathSegments( newName );
            String targetParentPath = ContextUtils.getPath( pathSegs, pathSegs.length - 1 );
            KomodoObject targetContext = ContextUtils.getContextForPath( wsStatus, targetParentPath );
            // only allow move to an existing context
            if ( targetContext == null ) {
                return new CommandResultImpl( false,
                                              Messages.getString( Messages.RenameCommand.cannotRename_targetContextDoesNotExist,
                                                                  targetParentPath ),
                                              null );
            }

            // Check validity of the new object name
            String newShortName = pathSegs[ pathSegs.length - 1 ];
            KomodoType kType = objContext.getTypeIdentifier( wsStatus.getTransaction() );
            //        if (!validateObjectName(newShortName,kType)) {
            //            return false;
            //        }

            //        // make sure type is valid for the target context
            //        if ( !validateChildType( kType.getType(), targetContext ) ) {
            //        	return false;
            //        }

            // Validate that the rename would not create a duplicate of same type
            //        if (!validateNotDuplicateType(objContext,newShortName,targetContext)) {
            //            return false;
            //        }

            //        try {
            // If teiid object was renamed, check if it is set as the default server.  unset default if necessary
            //            if (KomodoType.TEIID == kType) {
            //                final String server = wsStatus.getServer();
            //
            //                if ((server != null) && server.equals(objContext.getName(wsStatus.getTransaction()))) {
            //                    wsStatus.setServer(null);
            //                }
            //            }

            // Rename
            rename( objContext, newShortName, targetContext );

            return new CommandResultImpl( Messages.getString( Messages.RenameCommand.ObjectRenamed, objNameArg, newName ) );
        } catch ( Exception e ) {
            if (StringUtils.isBlank( objNameArg )) {
                return new CommandResultImpl( false, Messages.getString( SHELL.CommandFailure, NAME ), e );
            }

            return new CommandResultImpl( false, Messages.getString( Messages.RenameCommand.Failure, objNameArg ), e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 2;
    }

//    /**
//     * Validates whether another child of the same name and type already exists
//     * @param objToRename the object being renamed
//     * @param newName the new child name
//     * @param targetContext the parent context
//     * @return 'true' if exists, 'false' if not.
//     */
//    private boolean validateNotDuplicateType(WorkspaceContext objToRename, String newName, WorkspaceContext targetContext) throws Exception {
//    	// Determine if child with new name and original object type already exists
//    	WorkspaceContext existingChild = targetContext.getChild(newName,objToRename.getType());
//    	// If child exists, check the type
//    	if(existingChild!=null) {
//    		print(CompletionConstants.MESSAGE_INDENT,Messages.getString(Messages.RenameCommand.cannotRename_wouldCreateDuplicate, newName));
//    		return false;
//    	}
//    	return true;
//    }

    private void rename(KomodoObject origObject, String newShortName, KomodoObject targetObject) throws Exception {
        //
        if( origObject != null ) {
        	String parentAbsPath = targetObject.getAbsolutePath();
        	String newChildPath = parentAbsPath + FORWARD_SLASH + newShortName;
        	origObject.rename(getWorkspaceStatus().getTransaction(), newChildPath);
        } else {
        	throw new Exception(Messages.getString(Messages.RenameCommand.cannotRename_objectDoesNotExist, origObject));
        }
    }

    /**
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion(String lastArgument, List<CharSequence> candidates) throws Exception {
        if (getArguments().isEmpty()) {
			// List of potential completions
			List<String> potentialsList = new ArrayList<String>();
			KomodoObject[] children = getWorkspaceStatus().getCurrentContext().getChildren(getWorkspaceStatus().getTransaction());
			for(KomodoObject wsContext : children) {
				potentialsList.add(wsContext.getName(getWorkspaceStatus().getTransaction()));
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

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        return true;
    }

}
