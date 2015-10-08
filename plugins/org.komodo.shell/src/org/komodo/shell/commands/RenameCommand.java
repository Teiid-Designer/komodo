package org.komodo.shell.commands;

import java.util.ArrayList;
import java.util.List;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.Messages;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.repository.KomodoObject;

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
        String objNameArg=null;

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

            // Validate that the rename would not create a duplicate of same type
            if (!validateNotDuplicateType(objContext,newShortName,targetContext)) {
                return new CommandResultImpl( Messages.getString( Messages.RenameCommand.cannotRename_wouldCreateDuplicate, newName ) );
            }

            // Rename
            rename( objContext, newShortName, targetContext );
            return new CommandResultImpl( Messages.getString( Messages.RenameCommand.ObjectRenamed, objNameArg, newName ) );
        } catch ( Exception e ) {
            return new CommandResultImpl( e );
        }
    }

    /**
     * Validates whether another child of the same name and type already exists
     * @param objToRename the object being renamed
     * @param newName the new child name
     * @param targetContext the parent context
     * @return 'true' if exists, 'false' if not.
     */
    private boolean validateNotDuplicateType(KomodoObject objToRename, String newName, KomodoObject targetObject) throws Exception {
        boolean hasExistingWithName = false;
        KomodoObject[] objsOfType = targetObject.getChildrenOfType(getWorkspaceStatus().getTransaction(), objToRename.getTypeIdentifier(getWorkspaceStatus().getTransaction()).getType());

        for(KomodoObject kObj : objsOfType) {
            String kObjName = kObj.getName(getWorkspaceStatus().getTransaction());
            if(kObjName.equals(newName)) {
                hasExistingWithName = true;
                break;
            }
        }

        // Existing with supplied name not found - not a duplicate
        if(hasExistingWithName) {
            return false;
        }

    	return true;
    }

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

    /* (non-Javadoc)
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 2;
    }


}
