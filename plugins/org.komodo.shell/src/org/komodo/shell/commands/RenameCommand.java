package org.komodo.shell.commands;

import java.util.ArrayList;
import java.util.List;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.ShellI18n;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.KomodoObjectUtils;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.utils.i18n.I18n;

/**
 * A {@link ShellCommand command} that renames the referenced node.
 * <p>
 * Usage:
 * <p>
 * <code>&nbsp;&nbsp;
 * rename &lt;object-path&gt; &lt;new-name&gt;
 * </code>
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
            objNameArg = requiredArgument( 0, I18n.bind( ShellI18n.invalidArgMsgObjectName ) );
            String newName = requiredArgument( 1, I18n.bind( ShellI18n.invalidArgMsgNewName ) );

            WorkspaceStatus wsStatus = getWorkspaceStatus();
            // Get the context for current object and target object since they can be supplied with a path
            KomodoObject objContext = wsStatus.getContextForDisplayPath(objNameArg);

            // objContext null - Object could not be located
            if ( objContext == null ) {
                return new CommandResultImpl( false, I18n.bind( ShellI18n.cannotRenameObjectDoesNotExist, objNameArg ), null );
            }

            String[] pathSegs = newName.split(FORWARD_SLASH);
            String targetParentPath = getPath( pathSegs, pathSegs.length - 1 );
            KomodoObject targetContext = wsStatus.getContextForDisplayPath(targetParentPath);
            // only allow move to an existing context
            if ( targetContext == null ) {
                return new CommandResultImpl( false,
                                              I18n.bind( ShellI18n.cannotRenameTargetContextDoesNotExist, targetParentPath ),
                                              null );
            }

            // Check validity of the new object name
            String newShortName = pathSegs[ pathSegs.length - 1 ];

            // Validate that the rename would not create a duplicate of same type
            if (!validateNotDuplicateType(objContext,newShortName,targetContext)) {
                return new CommandResultImpl( I18n.bind( ShellI18n.cannotRenameWouldCreateDuplicate, newName ) );
            }

            // Rename
            rename( objContext, newShortName, targetContext );
            return new CommandResultImpl( I18n.bind( ShellI18n.objectRenamed, objNameArg, newName ) );
        } catch ( Exception e ) {
            return new CommandResultImpl( e );
        }
    }

    /**
     * Builds a path from the specified segments, starting at the root and including nLevels
     * @param pathSegments the array of segments
     * @param nLevels number of levels to include
     * @return the path
     */
    private String getPath(String[] pathSegments, int nLevels) {
        StringBuilder sb = new StringBuilder();
        for(int i=0; i<nLevels; i++) {
            if(i!=0) {
                sb.append(FORWARD_SLASH);
            }
            sb.append(pathSegments[i]);
        }
        return sb.toString();
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
        KomodoObject[] objsOfType = targetObject.getChildrenOfType(getTransaction(), objToRename.getTypeIdentifier(getTransaction()).getType());

        for(KomodoObject kObj : objsOfType) {
            String kObjName = kObj.getName(getTransaction());
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

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( ShellI18n.renameHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ShellI18n.renameExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ShellI18n.renameUsage ) );
    }

    private void rename(KomodoObject origObject, String newShortName, KomodoObject targetObject) throws Exception {
        //
        if( origObject != null ) {
        	String parentAbsPath = targetObject.getAbsolutePath();
        	String newChildPath = parentAbsPath + FORWARD_SLASH + newShortName;
        	origObject.rename( getTransaction(), newChildPath );
        } else {
        	throw new Exception(I18n.bind(ShellI18n.cannotRenameObjectDoesNotExist, origObject));
        }
    }

    /**
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public TabCompletionModifier tabCompletion(String lastArgument, List<CharSequence> candidates) throws Exception {
        if (getArguments().isEmpty()) {
			// List of potential completions
			List<String> potentialsList = new ArrayList<String>();
			KomodoObject[] children = getWorkspaceStatus().getCurrentContext().getChildren( getTransaction() );
			for(KomodoObject wsContext : children) {
				potentialsList.add(wsContext.getName( getTransaction() ));
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
        }
        return TabCompletionModifier.AUTO;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        return !KomodoObjectUtils.isRoot( getContext() );
    }

    /* (non-Javadoc)
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 2;
    }


}
