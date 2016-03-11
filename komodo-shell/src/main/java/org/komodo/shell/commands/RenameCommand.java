package org.komodo.shell.commands;

import java.util.ArrayList;
import java.util.List;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.ShellI18n;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 * A {@link ShellCommand command} that renames the referenced node.
 * <p>
 * Usage:
 * <p>
 * <code>&nbsp;&nbsp;
 * rename &lt;new-name | child-name&gt;&nbsp;[new-child-name]
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
        try {
            final String name = requiredArgument( 0, I18n.bind( ShellI18n.missingRenameFirstArg ) );
            final String newChildName = optionalArgument( 1 ); // for renaming a child
            final boolean renamingChild = !StringUtils.isBlank( newChildName );

            KomodoObject objToRename = null;
            String newName = null;
            final KomodoObject context = getContext();

            if ( renamingChild ) {
                if ( !context.hasChild( getTransaction(), name ) ) {
                    return new CommandResultImpl( false, I18n.bind( ShellI18n.childDoesNotExistToRename,
                                                                    getWorkspaceStatus().getDisplayPath( context, null ),
                                                                    name ),
                                                  null );
                }

                objToRename = context.getChild( getTransaction(), name );
                newName = newChildName;
            } else {
                objToRename = context;
                newName = name; // renaming current context
            }

            assert ( objToRename != null );
            assert ( newName != null );

            { // if new name is same as old name do nothing
                final String oldName = objToRename.getName( getTransaction() );

                if ( oldName.equals( newName ) ) {
                    return new CommandResultImpl( false, I18n.bind( ShellI18n.renameNewNameNotDifferent, newName ), null );
                }
            }

            { // Make sure rename does not create a duplicate of same type
                final KomodoObject parent = objToRename.getParent( getTransaction() );

                // cannot rename Komodo root
                if ( parent == null ) {
                    return new CommandResultImpl( false, I18n.bind( ShellI18n.cannotRenameKomodoRoot ), null );
                }

                final KomodoObject[] sameNamedKids = parent.getChildren( getTransaction(), newName );

                if ( sameNamedKids.length != 0 ) {
                    final Descriptor primaryType = objToRename.getPrimaryType( getTransaction() );

                    for ( final KomodoObject kid : sameNamedKids ) {
                        if ( primaryType.equals( kid.getPrimaryType( getTransaction() ) ) ) {
                            return new CommandResultImpl( false,
                                                          I18n.bind( ShellI18n.cannotRenameWouldCreateDuplicate, newName ),
                                                          null );
                        }
                    }
                }
            }

            // do the rename
            objToRename.rename( getTransaction(), newName );
            return new CommandResultImpl( I18n.bind( ShellI18n.objectRenamed, name, newName ) );
        } catch ( Exception e ) {
            return new CommandResultImpl( e );
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

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
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
    				if (item.startsWith(lastArgument)) {
    					candidates.add(item);
    				}
    			}
    		}
        }
        return TabCompletionModifier.AUTO;
    }

}
