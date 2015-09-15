/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb.entry;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_PROPERTY_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.MISSING_PROPERTY_NAME_VALUE;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.Arrays;
import java.util.List;
import org.komodo.relational.vdb.Entry;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.core.SetPropertyCommand;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to set VDB Entry properties
 */
public final class SetEntryPropertyCommand extends EntryShellCommand {

    static final String NAME = "set-entry-property"; //$NON-NLS-1$

    private static final String DESCRIPTION = "description"; //$NON-NLS-1$
    private static final String PATH = "path"; //$NON-NLS-1$

    private static final List< String > ALL_PROPS = Arrays.asList( new String[] { DESCRIPTION, PATH } );

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetEntryPropertyCommand( final WorkspaceStatus status ) {
        super( NAME, true, status );
        // Overrides the BuiltInCommand "set-property"
        setOverriddenCommands(new String[]{SetPropertyCommand.NAME});
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String name = requiredArgument( 0, getWorkspaceMessage(MISSING_PROPERTY_NAME_VALUE) );
        final String value = requiredArgument( 1, getWorkspaceMessage(MISSING_PROPERTY_NAME_VALUE) );

        final Entry entry = getEntry();
        final UnitOfWork transaction = getTransaction();
        boolean success = true;

        switch ( name ) {
            case DESCRIPTION:
                entry.setDescription( transaction, value );
                break;
            case PATH:
                entry.setPath( transaction, value );
                break;
            default:
                success = false;
                print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_PROPERTY_NAME, NAME ) );
                break;
        }

        return success;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        final Arguments args = getArguments();

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( ALL_PROPS );
            } else {
                for ( final String item : ALL_PROPS ) {
                    if ( item.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) {
                        candidates.add( item );
                    }
                }
            }

            return 0;
        }

        // no tab completion
        return -1;
    }

}
