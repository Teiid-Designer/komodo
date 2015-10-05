/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.shell.commands;

import static org.komodo.shell.Messages.SHELL.INVALID_BOOLEAN_GLOBAL_PROPERTY_VALUE;
import static org.komodo.shell.Messages.SetAutoCommitCommand.ENABLE_FLAG_MISSING;
import static org.komodo.shell.Messages.SetGlobalPropertyCommand.GlobalPropertySet;
import java.util.List;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.Messages;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A command that can enable or disable the automatic committing of commands.
 */
public class SetAutoCommitCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "set-auto-commit"; //$NON-NLS-1$

    /**
     * @param status
     *        the workspace status (cannot be <code>null</code>)
     */
    public SetAutoCommitCommand( final WorkspaceStatus status ) {
        super( status, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        try {
            final String arg = requiredArgument( 0, Messages.getString( ENABLE_FLAG_MISSING ) );

            // Check for invalid arg
            if ( !arg.equalsIgnoreCase( Boolean.TRUE.toString() ) && !arg.equalsIgnoreCase( Boolean.FALSE.toString() ) ) {
                return new CommandResultImpl( false,
                                              Messages.getString( INVALID_BOOLEAN_GLOBAL_PROPERTY_VALUE,
                                                                  arg,
                                                                  WorkspaceStatus.AUTO_COMMIT ),
                                              null );
            }

            return new CommandResultImpl( Messages.getString( GlobalPropertySet, WorkspaceStatus.AUTO_COMMIT ) );
        } catch ( final Exception e ) {
            return new CommandResultImpl( false, Messages.getString( SHELL.CommandFailure, NAME ), e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 1;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        return !getWorkspaceStatus().isAutoCommit();
    }

    /**
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        if ( getArguments().size() == 0 ) {
            updateCandidatesForBooleanProperty( lastArgument, candidates );
        }

        return ( candidates.isEmpty() ? -1 : ( toString().length() + 1 ) );
    }

}
