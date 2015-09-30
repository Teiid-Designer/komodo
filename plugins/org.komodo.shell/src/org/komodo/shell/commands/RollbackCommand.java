/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.shell.commands;

import static org.komodo.shell.Messages.SHELL.CommandFailure;
import static org.komodo.shell.Messages.SetGlobalPropertyCommand.GlobalPropertySet;
import java.util.List;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.Messages;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A command that rolls back the current transaction.
 */
public class RollbackCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "rollback"; //$NON-NLS-1$

    /**
     * @param status
     *        the workspace status (cannot be <code>null</code>)
     */
    public RollbackCommand( final WorkspaceStatus status ) {
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
            getWorkspaceStatus().rollback( RollbackCommand.class.getName() );
            return new CommandResultImpl( Messages.getString( GlobalPropertySet, WorkspaceStatus.AUTO_COMMIT ) );
        } catch ( final Exception e ) {
            return new CommandResultImpl( false, Messages.getString( CommandFailure, NAME ), e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 0;
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
        return -1; // no candidates to add
    }

}
