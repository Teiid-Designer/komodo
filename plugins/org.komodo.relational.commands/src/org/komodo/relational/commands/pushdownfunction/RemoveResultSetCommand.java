/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.pushdownfunction;

import static org.komodo.relational.commands.pushdownfunction.PushdownFunctionCommandMessages.RemoveResultSetCommand.RESULT_SET_REMOVED;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to remove the result set for a PushdownFunction.
 */
public final class RemoveResultSetCommand extends PushdownFunctionShellCommand {

    static final String NAME = "remove-result-set"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public RemoveResultSetCommand( final WorkspaceStatus status ) {
        super( NAME, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        CommandResult result = null;

        try {
            final PushdownFunction func = getPushdownFunction();
            func.removeResultSet( getTransaction() );

            result = new CommandResultImpl( getMessage( RESULT_SET_REMOVED ) );
        } catch ( final Exception e ) {
            result = new CommandResultImpl( e );
        }

        return result;
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

}
