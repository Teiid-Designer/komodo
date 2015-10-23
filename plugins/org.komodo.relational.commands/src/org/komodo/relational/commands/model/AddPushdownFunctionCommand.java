/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model;

import static org.komodo.relational.commands.model.ModelCommandMessages.AddPushdownFunctionCommand.PUSHDOWN_FUNCTION_ADDED;
import static org.komodo.relational.commands.model.ModelCommandMessages.General.MISSING_PUSHDOWN_FUNCTION_NAME;
import org.komodo.relational.model.Model;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add a Pushdown Function to a Model
 */
public final class AddPushdownFunctionCommand extends ModelShellCommand {

    static final String NAME = "add-pushdown-function"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddPushdownFunctionCommand( final WorkspaceStatus status ) {
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
            final String pushdownFunctionName = requiredArgument( 0, getMessage( MISSING_PUSHDOWN_FUNCTION_NAME ) );

            final Model model = getModel();
            model.addPushdownFunction( getTransaction(), pushdownFunctionName );

            result = new CommandResultImpl( getMessage( PUSHDOWN_FUNCTION_ADDED, pushdownFunctionName ) );
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
        return 1;
    }

}
