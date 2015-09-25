/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.storedprocedure;

import static org.komodo.relational.commands.storedprocedure.StoredProcedureCommandMessages.AddParameterCommand.ADD_PARAMETER_ERROR;
import static org.komodo.relational.commands.storedprocedure.StoredProcedureCommandMessages.AddParameterCommand.PARAMETER_ADDED;
import static org.komodo.relational.commands.storedprocedure.StoredProcedureCommandMessages.General.MISSING_PARAMETER_NAME;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add a Parameter to a StoredProcedure.
 */
public final class AddParameterCommand extends StoredProcedureShellCommand {

    static final String NAME = "add-parameter"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddParameterCommand( final WorkspaceStatus status ) {
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
            final String paramName = requiredArgument( 0, getMessage( MISSING_PARAMETER_NAME ) );

            final StoredProcedure proc = getStoredProcedure();
            proc.addParameter( getTransaction(), paramName );

            result = new CommandResultImpl( getMessage( PARAMETER_ADDED, paramName ) );
        } catch ( final Exception e ) {
            result = new CommandResultImpl( false, getMessage( ADD_PARAMETER_ERROR ), e );
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
