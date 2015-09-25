/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model;

import static org.komodo.relational.commands.model.ModelCommandMessages.AddStoredProcedureCommand.ADD_STORED_PROCEDURE_ERROR;
import static org.komodo.relational.commands.model.ModelCommandMessages.AddStoredProcedureCommand.STORED_PROCEDURE_ADDED;
import static org.komodo.relational.commands.model.ModelCommandMessages.General.MISSING_STORED_PROCEDURE_NAME;
import org.komodo.relational.model.Model;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add a Stored Procedure to a Model.
 */
public final class AddStoredProcedureCommand extends ModelShellCommand {

    static final String NAME = "add-stored-procedure"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddStoredProcedureCommand( final WorkspaceStatus status ) {
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
            final String procName = requiredArgument( 0, getMessage( MISSING_STORED_PROCEDURE_NAME ) );

            final Model model = getModel();
            model.addStoredProcedure( getTransaction(), procName );

            result = new CommandResultImpl( getMessage( STORED_PROCEDURE_ADDED, procName ) );
        } catch ( final Exception e ) {
            result = new CommandResultImpl( false, getMessage( ADD_STORED_PROCEDURE_ERROR ), e );
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
