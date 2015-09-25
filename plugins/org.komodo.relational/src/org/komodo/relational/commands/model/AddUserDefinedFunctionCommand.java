/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model;

import static org.komodo.relational.commands.model.ModelCommandMessages.AddUserDefinedFunctionCommand.ADD_USER_DEFINED_FUNCTION_ERROR;
import static org.komodo.relational.commands.model.ModelCommandMessages.AddUserDefinedFunctionCommand.USER_DEFINED_FUNCTION_ADDED;
import static org.komodo.relational.commands.model.ModelCommandMessages.General.MISSING_USER_DEFINED_FUNCTION_NAME;
import org.komodo.relational.model.Model;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add a UserDefined Function to a Model.
 */
public final class AddUserDefinedFunctionCommand extends ModelShellCommand {

    static final String NAME = "add-user-defined-function"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddUserDefinedFunctionCommand( final WorkspaceStatus status ) {
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
            final String udfName = requiredArgument( 0, getMessage( MISSING_USER_DEFINED_FUNCTION_NAME ) );

            final Model model = getModel();
            model.addUserDefinedFunction( getTransaction(), udfName );

            result = new CommandResultImpl( getMessage( USER_DEFINED_FUNCTION_ADDED, udfName ) );
        } catch ( final Exception e ) {
            result = new CommandResultImpl( false, getMessage( ADD_USER_DEFINED_FUNCTION_ERROR ), e );
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
