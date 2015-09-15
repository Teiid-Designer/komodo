/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model;

import static org.komodo.relational.commands.model.ModelCommandMessages.AddUserDefinedFunctionCommand.USER_DEFINED_FUNCTION_ADDED;
import static org.komodo.relational.commands.model.ModelCommandMessages.General.MISSING_USER_DEFINED_FUNCTION_NAME;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.model.Model;
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
        super( NAME, true, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String udfName = requiredArgument( 0, getMessage(MISSING_USER_DEFINED_FUNCTION_NAME) );

        final Model model = getModel();
        model.addUserDefinedFunction( getTransaction(), udfName );

        // Print success message
        print(MESSAGE_INDENT, getMessage(USER_DEFINED_FUNCTION_ADDED,udfName));
        
        return true;
    }

}
