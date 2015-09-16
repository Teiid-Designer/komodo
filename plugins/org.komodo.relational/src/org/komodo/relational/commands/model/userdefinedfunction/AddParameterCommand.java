/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model.userdefinedfunction;

import static org.komodo.relational.commands.model.userdefinedfunction.UserDefinedFunctionCommandMessages.AddParameterCommand.PARAMETER_ADDED;
import static org.komodo.relational.commands.model.userdefinedfunction.UserDefinedFunctionCommandMessages.General.MISSING_PARAMETER_NAME;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.model.UserDefinedFunction;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add a Parameter to a UserDefinedFunction.
 */
public final class AddParameterCommand extends UserDefinedFunctionShellCommand {

    static final String NAME = "add-uf-parameter"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddParameterCommand( final WorkspaceStatus status ) {
        super( NAME, true, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String paramName = requiredArgument( 0, getMessage(MISSING_PARAMETER_NAME) );

        final UserDefinedFunction func = getUserDefinedFunction();
        func.addParameter(getTransaction(), paramName);
        
        // Print success message
        print(MESSAGE_INDENT, getMessage(PARAMETER_ADDED,paramName));
        
        return true;
    }

}
