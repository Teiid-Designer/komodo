/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model.pushdownfunction;

import static org.komodo.relational.commands.model.pushdownfunction.PushdownFunctionCommandMessages.AddParameterCommand.PARAMETER_ADDED;
import static org.komodo.relational.commands.model.pushdownfunction.PushdownFunctionCommandMessages.General.MISSING_PARAMETER_NAME;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add a Parameter to a PushdownFunction.
 */
public final class AddParameterCommand extends PushdownFunctionShellCommand {

    static final String NAME = "add-pf-parameter"; //$NON-NLS-1$

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

        final PushdownFunction func = getPushdownFunction();
        func.addParameter(getTransaction(), paramName);
        
        // Print success message
        print(MESSAGE_INDENT, getMessage(PARAMETER_ADDED,paramName));
        
        return true;
    }

}
