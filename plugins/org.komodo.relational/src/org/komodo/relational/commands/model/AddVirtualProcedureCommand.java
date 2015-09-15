/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model;

import static org.komodo.relational.commands.model.ModelCommandMessages.AddVirtualProcedureCommand.VIRTUAL_PROCEDURE_ADDED;
import static org.komodo.relational.commands.model.ModelCommandMessages.General.MISSING_VIRTUAL_PROCEDURE_NAME;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.model.Model;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add a Virtual Procedure to a Model.
 */
public final class AddVirtualProcedureCommand extends ModelShellCommand {

    static final String NAME = "add-virtual-procedure"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddVirtualProcedureCommand( final WorkspaceStatus status ) {
        super( NAME, true, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String procName = requiredArgument( 0, getMessage(MISSING_VIRTUAL_PROCEDURE_NAME) );

        final Model model = getModel();
        model.addVirtualProcedure( getTransaction(), procName );

        // Print success message
        print(MESSAGE_INDENT, getMessage(VIRTUAL_PROCEDURE_ADDED,procName));
        
        return true;
    }

}
