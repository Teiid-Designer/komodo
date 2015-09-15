/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model;

import static org.komodo.relational.commands.model.ModelCommandMessages.AddTableCommand.TABLE_ADDED;
import static org.komodo.relational.commands.model.ModelCommandMessages.General.MISSING_TABLE_NAME;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.model.Model;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add a Table to a Model.
 */
public final class AddTableCommand extends ModelShellCommand {

    static final String NAME = "add-table"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddTableCommand( final WorkspaceStatus status ) {
        super( NAME, true, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String tableName = requiredArgument( 0, getMessage(MISSING_TABLE_NAME) );

        final Model model = getModel();
        model.addTable( getTransaction(), tableName );

        // Print success message
        print(MESSAGE_INDENT, getMessage(TABLE_ADDED,tableName));
        
        return true;
    }

}
