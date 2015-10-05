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
import org.komodo.relational.model.Model;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
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
            final String tableName = requiredArgument( 0, getMessage( MISSING_TABLE_NAME ) );

            final Model model = getModel();
            model.addTable( getTransaction(), tableName );

            result = new CommandResultImpl( getMessage( TABLE_ADDED, tableName ) );
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
