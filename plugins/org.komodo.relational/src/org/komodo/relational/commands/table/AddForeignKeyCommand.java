/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.table;

import static org.komodo.relational.commands.table.TableCommandMessages.AddForeignKeyCommand.FOREIGN_KEY_ADDED;
import static org.komodo.relational.commands.table.TableCommandMessages.General.MISSING_FOREIGN_KEY_NAME;
import org.komodo.relational.model.Table;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add a Foreign Key to a Table.
 */
public final class AddForeignKeyCommand extends TableShellCommand {

    static final String NAME = "add-foreign-key"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddForeignKeyCommand( final WorkspaceStatus status ) {
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
            final String fkName = requiredArgument( 0, getMessage( MISSING_FOREIGN_KEY_NAME ) );

            final Table table = getTable();
            //table.addForeignKey( getTransaction(), fkName );

            result = new CommandResultImpl( getMessage( FOREIGN_KEY_ADDED, fkName ) );
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
