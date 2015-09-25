/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.table;

import static org.komodo.relational.commands.table.TableCommandMessages.AddIndexCommand.ADD_INDEX_ERROR;
import static org.komodo.relational.commands.table.TableCommandMessages.AddIndexCommand.INDEX_ADDED;
import static org.komodo.relational.commands.table.TableCommandMessages.General.MISSING_INDEX_NAME;
import org.komodo.relational.model.Table;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add an Index to a Table.
 */
public final class AddIndexCommand extends TableShellCommand {

    static final String NAME = "add-index"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddIndexCommand( final WorkspaceStatus status ) {
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
            final String indexName = requiredArgument( 0, getMessage( MISSING_INDEX_NAME ) );

            final Table table = getTable();
            table.addIndex( getTransaction(), indexName );

            result = new CommandResultImpl( getMessage( INDEX_ADDED, indexName ) );
        } catch ( final Exception e ) {
            result = new CommandResultImpl( false, getMessage( ADD_INDEX_ERROR ), e );
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
