/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.table;

import static org.komodo.relational.commands.table.TableCommandMessages.DeletePrimaryKeyCommand.NO_PK_TO_REMOVE;
import static org.komodo.relational.commands.table.TableCommandMessages.DeletePrimaryKeyCommand.PRIMARY_KEY_DELETED;
import org.komodo.relational.model.Table;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to delete a PrimaryKey from a Table.
 */
public final class DeletePrimaryKeyCommand extends TableShellCommand {

    static final String NAME = "delete-primary-key"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public DeletePrimaryKeyCommand( final WorkspaceStatus status ) {
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
            final Table table = getTable();
            if( table.getPrimaryKey(getTransaction())==null ) {
                return new CommandResultImpl( getMessage( NO_PK_TO_REMOVE ));
            }
            table.removePrimaryKey( getTransaction()  );

            result = new CommandResultImpl( getMessage( PRIMARY_KEY_DELETED ) );
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
        return 0;
    }

}
