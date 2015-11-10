/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.table;

import static org.komodo.relational.commands.table.TableCommandMessages.ShowColumnsCommand.COLUMNS_HEADER;
import static org.komodo.relational.commands.table.TableCommandMessages.ShowColumnsCommand.NO_COLUMNS;
import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.General.PRINT_RELATIONAL_OBJECT;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Table;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to show all the {@link Column columns} of a {@link Table}.
 */
public final class ShowColumnsCommand extends TableShellCommand {

    static final String NAME = "show-columns"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ShowColumnsCommand( final WorkspaceStatus status ) {
        super( NAME, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        try {
            final Table table = getTable();
            final Column[] columns = table.getColumns( getTransaction() );

            if ( columns.length == 0 ) {
                print( MESSAGE_INDENT, getMessage( NO_COLUMNS, table.getName( getTransaction() ) ) );
            } else {
                print( MESSAGE_INDENT, getMessage( COLUMNS_HEADER, table.getName( getTransaction() ) ) );

                final int indent = (MESSAGE_INDENT * 2);

                for ( final Column column : columns ) {
                    print( indent,
                           getWorkspaceMessage( PRINT_RELATIONAL_OBJECT,
                                                column.getName( getTransaction() ),
                                                column.getTypeDisplayName() ) );
                }
            }

            return CommandResult.SUCCESS;
        } catch ( final Exception e ) {
            return new CommandResultImpl( e );
        }
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
