/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.tabularresultset;

import static org.komodo.relational.commands.tabularresultset.TabularResultSetCommandMessages.DeleteColumnCommand.COLUMN_DELETED;
import static org.komodo.relational.commands.tabularresultset.TabularResultSetCommandMessages.General.MISSING_COLUMN_NAME;
import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.model.ResultSetColumn;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to delete a Column from a TabularResultSet.
 */
public final class DeleteColumnCommand extends TabularResultSetShellCommand {

    static final String NAME = "delete-column"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public DeleteColumnCommand( final WorkspaceStatus status ) {
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
            final String columnName = requiredArgument( 0, getMessage( MISSING_COLUMN_NAME ) );

            final TabularResultSet resultSet = getTabularResultSet();
            resultSet.removeColumn( getTransaction(), columnName );

            result = new CommandResultImpl( getMessage( COLUMN_DELETED, columnName ) );
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

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        final Arguments args = getArguments();

        final UnitOfWork uow = getTransaction();
        final TabularResultSet resultSet = getTabularResultSet();
        final ResultSetColumn[] columns = resultSet.getColumns( uow );
        List<String> existingColumnNames = new ArrayList<String>(columns.length);
        for(ResultSetColumn column : columns) {
            existingColumnNames.add(column.getName(uow));
        }

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( existingColumnNames );
            } else {
                for ( final String item : existingColumnNames ) {
                    if ( item.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) {
                        candidates.add( item );
                    }
                }
            }

            return 0;
        }

        // no tab completion
        return -1;
    }

}
