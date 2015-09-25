/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.view;

import static org.komodo.relational.commands.view.ViewCommandMessages.DeleteColumnCommand.COLUMN_DELETED;
import static org.komodo.relational.commands.view.ViewCommandMessages.DeleteColumnCommand.DELETE_COLUMN_ERROR;
import static org.komodo.relational.commands.view.ViewCommandMessages.General.MISSING_COLUMN_NAME;
import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.View;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to delete a Column from a View.
 */
public final class DeleteColumnCommand extends ViewShellCommand {

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

            final View view = getView();
            view.removeColumn( getTransaction(), columnName );

            result = new CommandResultImpl( getMessage( COLUMN_DELETED, columnName ) );
        } catch ( final Exception e ) {
            result = new CommandResultImpl( false, getMessage( DELETE_COLUMN_ERROR ), e );
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
        final View view = getView();
        final Column[] columns = view.getColumns( uow );
        List<String> existingColumnNames = new ArrayList<String>(columns.length);
        for(Column column : columns) {
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
