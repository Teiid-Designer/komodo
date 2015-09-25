/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.table;

import static org.komodo.relational.commands.table.TableCommandMessages.DeleteForeignKeyCommand.DELETE_FOREIGN_KEY_ERROR;
import static org.komodo.relational.commands.table.TableCommandMessages.DeleteForeignKeyCommand.FOREIGN_KEY_DELETED;
import static org.komodo.relational.commands.table.TableCommandMessages.General.MISSING_FOREIGN_KEY_NAME;
import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Table;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to delete a ForeignKey from a Table.
 */
public final class DeleteForeignKeyCommand extends TableShellCommand {

    static final String NAME = "delete-foreign-key"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public DeleteForeignKeyCommand( final WorkspaceStatus status ) {
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
            table.removeForeignKey( getTransaction(), fkName );

            result = new CommandResultImpl( getMessage( FOREIGN_KEY_DELETED, fkName ) );
        } catch ( final Exception e ) {
            result = new CommandResultImpl( false, getMessage( DELETE_FOREIGN_KEY_ERROR ), e );
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
        final Table table = getTable();
        final ForeignKey[] fks = table.getForeignKeys( uow );
        List<String> existingFKNames = new ArrayList<String>(fks.length);
        for(ForeignKey fk : fks) {
            existingFKNames.add(fk.getName(uow));
        }

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( existingFKNames );
            } else {
                for ( final String item : existingFKNames ) {
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
