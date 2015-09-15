/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model.table;

import static org.komodo.relational.commands.model.table.TableCommandMessages.DeleteUniqueConstraintCommand.UNIQUE_CONSTRAINT_DELETED;
import static org.komodo.relational.commands.model.table.TableCommandMessages.General.MISSING_UNIQUE_CONSTRAINT_NAME;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to delete a UniqueConstraint from a Table.
 */
public final class DeleteUniqueConstraintCommand extends TableShellCommand {

    static final String NAME = "delete-unique-constraint"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public DeleteUniqueConstraintCommand( final WorkspaceStatus status ) {
        super( NAME, true, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String ucName = requiredArgument( 0, getMessage(MISSING_UNIQUE_CONSTRAINT_NAME) );

        final Table table = getTable();
        table.removeUniqueConstraint( getTransaction(), ucName );

        // Print success message
        print(MESSAGE_INDENT, getMessage(UNIQUE_CONSTRAINT_DELETED,ucName));
        
        return true;
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
        final UniqueConstraint[] ucs = table.getUniqueConstraints( uow );
        List<String> existingUCNames = new ArrayList<String>(ucs.length);
        for(UniqueConstraint uc : ucs) {
            existingUCNames.add(uc.getName(uow));
        }
        
        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( existingUCNames );
            } else {
                for ( final String item : existingUCNames ) {
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
