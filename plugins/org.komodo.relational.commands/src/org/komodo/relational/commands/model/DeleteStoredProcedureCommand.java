/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model;

import static org.komodo.relational.commands.model.ModelCommandMessages.DeleteStoredProcedureCommand.STORED_PROCEDURE_DELETED;
import static org.komodo.relational.commands.model.ModelCommandMessages.General.MISSING_STORED_PROCEDURE_NAME;
import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Procedure;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to delete a Stored Procedure from a Model.
 */
public final class DeleteStoredProcedureCommand extends ModelShellCommand {

    static final String NAME = "delete-stored-procedure"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public DeleteStoredProcedureCommand( final WorkspaceStatus status ) {
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
            final String procName = requiredArgument( 0, getMessage( MISSING_STORED_PROCEDURE_NAME ) );

            final Model model = getModel();
            model.removeProcedure( getTransaction(), procName );

            result = new CommandResultImpl( getMessage( STORED_PROCEDURE_DELETED, procName ) );
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
        final Model model = getModel();
        final Procedure[] procs = model.getProcedures( uow );
        List<String> existingProcNames = new ArrayList<String>(procs.length);
        for(Procedure proc : procs) {
            existingProcNames.add(proc.getName(uow));
        }

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( existingProcNames );
            } else {
                for ( final String item : existingProcNames ) {
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
