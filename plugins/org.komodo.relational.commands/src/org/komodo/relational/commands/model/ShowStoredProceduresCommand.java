/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model;

import static org.komodo.relational.commands.model.ModelCommandMessages.ShowStoredProceduresCommand.NO_STORED_PROCEDURES;
import static org.komodo.relational.commands.model.ModelCommandMessages.ShowStoredProceduresCommand.STORED_PROCEDURES_HEADER;
import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.General.PRINT_RELATIONAL_OBJECT;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to show all the {@link StoredProcedure stored procedures} of a {@link Model model}.
 */
public final class ShowStoredProceduresCommand extends ModelShellCommand {

    static final String NAME = "show-stored-procedures"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ShowStoredProceduresCommand( final WorkspaceStatus status ) {
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
            final Model model = getModel();
            final Procedure[] procedures = model.getProcedures( getTransaction() );

            if ( procedures.length == 0 ) {
                print( MESSAGE_INDENT, getMessage( NO_STORED_PROCEDURES, model.getName( getTransaction() ) ) );
            } else {
                final List< Procedure > storedProcedures = new ArrayList< >( procedures.length );

                for ( final Procedure procedure : procedures ) {
                    if ( StoredProcedure.RESOLVER.resolvable( getTransaction(), procedure ) ) {
                        storedProcedures.add( procedure );
                    }
                }

                if ( storedProcedures.isEmpty() ) {
                    print( MESSAGE_INDENT, getMessage( NO_STORED_PROCEDURES, model.getName( getTransaction() ) ) );
                } else {
                    print( MESSAGE_INDENT, getMessage( STORED_PROCEDURES_HEADER, model.getName( getTransaction() ) ) );

                    final int indent = (MESSAGE_INDENT * 2);

                    for ( final Procedure storedProc : storedProcedures ) {
                        print( indent,
                               getWorkspaceMessage( PRINT_RELATIONAL_OBJECT,
                                                    storedProc.getName( getTransaction() ),
                                                    storedProc.getTypeDisplayName() ) );
                    }
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
