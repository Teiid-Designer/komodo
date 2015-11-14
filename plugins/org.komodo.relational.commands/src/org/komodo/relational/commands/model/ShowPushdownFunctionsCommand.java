/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model;

import static org.komodo.relational.commands.model.ModelCommandMessages.ShowPushdownFunctionsCommand.NO_PUSHDOWN_FUNCTIONS;
import static org.komodo.relational.commands.model.ModelCommandMessages.ShowPushdownFunctionsCommand.PUSHDOWNS_HEADER;
import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.General.PRINT_RELATIONAL_OBJECT;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.model.Function;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to show all the {@link PushdownFunction pushdown functions} of a {@link Model model}.
 */
public final class ShowPushdownFunctionsCommand extends ModelShellCommand {

    static final String NAME = "show-pushdown-functions"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ShowPushdownFunctionsCommand( final WorkspaceStatus status ) {
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
            final Function[] functions = model.getFunctions( getTransaction() );

            if ( functions.length == 0 ) {
                print( MESSAGE_INDENT, getMessage( NO_PUSHDOWN_FUNCTIONS, model.getName( getTransaction() ) ) );
            } else {
                final List< Function > pushdowns = new ArrayList< >( functions.length );

                for ( final Function function : functions ) {
                    if ( PushdownFunction.RESOLVER.resolvable( getTransaction(), function ) ) {
                        pushdowns.add( function );
                    }
                }

                if ( pushdowns.isEmpty() ) {
                    print( MESSAGE_INDENT, getMessage( NO_PUSHDOWN_FUNCTIONS, model.getName( getTransaction() ) ) );
                } else {
                    print( MESSAGE_INDENT, getMessage( PUSHDOWNS_HEADER, model.getName( getTransaction() ) ) );

                    final int indent = (MESSAGE_INDENT * 2);

                    for ( final Function function : pushdowns ) {
                        print( indent,
                               getWorkspaceMessage( PRINT_RELATIONAL_OBJECT,
                                                    function.getName( getTransaction() ),
                                                    function.getTypeDisplayName() ) );
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
