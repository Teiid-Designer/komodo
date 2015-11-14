/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model;

import static org.komodo.relational.commands.model.ModelCommandMessages.ShowViewsCommand.NO_VIEWS;
import static org.komodo.relational.commands.model.ModelCommandMessages.ShowViewsCommand.VIEWS_HEADER;
import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.General.PRINT_RELATIONAL_OBJECT;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.View;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to show all the {@link View views} of a {@link Model model}.
 */
public final class ShowViewsCommand extends ModelShellCommand {

    static final String NAME = "show-views"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ShowViewsCommand( final WorkspaceStatus status ) {
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
            final View[] views = model.getViews( getTransaction() );

            if ( views.length == 0 ) {
                print( MESSAGE_INDENT, getMessage( NO_VIEWS, model.getName( getTransaction() ) ) );
            } else {
                print( MESSAGE_INDENT, getMessage( VIEWS_HEADER, model.getName( getTransaction() ) ) );

                final int indent = (MESSAGE_INDENT * 2);

                for ( final View view : views ) {
                    print( indent,
                           getWorkspaceMessage( PRINT_RELATIONAL_OBJECT,
                                                view.getName( getTransaction() ),
                                                view.getTypeDisplayName() ) );
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
