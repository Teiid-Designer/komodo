/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model;

import static org.komodo.relational.commands.model.ModelCommandMessages.ShowSourcesCommand.NO_SOURCES;
import static org.komodo.relational.commands.model.ModelCommandMessages.ShowSourcesCommand.SOURCES_HEADER;
import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.General.PRINT_RELATIONAL_OBJECT;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.model.Model;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to show all the {@link ModelSource sources} of a {@link Model model}.
 */
public final class ShowSourcesCommand extends ModelShellCommand {

    static final String NAME = "show-sources"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ShowSourcesCommand( final WorkspaceStatus status ) {
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
            final ModelSource[] sources = model.getSources( getTransaction() );

            if ( sources.length == 0 ) {
                print( MESSAGE_INDENT, getMessage( NO_SOURCES, model.getName( getTransaction() ) ) );
            } else {
                print( MESSAGE_INDENT, getMessage( SOURCES_HEADER, model.getName( getTransaction() ) ) );

                final int indent = (MESSAGE_INDENT * 2);

                for ( final ModelSource source : sources ) {
                    print( indent,
                           getWorkspaceMessage( PRINT_RELATIONAL_OBJECT,
                                                source.getName( getTransaction() ),
                                                source.getTypeDisplayName() ) );
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
