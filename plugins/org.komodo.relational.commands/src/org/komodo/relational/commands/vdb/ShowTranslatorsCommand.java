/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import static org.komodo.relational.commands.vdb.VdbCommandMessages.ShowTranslatorsCommand.NO_TRANSLATORS;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.ShowTranslatorsCommand.TRANSLATORS_HEADER;
import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.General.PRINT_RELATIONAL_OBJECT;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to show all translators in a VDB.
 */
public final class ShowTranslatorsCommand extends VdbShellCommand {

    static final String NAME = "show-translators"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ShowTranslatorsCommand( final WorkspaceStatus status ) {
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
            final UnitOfWork uow = getTransaction();
            final Vdb vdb = getVdb();
            final Translator[] translators = vdb.getTranslators( uow );

            if ( translators.length == 0 ) {
                print( MESSAGE_INDENT, getMessage( NO_TRANSLATORS, vdb.getName( uow ) ) );
            } else {
                print( MESSAGE_INDENT, getMessage( TRANSLATORS_HEADER, vdb.getName( uow ) ) );

                final int indent = (MESSAGE_INDENT * 2);

                for ( final Translator translator : translators ) {
                    print( indent,
                           getMessage( PRINT_RELATIONAL_OBJECT, translator.getName( uow ), translator.getTypeDisplayName() ) );
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
