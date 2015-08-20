/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.PRINT_RELATIONAL_OBJECT;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.NO_MODELS;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.model.Model;
import org.komodo.relational.vdb.Vdb;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to show all models in a VDB.
 */
public final class ShowModelsCommand extends VdbShellCommand {

    static final String NAME = "show-models"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ShowModelsCommand( final WorkspaceStatus status ) {
        super( NAME, false, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final UnitOfWork uow = getTransaction();
        final Vdb vdb = getVdb();
        final Model[] models = vdb.getModels( uow );

        if ( models.length == 0 ) {
            print( MESSAGE_INDENT, NO_MODELS.getMessage() );
        } else {
            for ( final Model model : models ) {
                final String name = model.getName( uow );
                print( MESSAGE_INDENT, getMessage(PRINT_RELATIONAL_OBJECT, name, getDisplayType( model ) ) );
            }
        }

        return true;
    }

}
