/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.PRINT_RELATIONAL_OBJECT;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.ShowImportsCommand.NO_IMPORTS;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to show all VDB imports in a VDB.
 */
public final class ShowImportsCommand extends VdbShellCommand {

    static final String NAME = "show-imports"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ShowImportsCommand( final WorkspaceStatus status ) {
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
        final VdbImport[] imports = vdb.getImports( uow );

        if ( imports.length == 0 ) {
            print( MESSAGE_INDENT, getMessage(NO_IMPORTS) );
        } else {
            for ( final VdbImport vdbImport : imports ) {
                final String name = vdbImport.getName( uow );
                print( MESSAGE_INDENT, getWorkspaceMessage(PRINT_RELATIONAL_OBJECT, name, getDisplayType( vdbImport ) ) );
            }
        }

        return true;
    }

}
