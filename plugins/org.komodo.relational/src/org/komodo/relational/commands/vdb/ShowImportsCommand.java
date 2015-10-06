/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import static org.komodo.relational.commands.vdb.VdbCommandMessages.General.NAME_TYPE_DISPLAY;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.ShowImportsCommand.IMPORTS_HEADER;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.ShowImportsCommand.NO_IMPORTS;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
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
            final UnitOfWork uow = getTransaction();
            final Vdb vdb = getVdb();
            final VdbImport[] imports = vdb.getImports( uow );

            if ( imports.length == 0 ) {
                result = new CommandResultImpl( getMessage( NO_IMPORTS, vdb.getName( uow ) ) );
            } else {
                print( MESSAGE_INDENT, getMessage( IMPORTS_HEADER, vdb.getName( uow ) ) );

                for ( final VdbImport theImport : imports ) {
                    print( MESSAGE_INDENT,
                           getMessage( NAME_TYPE_DISPLAY, theImport.getName( uow ), theImport.getTypeDisplayName() ) );
                }

                result = CommandResult.SUCCESS;
            }
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
        return 0;
    }

}
