/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.PRINT_RELATIONAL_OBJECT;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.NO_ENTRIES;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.vdb.Entry;
import org.komodo.relational.vdb.Vdb;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to show all entries in a VDB.
 */
public final class ShowEntriesCommand extends VdbShellCommand {

    static final String NAME = "show-entries"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ShowEntriesCommand( final WorkspaceStatus status ) {
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
        final Entry[] entries = vdb.getEntries( uow );

        if ( entries.length == 0 ) {
            print( MESSAGE_INDENT, NO_ENTRIES.getMessage() );
        } else {
            for ( final Entry entry : entries ) {
                final String name = entry.getName( uow );
                print( MESSAGE_INDENT, getMessage(PRINT_RELATIONAL_OBJECT, name, getDisplayType( entry ) ) );
            }
        }

        return true;
    }

}
