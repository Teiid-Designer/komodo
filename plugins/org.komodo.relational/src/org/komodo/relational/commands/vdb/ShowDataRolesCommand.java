/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.PRINT_RELATIONAL_OBJECT;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.ShowDataRolesCommand.NO_DATA_ROLES;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Vdb;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to show all data roles in a VDB.
 */
public final class ShowDataRolesCommand extends VdbShellCommand {

    static final String NAME = "show-data-roles"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ShowDataRolesCommand( final WorkspaceStatus status ) {
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
        final DataRole[] dataRoles = vdb.getDataRoles( uow );

        if ( dataRoles.length == 0 ) {
            print( MESSAGE_INDENT, getMessage(NO_DATA_ROLES) );
        } else {
            for ( final DataRole role : dataRoles ) {
                final String name = role.getName( uow );
                print( MESSAGE_INDENT, getWorkspaceMessage(PRINT_RELATIONAL_OBJECT, name, getDisplayType( role ) ) );
            }
        }

        return true;
    }

}
