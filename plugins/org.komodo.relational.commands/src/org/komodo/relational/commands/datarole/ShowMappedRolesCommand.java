/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.datarole;

import static org.komodo.relational.commands.datarole.DataRoleCommandMessages.ShowMappedRolesCommand.MAPPED_ROLES_HEADER;
import static org.komodo.relational.commands.datarole.DataRoleCommandMessages.ShowMappedRolesCommand.NO_MAPPED_ROLES;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.vdb.DataRole;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to show all the mapped roles of a {@link DataRole}.
 */
public final class ShowMappedRolesCommand extends DataRoleShellCommand {

    static final String NAME = "show-mapped-roles"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ShowMappedRolesCommand( final WorkspaceStatus status ) {
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
            final DataRole dataRole = getDataRole();
            final String[] roles = dataRole.getMappedRoles( getTransaction() );

            if ( roles.length == 0 ) {
                result = new CommandResultImpl( getMessage( NO_MAPPED_ROLES, dataRole.getName( getTransaction() ) ) );
            } else {
                print( MESSAGE_INDENT, getMessage( MAPPED_ROLES_HEADER, dataRole.getName( getTransaction() ) ) );

                final int indent = (MESSAGE_INDENT * 2);

                for ( final String role : roles ) {
                    print( indent, role );
                }
            }

            result = CommandResult.SUCCESS;
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
