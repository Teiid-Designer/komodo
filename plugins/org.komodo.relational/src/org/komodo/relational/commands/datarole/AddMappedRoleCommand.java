/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.datarole;

import static org.komodo.relational.commands.datarole.DataRoleCommandMessages.AddMappedRoleCommand.ADD_MAPPED_ROLE_ERROR;
import static org.komodo.relational.commands.datarole.DataRoleCommandMessages.AddMappedRoleCommand.MAPPED_ROLE_ADDED;
import static org.komodo.relational.commands.datarole.DataRoleCommandMessages.General.MISSING_MAPPED_ROLE_NAME;
import org.komodo.relational.vdb.DataRole;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add a mapped role to a {@link DataRole}.
 */
public final class AddMappedRoleCommand extends DataRoleShellCommand {

    static final String NAME = "add-mapped-role"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddMappedRoleCommand( final WorkspaceStatus status ) {
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
            final String mappedRoleName = requiredArgument( 0, getMessage( MISSING_MAPPED_ROLE_NAME ) );

            final DataRole dataRole = getDataRole();
            dataRole.addMappedRole( getTransaction(), mappedRoleName );

            result = new CommandResultImpl( getMessage( MAPPED_ROLE_ADDED, mappedRoleName ) );
        } catch ( final Exception e ) {
            result = new CommandResultImpl( false, getMessage( ADD_MAPPED_ROLE_ERROR ), e );
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
        return 1;
    }

}
