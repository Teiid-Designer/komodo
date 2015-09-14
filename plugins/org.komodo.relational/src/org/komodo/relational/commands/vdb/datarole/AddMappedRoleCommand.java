/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb.datarole;

import static org.komodo.relational.commands.vdb.datarole.DataRoleCommandMessages.AddMappedRoleCommand.MISSING_MAPPED_ROLE_NAME;
import org.komodo.relational.vdb.DataRole;
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
        super( NAME, true, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String mappedRoleName = requiredArgument( 0, getMessage(MISSING_MAPPED_ROLE_NAME) );

        final DataRole dataRole = getDataRole();
        dataRole.addMappedRole( getTransaction(), mappedRoleName );

        return true;
    }

}
