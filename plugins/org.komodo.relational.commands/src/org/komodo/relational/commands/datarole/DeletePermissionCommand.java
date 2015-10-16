/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.datarole;

import static org.komodo.relational.commands.datarole.DataRoleCommandMessages.DeletePermissionCommand.PERMISSION_DELETED;
import static org.komodo.relational.commands.datarole.DataRoleCommandMessages.General.MISSING_PERMISSION_NAME;
import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Permission;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to delete a Permission from a DataRole.
 */
public final class DeletePermissionCommand extends DataRoleShellCommand {

    static final String NAME = "delete-permission"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public DeletePermissionCommand( final WorkspaceStatus status ) {
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
            final String permissionName = requiredArgument( 0, getMessage( MISSING_PERMISSION_NAME ) );

            final DataRole dataRole = getDataRole();
            dataRole.removePermission( getTransaction(), permissionName );

            result = new CommandResultImpl( getMessage( PERMISSION_DELETED, permissionName ) );
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
        return 1;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        final Arguments args = getArguments();

        final UnitOfWork uow = getTransaction();
        final DataRole dataRole = getDataRole();
        final Permission[] permissions = dataRole.getPermissions( uow );
        List<String> existingPermissionNames = new ArrayList<String>(permissions.length);
        for(Permission permission : permissions) {
            existingPermissionNames.add(permission.getName(uow));
        }

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( existingPermissionNames );
            } else {
                for ( final String item : existingPermissionNames ) {
                    if ( item.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) {
                        candidates.add( item );
                    }
                }
            }

            return 0;
        }

        // no tab completion
        return -1;
    }

}
