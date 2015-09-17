/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.datarole;

import static org.komodo.relational.commands.datarole.DataRoleCommandMessages.DeleteMappedRoleCommand.MAPPED_ROLE_DELETED;
import static org.komodo.relational.commands.datarole.DataRoleCommandMessages.General.MISSING_MAPPED_ROLE_NAME;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.vdb.DataRole;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to delete a MappedRole from a DataRole.
 */
public final class DeleteMappedRoleCommand extends DataRoleShellCommand {

    static final String NAME = "delete-mapped-role"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public DeleteMappedRoleCommand( final WorkspaceStatus status ) {
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
        dataRole.removeMappedRole( getTransaction(), mappedRoleName );

        // Print success message
        print(MESSAGE_INDENT, getMessage(MAPPED_ROLE_DELETED,mappedRoleName));
        
        return true;
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
        final String[] mappedRoles = dataRole.getMappedRoles( uow );
        List<String> existingRoleNames = new ArrayList<String>(mappedRoles.length);
        for(String mappedRole : mappedRoles) {
            existingRoleNames.add(mappedRole);
        }
        
        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( existingRoleNames );
            } else {
                for ( final String item : existingRoleNames ) {
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
