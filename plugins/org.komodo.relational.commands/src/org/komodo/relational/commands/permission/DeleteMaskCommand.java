/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.permission;

import static org.komodo.relational.commands.permission.PermissionCommandMessages.DeleteMaskCommand.DELETE_MASK_ERROR;
import static org.komodo.relational.commands.permission.PermissionCommandMessages.DeleteMaskCommand.MASK_DELETED;
import static org.komodo.relational.commands.permission.PermissionCommandMessages.General.MISSING_MASK_NAME;
import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.Permission;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to delete a Mask from a Permission.
 */
public final class DeleteMaskCommand extends PermissionShellCommand {

    static final String NAME = "delete-mask"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public DeleteMaskCommand( final WorkspaceStatus status ) {
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
            final String maskName = requiredArgument( 0, getMessage( MISSING_MASK_NAME ) );

            final Permission permission = getPermission();
            permission.removeMask( getTransaction(), maskName );

            result = new CommandResultImpl( getMessage( MASK_DELETED, maskName ) );
        } catch ( final Exception e ) {
            result = new CommandResultImpl( false, getMessage( DELETE_MASK_ERROR ), e );
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
        final Permission permission = getPermission();
        final Condition[] conditions = permission.getConditions( uow );
        List<String> existingConditionNames = new ArrayList<String>(conditions.length);
        for(Condition condition : conditions) {
            existingConditionNames.add(condition.getName(uow));
        }

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( existingConditionNames );
            } else {
                for ( final String item : existingConditionNames ) {
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
