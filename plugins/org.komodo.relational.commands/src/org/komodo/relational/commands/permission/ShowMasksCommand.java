/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.permission;

import static org.komodo.relational.commands.permission.PermissionCommandMessages.ShowMasksCommand.MASKS_HEADER;
import static org.komodo.relational.commands.permission.PermissionCommandMessages.ShowMasksCommand.NO_MASKS;
import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.General.PRINT_RELATIONAL_OBJECT;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.vdb.Mask;
import org.komodo.relational.vdb.Permission;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to show all the {@link Mask masks} of a {@link Permission}.
 */
public final class ShowMasksCommand extends PermissionShellCommand {

    static final String NAME = "show-masks"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ShowMasksCommand( final WorkspaceStatus status ) {
        super( NAME, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        try {
            final Permission permission = getPermission();
            final Mask[] masks = permission.getMasks( getTransaction() );

            if ( masks.length == 0 ) {
                print( MESSAGE_INDENT, getMessage( NO_MASKS, permission.getName( getTransaction() ) ) );
            } else {
                print( MESSAGE_INDENT, getMessage( MASKS_HEADER, permission.getName( getTransaction() ) ) );

                final int indent = (MESSAGE_INDENT * 2);

                for ( final Mask mask : masks ) {
                    print( indent,
                           getWorkspaceMessage( PRINT_RELATIONAL_OBJECT,
                                                mask.getName( getTransaction() ),
                                                mask.getTypeDisplayName() ) );
                }
            }

            return CommandResult.SUCCESS;
        } catch ( final Exception e ) {
            return new CommandResultImpl( e );
        }
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
