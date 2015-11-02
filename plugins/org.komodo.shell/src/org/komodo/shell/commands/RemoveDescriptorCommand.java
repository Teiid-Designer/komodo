/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.shell.commands;

import static org.komodo.shell.Messages.RemoveDescriptorCommand.DESCRIPTOR_REMOVED;
import static org.komodo.shell.Messages.RemoveDescriptorCommand.MISSING_DESCRIPTOR_NAME;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.Messages;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.KomodoObjectUtils;
import org.komodo.spi.repository.KomodoObject;

/**
 * Removes a descriptor from a {@link KomodoObject}.
 * <p>
 * Usage:
 * <p>
 * <code>&nbsp;&nbsp;
 * remove-descriptor &lt;descriptor-name&gt; {child-type}
 * </code>
 */
public class RemoveDescriptorCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "remove-descriptor"; //$NON-NLS-1$

    /**
     * @param status
     *        the workspace status (cannot be <code>null</code>)
     */
    public RemoveDescriptorCommand( final WorkspaceStatus status ) {
        super( status, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        try {
            final String descriptorNameArg = requiredArgument( 0, Messages.getString( MISSING_DESCRIPTOR_NAME ) );

            final KomodoObject kobject = getContext();
            kobject.removeDescriptor(getTransaction(), descriptorNameArg);
            return new CommandResultImpl( Messages.getString( DESCRIPTOR_REMOVED, descriptorNameArg ) );
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
        return 1;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        return ( !KomodoObjectUtils.isRoot(getContext()) && !KomodoObjectUtils.isRootChild(getContext()) );
    }

}
