/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.shell.commands;

import static org.komodo.shell.Messages.SetPrimaryTypeCommand.PRIMARY_TYPE_SET;
import static org.komodo.shell.Messages.SetPrimaryTypeCommand.MISSING_TYPE_NAME;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.Messages;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.KomodoObjectUtils;
import org.komodo.spi.repository.KomodoObject;

/**
 * Sets the primary type of a {@link KomodoObject}.
 * <p>
 * Usage:
 * <p>
 * <code>&nbsp;&nbsp;
 * set-primary-type &lt;type&gt; {child-type}
 * </code>
 */
public class SetPrimaryTypeCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "set-primary-type"; //$NON-NLS-1$

    /**
     * @param status
     *        the workspace status (cannot be <code>null</code>)
     */
    public SetPrimaryTypeCommand( final WorkspaceStatus status ) {
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
            final String typeName = requiredArgument( 0, Messages.getString( MISSING_TYPE_NAME ) );

            final KomodoObject kobject = getContext();
            kobject.setPrimaryType( getTransaction(), typeName );
            return new CommandResultImpl( Messages.getString( PRIMARY_TYPE_SET, typeName ) );
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
