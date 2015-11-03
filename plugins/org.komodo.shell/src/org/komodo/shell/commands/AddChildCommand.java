/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.shell.commands;

import static org.komodo.shell.Messages.AddChildCommand.CHILD_ADDED;
import static org.komodo.shell.Messages.AddChildCommand.MISSING_CHILD_NAME;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.Messages;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.KomodoObjectUtils;
import org.komodo.spi.repository.KomodoObject;

/**
 * Adds a child to a {@link KomodoObject}.
 * <p>
 * Usage:
 * <p>
 * <code>&nbsp;&nbsp;
 * add-child &lt;new-child-name&gt; {child-type}
 * </code>
 */
public class AddChildCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "add-child"; //$NON-NLS-1$

    /**
     * @param status
     *        the workspace status (cannot be <code>null</code>)
     */
    public AddChildCommand( final WorkspaceStatus status ) {
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
            final String childNameArg = requiredArgument( 0, Messages.getString( MISSING_CHILD_NAME ) );
            final String childTypeArg = optionalArgument( 1 );

            final KomodoObject kobject = getContext();
            kobject.addChild( getTransaction(), childNameArg, childTypeArg );
            return new CommandResultImpl( Messages.getString( CHILD_ADDED, childNameArg ) );
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
        return 2;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        return !KomodoObjectUtils.isRoot( getContext() );
    }

}
