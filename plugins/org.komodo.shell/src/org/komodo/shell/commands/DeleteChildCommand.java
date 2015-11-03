/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.shell.commands;

import static org.komodo.shell.Messages.DeleteChildCommand.CHILD_DELETED;
import static org.komodo.shell.Messages.DeleteChildCommand.MISSING_CHILD_NAME;
import static org.komodo.shell.Messages.DeleteChildCommand.NO_CHILD_WITH_NAME_AND_TYPE;
import static org.komodo.shell.Messages.DeleteChildCommand.NO_CHILD_WITH_NAME;
import static org.komodo.shell.Messages.DeleteChildCommand.ERROR_GETTING_CHILD;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.Messages;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.KomodoObjectUtils;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.utils.StringUtils;

/**
 * Deletes a child from a {@link KomodoObject}.
 * <p>
 * Usage:
 * <p>
 * <code>&nbsp;&nbsp;
 * delete-child &lt;child-name&gt; {child-type}
 * </code>
 */
public class DeleteChildCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "delete-child"; //$NON-NLS-1$

    /**
     * @param status
     *        the workspace status (cannot be <code>null</code>)
     */
    public DeleteChildCommand( final WorkspaceStatus status ) {
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
            KomodoObject childObject = null;
            // Determine if child exists before attempting delete.
            if(!StringUtils.isBlank(childTypeArg)) {
                if(!kobject.hasChild(getTransaction(), childNameArg, childTypeArg)) {
                    return new CommandResultImpl( false, Messages.getString(NO_CHILD_WITH_NAME_AND_TYPE, childNameArg, childTypeArg), null);
                }
                childObject = kobject.getChild(getTransaction(), childNameArg, childTypeArg);
            } else {
                if(!kobject.hasChild(getTransaction(), childNameArg)) return new CommandResultImpl( false, Messages.getString(NO_CHILD_WITH_NAME, childNameArg), null);
                childObject = kobject.getChild(getTransaction(), childNameArg);
            }
            
            if(childObject==null) return new CommandResultImpl(false, Messages.getString(ERROR_GETTING_CHILD, childNameArg), null);

            childObject.remove(getTransaction());
            return new CommandResultImpl( Messages.getString( CHILD_DELETED, childNameArg ) );
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
