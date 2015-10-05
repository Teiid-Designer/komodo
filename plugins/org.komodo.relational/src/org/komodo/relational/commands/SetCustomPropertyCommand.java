/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands;

import static org.komodo.relational.commands.WorkspaceCommandMessages.RESOURCE_BUNDLE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.MISSING_PROPERTY_NAME_VALUE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.SET_PROPERTY_SUCCESS;
import org.komodo.relational.Messages;
import org.komodo.relational.RelationalObject;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to set a custom property on a {@link RelationalObject}.
 */
public final class SetCustomPropertyCommand extends RelationalShellCommand {

    static final String NAME = "set-custom-property"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetCustomPropertyCommand( final WorkspaceStatus status ) {
        super( status, NAME );
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
            final String name = requiredArgument( 0, getMessage( MISSING_PROPERTY_NAME_VALUE ) );
            final String value = requiredArgument( 1, getMessage( MISSING_PROPERTY_NAME_VALUE ) );

            final RelationalObject robject = get();
            robject.setProperty( getTransaction(), name, value );

            result = new CommandResultImpl( getMessage( SET_PROPERTY_SUCCESS, name ) );
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
        return 2;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        return ( getContext() instanceof RelationalObject );
    }

    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, Messages.getString( RESOURCE_BUNDLE, getClass().getSimpleName() + ".help", getName() ) ); //$NON-NLS-1$
    }

    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, Messages.getString( RESOURCE_BUNDLE, getClass().getSimpleName() + ".examples" ) ); //$NON-NLS-1$
    }

    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, Messages.getString( RESOURCE_BUNDLE, getClass().getSimpleName() + ".usage" ) ); //$NON-NLS-1$
    }


}
