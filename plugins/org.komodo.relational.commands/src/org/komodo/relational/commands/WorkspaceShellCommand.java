/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands;

import static org.komodo.relational.commands.WorkspaceCommandMessages.RESOURCE_BUNDLE;
import org.komodo.relational.Messages;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoType;

/**
 * A base class for @{link {@link WorkspaceManager workspace manager}-related shell commands.
 */
abstract class WorkspaceShellCommand extends RelationalShellCommand {

    protected WorkspaceShellCommand( final WorkspaceStatus status,
                                     final String name ) {
        super( status, name );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.RelationalShellCommand#getMessage(java.lang.Enum, java.lang.Object[])
     */
    @Override
    protected String getMessage( final Enum< ? > key,
                                 final Object... parameters ) {
        return Messages.getString( RESOURCE_BUNDLE, key.toString(), parameters );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            final KomodoType contextType = getContext().getTypeIdentifier( getTransaction() );
            return ( contextType == KomodoType.WORKSPACE );
        } catch ( final Exception e ) {
            return false;
        }
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
