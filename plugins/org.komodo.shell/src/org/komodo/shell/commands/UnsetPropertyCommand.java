/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.shell.commands;

import java.util.List;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.Messages;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.KomodoObjectUtils;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.utils.StringUtils;

/**
 * UnsetPropertyCommand - unsets the specified property of a KomodoObject.
 */
public class UnsetPropertyCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "unset-property"; //$NON-NLS-1$

    /**
     * @param status
     *        the workspace status (cannot be <code>null</code>)
     */
    public UnsetPropertyCommand( final WorkspaceStatus status ) {
        super( status, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        return true;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        try {
            final String propNameArg = requiredArgument( 0, Messages.getString( Messages.SHELL.InvalidArgMsg_PropertyName ) );

            if ( !validateProperty( propNameArg, getContext(), true ) ) {
                return new CommandResultImpl( false, Messages.getString( SHELL.CommandFailure, NAME ), null );
            }

            final KomodoObject context = getContext();

            // remove the property by setting its value to null
            final String propertyName = ( !isShowingPropertyNamePrefixes() ? KomodoObjectUtils.attachPrefix( getWorkspaceStatus(),context, propNameArg ) : propNameArg );
            context.setProperty( getWorkspaceStatus().getTransaction(),propertyName, (Object[])null );

            return new CommandResultImpl( getString( "propertyUnset", propNameArg ) ); //$NON-NLS-1$
        } catch ( final Exception e ) {
            return new CommandResultImpl( false, Messages.getString( SHELL.CommandFailure, NAME ), e );
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

    private String getString( final String msgKey,
                              final String... args ) {
        return Messages.getString( UnsetPropertyCommand.class.getSimpleName() + '.' + msgKey, ( Object[] )args );
    }

    /**
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        if ( getArguments().size() == 0 ) {
            updateTabCompleteCandidatesForProperty( candidates, getContext(), lastArgument );

            return ( candidates.isEmpty() ? -1 : ( StringUtils.isBlank( lastArgument ) ? 0 : ( toString().length() + 1 ) ) );
        }
        return -1;
    }

}
