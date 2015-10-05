/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.shell.commands;

import java.util.List;
import java.util.Set;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.Messages;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.utils.StringUtils;

/**
 * A command to set the value of a global property
 */
public class SetGlobalPropertyCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "set-global"; //$NON-NLS-1$

    /**
     * @param status
     *        the workspace status (cannot be <code>null</code>)
     */
    public SetGlobalPropertyCommand( final WorkspaceStatus status ) {
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
            // property name and value are required
            String propNameArg = requiredArgument(0, Messages.getString(Messages.SetGlobalPropertyCommand.InvalidArgMsg_GlobalPropertyName));
            String propValueArg = requiredArgument(1, Messages.getString(Messages.SetGlobalPropertyCommand.InvalidArgMsg_PropertyValue ) );

            // validate global property name and value
            final String errorMsg = getWorkspaceStatus().validateGlobalPropertyValue( propNameArg, propValueArg );

            if ( !StringUtils.isEmpty( errorMsg ) ) {
                return new CommandResultImpl( false,
                                              Messages.getString( Messages.SetGlobalPropertyCommand.InvalidGlobalProperty,
                                                                  errorMsg ),
                                              null );
            }

            // Set the property
            setGlobalProperty( propNameArg, propValueArg );
            return new CommandResultImpl( Messages.getString( Messages.SetGlobalPropertyCommand.GlobalPropertySet, propNameArg ) );
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
        return 2;
    }

    /**
     * Sets a global workspace property
     * @param propName the global property name
     * @param propValue the property value
     * @throws Exception the exception
     */
    private void setGlobalProperty(String propName, String propValue) throws Exception {
        WorkspaceStatus wsStatus = getWorkspaceStatus();
        wsStatus.setProperty(propName, propValue);
    }

    /**
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        if ( getArguments().size() == 0 ) {
            // Global property completion options
            final Set< String > potentials = WorkspaceStatus.GLOBAL_PROPS.keySet();

            if ( lastArgument == null ) {
                candidates.addAll( potentials );
            } else {
                for ( final String name : potentials ) {
                    if ( name.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) {
                        candidates.add( name );
                    }
                }
            }
        } else if ( getArguments().size() == 1 ) {
            if ( getWorkspaceStatus().isBooleanProperty( getArguments().get( 0 ) ) ) {
                updateCandidatesForBooleanProperty( lastArgument, candidates );
            }
        }

        return ( candidates.isEmpty() ? -1 : ( StringUtils.isBlank( lastArgument ) ? 0 : ( toString().length() + 1 ) ) );
    }

}
