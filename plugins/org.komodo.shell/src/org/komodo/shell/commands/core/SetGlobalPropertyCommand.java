/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.shell.commands.core;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.List;
import java.util.Set;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.Messages;
import org.komodo.shell.api.InvalidCommandArgumentException;
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
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        try {
            // property name and value are required
            String propNameArg = requiredArgument(0, Messages.getString(Messages.SetGlobalPropertyCommand.InvalidArgMsg_GlobalPropertyName));
            String propValueArg = requiredArgument(1, Messages.getString(Messages.SetGlobalPropertyCommand.InvalidArgMsg_PropertyValue ) );

            // validate global property name and value
            final String errorMsg = getWorkspaceStatus().validateGlobalPropertyValue( propNameArg, propValueArg );

            if ( !StringUtils.isEmpty( errorMsg ) ) {
                print( MESSAGE_INDENT, Messages.getString( Messages.SetGlobalPropertyCommand.InvalidGlobalProperty, errorMsg ) );
                return false;
            }

            // Set the property
            setGlobalProperty( propNameArg, propValueArg );
            print( MESSAGE_INDENT, Messages.getString( Messages.SetGlobalPropertyCommand.GlobalPropertySet, propNameArg ) );

            return true;
        } catch ( final InvalidCommandArgumentException e ) {
            throw e;
        } catch ( final Exception e ) {
            print( MESSAGE_INDENT, getString( "error", e.getLocalizedMessage() ) ); //$NON-NLS-1$
            return false;
        }
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
    
    private String getString( final String msgKey,
                              final String... args ) {
        return Messages.getString( SetGlobalPropertyCommand.class.getSimpleName() + '.' + msgKey, ( Object[] )args );
    }

    /**
     * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
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

            return ( candidates.isEmpty() ? -1 : ( StringUtils.isBlank( lastArgument ) ? 0 : ( toString().length() + 1 ) ) );
        }

        return -1;
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

    /* (non-Javadoc)
     * @see org.komodo.shell.BuiltInShellCommand#shouldCommit()
     */
    @Override
    protected boolean shouldCommit() {
        return true;
    }

}
