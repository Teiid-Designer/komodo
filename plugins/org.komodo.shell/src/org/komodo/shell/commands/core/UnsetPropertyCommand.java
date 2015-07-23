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
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.Messages;
import org.komodo.shell.api.InvalidCommandArgumentException;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.utils.StringUtils;

/**
 * A command to unset/remove a property value.
 */
public class UnsetPropertyCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "unset"; //$NON-NLS-1$

    /**
     * @param status
     *        the workspace status (cannot be <code>null</code>)
     */
    public UnsetPropertyCommand( final WorkspaceStatus status ) {
        super( status, NAME );
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#execute()
     */
    @Override
    public boolean execute() throws Exception {
        try {
            final String propNameArg = requiredArgument( 0, getString( "missingPropertyName" ) ); //$NON-NLS-1$

            if ( !validateProperty( propNameArg, getContext(), true ) ) {
                return false;
            }

            final WorkspaceContext context = getContext();

            // remove the property by setting its value to null
            final String propertyName = ( !isShowingPropertyNamePrefixes() ? attachPrefix( context, propNameArg ) : propNameArg );
            context.setPropertyValue( propertyName, null );

            // Commit transaction
            if ( isAutoCommit() ) {
                getWorkspaceStatus().commit( UnsetPropertyCommand.class.getSimpleName() );
            }

            // Print message
            print( MESSAGE_INDENT, getString( "propertyUnset", propNameArg ) ); //$NON-NLS-1$

            return true;
        } catch ( final InvalidCommandArgumentException e ) {
            throw e;
        } catch ( final Exception e ) {
            print( MESSAGE_INDENT, getString( "error", e.getLocalizedMessage() ) ); //$NON-NLS-1$
            return false;
        }
    }

    private String getString( final String msgKey,
                              final String... args ) {
        return Messages.getString( UnsetPropertyCommand.class.getSimpleName() + '.' + msgKey, ( Object[] )args );
    }

    /**
     * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        if ( getArguments().size() == 0 ) {
            final WorkspaceContext context = getContext();
            final List< String > names = context.getProperties();
            final boolean noLastArg = StringUtils.isBlank( lastArgument );

            // make sure property has a value and remove property prefixes if necessary
            for ( String propName : names ) {
                if ( !StringUtils.isBlank( context.getPropertyValue( propName ) ) ) {
                    boolean add = noLastArg;

                    if ( !isShowingPropertyNamePrefixes() ) {
                        propName = removePrefix( propName );
                    }

                    if ( !noLastArg && propName.startsWith( lastArgument ) ) {
                        add = true;
                    }

                    if ( add ) {
                        candidates.add( propName );
                    }
                }
            }

            return ( candidates.isEmpty() ? -1 : ( StringUtils.isBlank( lastArgument ) ? 0 : ( toString().length() + 1 ) ) );
        }

        return -1;
    }

}
