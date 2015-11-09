/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.teiid;

import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.General.INVALID_BOOLEAN_PROPERTY_VALUE;
import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.General.INVALID_INTEGER_PROPERTY_VALUE;
import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.General.INVALID_PROPERTY_NAME;
import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.General.MISSING_PROPERTY_NAME_VALUE;
import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.General.SET_PROPERTY_SUCCESS;
import java.util.List;
import org.komodo.relational.teiid.Teiid;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.SetPropertyCommand;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;

/**
 * A shell command to set Teiid properties
 */
public final class SetTeiidPropertyCommand extends TeiidShellCommand {

    static final String NAME = SetPropertyCommand.NAME;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetTeiidPropertyCommand( final WorkspaceStatus status ) {
        super( NAME, status );
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
            final String name = requiredArgument( 0, getWorkspaceMessage( MISSING_PROPERTY_NAME_VALUE ) );
            final String value = requiredArgument( 1, getWorkspaceMessage( MISSING_PROPERTY_NAME_VALUE ) );

            final Teiid teiid = getTeiid();
            final UnitOfWork transaction = getTransaction();
            String errorMsg = null;

            switch ( name ) {
                case ADMIN_PORT:
                    try {
                        final int port = Integer.parseInt( value );
                        teiid.setAdminPort( transaction, port );
                    } catch ( final NumberFormatException e ) {
                        errorMsg = getWorkspaceMessage( INVALID_INTEGER_PROPERTY_VALUE, ADMIN_PORT );
                    }

                    break;
                case ADMIN_PASSWORD:
                    teiid.setAdminPassword( transaction, value );
                    break;
                case ADMIN_SECURE:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        teiid.setAdminSecure( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = getWorkspaceMessage( INVALID_BOOLEAN_PROPERTY_VALUE, ADMIN_SECURE );
                    }

                    break;
                case ADMIN_USER:
                    teiid.setAdminUser( transaction, value );
                    break;
                case JDBC_PORT:
                    try {
                        final int port = Integer.parseInt( value );
                        teiid.setJdbcPort( transaction, port );
                    } catch ( final NumberFormatException e ) {
                        errorMsg = getWorkspaceMessage( INVALID_INTEGER_PROPERTY_VALUE, JDBC_PORT );
                    }

                    break;
                case JDBC_PASSWORD:
                    teiid.setJdbcPassword( transaction, value );
                    break;
                case JDBC_SECURE:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        teiid.setJdbcSecure( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = getWorkspaceMessage( INVALID_BOOLEAN_PROPERTY_VALUE, JDBC_SECURE );
                    }

                    break;
                case JDBC_USER:
                    teiid.setJdbcUsername( transaction, value );
                    break;
                case HOST:
                    teiid.setHost( transaction, value );
                    break;
                default:
                    errorMsg = getWorkspaceMessage( INVALID_PROPERTY_NAME, name, Teiid.class.getSimpleName() );
                    break;
            }

            if ( StringUtils.isBlank( errorMsg ) ) {
                result = new CommandResultImpl( getWorkspaceMessage( SET_PROPERTY_SUCCESS, name ) );
            } else {
                result = new CommandResultImpl( false, errorMsg, null );
            }
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
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        final Arguments args = getArguments();

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( ALL_PROPS );
            } else {
                for ( final String item : ALL_PROPS ) {
                    if ( item.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) {
                        candidates.add( item );
                    }
                }
            }

            return 0;
        }

        if ( (args.size() == 1) && ( ADMIN_SECURE.equals(getArguments().get(0)) || JDBC_SECURE.equals(getArguments().get(0)) ) ) {
            updateCandidatesForBooleanProperty( lastArgument, candidates );
            return ( candidates.isEmpty() ? -1 : ( StringUtils.isBlank( lastArgument ) ? 0 : ( toString().length() + 1 ) ) );
        }

        // no tab completion
        return -1;
    }

}
