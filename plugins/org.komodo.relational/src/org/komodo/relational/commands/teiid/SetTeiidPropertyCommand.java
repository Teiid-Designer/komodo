/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.teiid;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_BOOLEAN_PROPERTY_VALUE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_INTEGER_PROPERTY_VALUE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_PROPERTY_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.MISSING_PROPERTY_NAME_VALUE;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.Arrays;
import java.util.List;
import org.komodo.relational.teiid.Teiid;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.core.SetPropertyCommand;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to set Teiid properties
 */
public final class SetTeiidPropertyCommand extends TeiidShellCommand {

    static final String NAME = "set-teiid-property"; //$NON-NLS-1$

    private static final String ADMIN_PORT = "adminPort"; //$NON-NLS-1$
    private static final String ADMIN_PASSWORD = "adminPswd"; //$NON-NLS-1$
    private static final String ADMIN_SECURE = "adminSecure"; //$NON-NLS-1$
    private static final String ADMIN_USER = "adminUser"; //$NON-NLS-1$
    private static final String JDBC_PORT = "jdbcPort"; //$NON-NLS-1$
    private static final String JDBC_PASSWORD = "jdbcPswd"; //$NON-NLS-1$
    private static final String JDBC_SECURE = "jdbcSecure"; //$NON-NLS-1$
    private static final String JDBC_USER = "jdbcUser"; //$NON-NLS-1$

    private static final List< String > ALL_PROPS = Arrays.asList( new String[] { ADMIN_PORT, ADMIN_PASSWORD, ADMIN_SECURE, ADMIN_USER, 
                                                                                  JDBC_PORT, JDBC_PASSWORD, JDBC_SECURE, JDBC_USER } );

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetTeiidPropertyCommand( final WorkspaceStatus status ) {
        super( NAME, true, status );
        // Overrides the BuiltInCommand "set-property"
        setOverriddenCommands(new String[]{SetPropertyCommand.NAME});
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String name = requiredArgument( 0, getWorkspaceMessage(MISSING_PROPERTY_NAME_VALUE) );
        final String value = requiredArgument( 1, getWorkspaceMessage(MISSING_PROPERTY_NAME_VALUE) );

        final Teiid teiid = getTeiid();
        final UnitOfWork transaction = getTransaction();
        boolean success = true;

        switch ( name ) {
            case ADMIN_PORT:
                try {
                    final int port = Integer.parseInt( value );
                    teiid.setAdminPort( transaction, port );
                } catch ( final NumberFormatException e ) {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_INTEGER_PROPERTY_VALUE, ADMIN_PORT ) );
                    success = false;
                }

                break;
            case ADMIN_PASSWORD:
                teiid.setAdminPassword( transaction, value );
                break;
            case ADMIN_SECURE:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    teiid.setAdminSecure( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, ADMIN_SECURE ) );
                    success = false;
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
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_INTEGER_PROPERTY_VALUE, JDBC_PORT ) );
                    success = false;
                }

                break;
            case JDBC_PASSWORD:
                teiid.setJdbcPassword( transaction, value );
                break;
            case JDBC_SECURE:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    teiid.setJdbcSecure( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, JDBC_SECURE ) );
                    success = false;
                }

                break;
            case JDBC_USER:
                teiid.setJdbcUsername( transaction, value );
                break;
            default:
                success = false;
                print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_PROPERTY_NAME, NAME ) );
                break;
        }

        return success;
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
            candidates.add( Boolean.TRUE.toString() );
            candidates.add( Boolean.FALSE.toString() );

            return 0;
        }

        // no tab completion
        return -1;
    }

}
