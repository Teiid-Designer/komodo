/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_BOOLEAN_PROPERTY_VALUE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_INTEGER_PROPERTY_VALUE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_PROPERTY_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.MISSING_PROPERTY_NAME_VALUE;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.Arrays;
import java.util.List;
import org.komodo.relational.vdb.Vdb;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.core.SetPropertyCommand;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to set a VDB version.
 */
public final class SetVdbPropertyCommand extends VdbShellCommand {

    static final String NAME = "set-vdb-property"; //$NON-NLS-1$

    private static final String ALLOWED_LANGUAGES = "allowed-languages"; //$NON-NLS-1$
    private static final String AUTHENTICATION_TYPE = "authentication-type"; //$NON-NLS-1$
    private static final String CONNECTION_TYPE = "connection-type"; //$NON-NLS-1$
    private static final String DESCRIPTION = "description"; //$NON-NLS-1$
    private static final String GSS_PATTERN = "gss-pattern"; //$NON-NLS-1$
    private static final String ORIGINAL_FILE_PATH = "original-file-path"; //$NON-NLS-1$
    private static final String PASSWORD_PATTERN = "password-pattern"; //$NON-NLS-1$
    private static final String PREVIEW = "preview"; //$NON-NLS-1$
    private static final String QUERY_TIMEOUT = "query-timeout"; //$NON-NLS-1$
    private static final String SECURITY_DOMAIN = "security-domain"; //$NON-NLS-1$
    private static final String VERSION = "version"; //$NON-NLS-1$

    private static final List< String > ALL_PROPS = Arrays.asList( new String[] { ALLOWED_LANGUAGES, AUTHENTICATION_TYPE,
                                                                                 CONNECTION_TYPE, DESCRIPTION, GSS_PATTERN,
                                                                                 ORIGINAL_FILE_PATH, PASSWORD_PATTERN, PREVIEW,
                                                                                 QUERY_TIMEOUT, SECURITY_DOMAIN, VERSION } );

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetVdbPropertyCommand( final WorkspaceStatus status ) {
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
        final String value = requiredArgument( 0, getWorkspaceMessage(MISSING_PROPERTY_NAME_VALUE) );

        final Vdb vdb = getVdb();
        final UnitOfWork transaction = getTransaction();
        boolean success = true;

        switch ( name ) {
            case ALLOWED_LANGUAGES:
                vdb.setAllowedLanguages( transaction, value );
                break;
            case AUTHENTICATION_TYPE:
                vdb.setAuthenticationType( transaction, value );
                break;
            case CONNECTION_TYPE:
                vdb.setConnectionType( transaction, value );
                break;
            case DESCRIPTION:
                vdb.setDescription( transaction, value );
                break;
            case GSS_PATTERN:
                vdb.setGssPattern( transaction, value );
                break;
            case ORIGINAL_FILE_PATH:
                vdb.setOriginalFilePath( transaction, value );
                break;
            case PASSWORD_PATTERN:
                vdb.setPasswordPattern( transaction, value );
                break;
            case PREVIEW:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    vdb.setPreview( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, PREVIEW ) );
                    success = false;
                }

                break;
            case QUERY_TIMEOUT:
                try {
                    final int timeout = Integer.parseInt( value );
                    vdb.setQueryTimeout( transaction, timeout );
                } catch ( final NumberFormatException e ) {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_INTEGER_PROPERTY_VALUE, QUERY_TIMEOUT ) );
                    success = false;
                }

                break;
            case SECURITY_DOMAIN:
                vdb.setSecurityDomain( transaction, value );
                break;
            case VERSION:
                try {
                    final int version = Integer.parseInt( value );
                    vdb.setVersion( transaction, version );
                } catch ( final NumberFormatException e ) {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_INTEGER_PROPERTY_VALUE, VERSION ) );
                    success = false;
                }

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

        if ( ( args.size() == 1 ) && PREVIEW.equals( getArguments().get( 0 ) ) ) {
            candidates.add( Boolean.TRUE.toString() );
            candidates.add( Boolean.FALSE.toString() );

            return 0;
        }

        // no tab completion
        return -1;
    }

}
