/*************************************************************************************
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership. Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 ************************************************************************************/
package org.komodo.shell.commands.core;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import org.komodo.repository.KomodoTypeRegistry;
import org.komodo.repository.RepositoryImpl;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.Messages;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoType;

/**
 * Finds objects in the workspace of a specified type. Search results are the workspace paths of the found objects.
 */
public final class FindCommand extends BuiltInShellCommand {

    private static final String NAME = "find"; //$NON-NLS-1$
    private static final List< KomodoType > NOT_APPLICABLE_TYPES = Arrays.asList( new KomodoType[] { KomodoType.UNKNOWN,
                                                                                                    KomodoType.WORKSPACE } );
    private static final String REPO_WS_ROOT_PATH = ( RepositoryImpl.WORKSPACE_ROOT + StringConstants.FORWARD_SLASH );

    /**
     * @param status
     *        the workspace status (cannot be <code>null</code>)
     */
    public FindCommand( final WorkspaceStatus status ) {
        super( NAME, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#execute()
     */
    @Override
    public boolean execute() throws Exception {
        try {
            final String typeArg = requiredArgument( 0, Messages.getString( "FindCommand.MissingTypeArg" ) ); //$NON-NLS-1$
            final KomodoType queryType = getQueryType( typeArg );

            if ( queryType == null ) {
                print( MESSAGE_INDENT, Messages.getString( "FindCommand.InvalidType", typeArg ) ); //$NON-NLS-1$
                return false;
            }

            // query
            final String[] foundObjectPaths = query( queryType );

            // print results
            printResults( queryType, foundObjectPaths );
        } catch ( final Exception e ) {
            print( MESSAGE_INDENT, Messages.getString( "FindCommand.Failure" ) ); //$NON-NLS-1$
            print( MESSAGE_INDENT, "\t" + e.getMessage() ); //$NON-NLS-1$
            return false;
        }

        return true;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( int indent ) {
        super.printHelpDescription( indent );
        final int detailIndent = ( indent * 2 );

        print();
        print( detailIndent, Messages.getString( "FindCommand.helpTypesHeading" ) ); //$NON-NLS-1$

        final List< String > types = KomodoType.getTypes();

        // remove not applicable types
        for ( final KomodoType ktype : KomodoType.values() ) {
            if ( NOT_APPLICABLE_TYPES.contains( ktype ) ) {
                types.remove( ktype.getType() );
            }
        }

        Collections.sort( types );

        final StringBuilder builder = new StringBuilder();
        int i = 0;

        for ( final String type : types ) {
            if ( i % 5 == 0 ) {
                print( detailIndent, builder.toString() );
                builder.setLength( 0 );
            }

            builder.append( String.format( "%-25s", type ) ); //$NON-NLS-1$
            ++i;
        }

        if ( builder.length() != 0 ) {
            print( detailIndent, builder.toString() );
        }
    }

    private KomodoType getQueryType( final String typeArg ) {
        final KomodoType ktype = KomodoType.getKomodoType( typeArg );
        return ( NOT_APPLICABLE_TYPES.contains( ktype ) ? null : ktype );
    }

    private void printResults( final KomodoType queryType,
                               final String[] foundObjectPaths ) throws Exception {
        if ( foundObjectPaths.length == 0 ) {
            print( MESSAGE_INDENT, Messages.getString( "FindCommand.NoObjectsFound", queryType.getType() ) ); //$NON-NLS-1$
        } else {
            // print header
            final String header = Messages.getString( "FindCommand.TypeHeader", queryType.getType() ); //$NON-NLS-1$
            print( MESSAGE_INDENT, header );

            // print paths of found objects
            final int indent = ( 2 * MESSAGE_INDENT );

            for ( final String absolutePath : foundObjectPaths ) {
                String path = absolutePath;

                // should always start with the absolute repository path but check just in case
                if ( path.startsWith( REPO_WS_ROOT_PATH ) ) {
                    path = ( ContextUtils.ROOT_OPT3 + path.substring( REPO_WS_ROOT_PATH.length() ) );
                }

                print( indent, path );
            }
        }
    }

    private String[] query( final KomodoType queryType ) throws Exception {
        final String lexiconType = KomodoTypeRegistry.getInstance().getIdentifier( queryType ).getLexiconType();
        return getContext().getWorkspaceManager().findByType( getWorkspaceStatus().getTransaction(), lexiconType );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        if ( getArguments().isEmpty() ) {
            final boolean noLastArg = ( lastArgument == null );

            for ( final KomodoType kType : KomodoType.values() ) {
                if ( NOT_APPLICABLE_TYPES.contains( kType ) ) {
                    continue;
                }

                if ( ( noLastArg ) || ( kType.name().toUpperCase().startsWith( lastArgument.toUpperCase() ) ) ) {
                    candidates.add( kType.getType() );
                }
            }

            return 0;
        }

        // no completions if more than one arg
        return -1;
    }

}
