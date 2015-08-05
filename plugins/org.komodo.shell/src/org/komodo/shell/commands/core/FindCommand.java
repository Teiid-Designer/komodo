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
import java.util.Comparator;
import java.util.List;
import org.komodo.repository.KomodoTypeRegistry;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.Messages;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.repository.KomodoType;
import org.komodo.utils.StringUtils;

/**
 * Finds objects in the workspace of a specified type. Search results are the workspace paths of the found objects.
 */
public final class FindCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "find"; //$NON-NLS-1$

    private static final List< KomodoType > NOT_APPLICABLE_TYPES = Arrays.asList( new KomodoType[] { KomodoType.UNKNOWN,
                                                                                                    KomodoType.WORKSPACE } );

    /**
     * @param status
     *        the workspace status (cannot be <code>null</code>)
     */
    public FindCommand( final WorkspaceStatus status ) {
        super( status, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#execute()
     */
    @Override
    public boolean execute() throws Exception {
        final String typeArg = requiredArgument( 0, Messages.getString( "FindCommand.MissingTypeArg" ) ); //$NON-NLS-1$
        final KomodoType queryType = getQueryType( typeArg );

        if ( queryType == null ) {
            print( MESSAGE_INDENT, Messages.getString( "FindCommand.InvalidType", typeArg ) ); //$NON-NLS-1$
            return false;
        }

        // may have a name pattern
        final String pattern = optionalArgument( 1 );

        try {
            // query
            final String[] foundObjectPaths = query( getWorkspaceStatus(), queryType, null, pattern );

            // print results
            printResults( queryType, foundObjectPaths );
        } catch ( final Exception e ) {
            print( MESSAGE_INDENT, Messages.getString( "FindCommand.Failure", e.getLocalizedMessage() ) ); //$NON-NLS-1$
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

            for ( final String path : foundObjectPaths ) {
                print( indent, path );
            }
        }
    }

    /**
     * Query to find the display paths of the specified object type
     * @param wsStatus
     *        the workspace status
     * @param queryType
     *        the type of object being searched for (cannot be <code>null</code>)
     * @param parentPath
     *        the parent path whose children recursively will be checked (can be empty if searching from the workspace root)
     * @param pattern
     *        the regex used to match object names (can be empty if all objects of the given type are being requested)
     * @return the display paths of the workspace objects with the matching type (never <code>null</code> but can be empty)
     * @throws Exception
     *         if an error occurs
     */
    protected static String[] query( final WorkspaceStatus wsStatus,
    		                         final KomodoType queryType,
    		                         final String parentPath,
    		                         final String pattern ) throws Exception {
        final String lexiconType = KomodoTypeRegistry.getInstance().getIdentifier( queryType ).getLexiconType();
        final String[] searchResults = wsStatus.getCurrentContext().getWorkspaceManager().findByType( wsStatus.getTransaction(),
                                                                                      lexiconType, parentPath, pattern );

        if ( searchResults.length == 0 ) {
            return searchResults;
        }

        final String[] result = new String[ searchResults.length ];
        int i = 0;
        for ( final String absolutePath : searchResults ) {
            result[i] = ContextUtils.convertPathToDisplayPath(absolutePath);
            ++i;
        }

        return result;
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
            final boolean noLastArg = StringUtils.isBlank( lastArgument );

            for ( final KomodoType kType : KomodoType.values() ) {
                if ( NOT_APPLICABLE_TYPES.contains( kType ) ) {
                    continue;
                }

                if ( noLastArg || ( kType.getType().toUpperCase().startsWith( lastArgument.toUpperCase() ) ) ) {
                    candidates.add( kType.getType() );
                }
            }

            Collections.sort( candidates, new Comparator< CharSequence >() {

                /**
                 * {@inheritDoc}
                 *
                 * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
                 */
                @Override
                public int compare( final CharSequence thisType,
                                    final CharSequence thatType ) {
                    return thisType.toString().compareTo( thatType.toString() );
                }
            });

            return ( candidates.isEmpty() ? -1 : ( toString().length() + 1 ) );
        }

        // no completions if more than one arg
        return -1;
    }

}
