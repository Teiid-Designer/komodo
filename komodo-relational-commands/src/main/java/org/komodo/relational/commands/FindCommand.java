/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.relational.commands;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;

import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.komodo.relational.RelationalObject;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.repository.KomodoTypeRegistry;
import org.komodo.repository.ObjectImpl;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.ui.KomodoObjectLabelProvider;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to find a relational object
 */
public final class FindCommand extends RelationalShellCommand {

    static final String NAME = "find"; //$NON-NLS-1$

    private static final List< KomodoType > NOT_APPLICABLE_TYPES = Arrays.asList( new KomodoType[] { KomodoType.UNKNOWN,
        KomodoType.WORKSPACE } );

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public FindCommand( final WorkspaceStatus status ) {
        super( status, NAME );
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
            final String typeName = requiredArgument( 0, I18n.bind( RelationalCommandsI18n.missingTypeName ) );
            final KomodoType queryType = getQueryType( typeName );

            if ( queryType == null ) {
                result = new CommandResultImpl( false, I18n.bind( RelationalCommandsI18n.invalidType, typeName ), null );
            } else {
                // may have a name pattern
                final String pattern = optionalArgument( 1 );

                // query
                final String[] foundObjectPaths = query( getWorkspaceStatus(), queryType, null, pattern );

                // print results
                printResults( queryType, pattern, foundObjectPaths );
                result = CommandResult.SUCCESS;
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
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        return ( getContext() instanceof RelationalObject );
    }

    private KomodoType getQueryType( final String typeArg ) {
        final KomodoType ktype = KomodoType.getKomodoType( typeArg );
        return ( NOT_APPLICABLE_TYPES.contains( ktype ) ? null : ktype );
    }

    private void printResults( final KomodoType queryType,
                               final String pattern,
                               final String[] foundObjectPaths ) throws Exception {
        if ( foundObjectPaths.length == 0 ) {
            if(StringUtils.isBlank(pattern)) {
                print( MESSAGE_INDENT, I18n.bind(RelationalCommandsI18n.noObjectsFound, queryType.getType() ) );
            } else {
                print( MESSAGE_INDENT, I18n.bind(RelationalCommandsI18n.noObjectsFoundForPattern, queryType.getType(), pattern ) );
            }
        } else {
            // print header
            if(StringUtils.isBlank(pattern)) {
                print( MESSAGE_INDENT, I18n.bind(RelationalCommandsI18n.typeHeader, queryType.getType() ) );
            } else {
                print( MESSAGE_INDENT, I18n.bind(RelationalCommandsI18n.typeHeaderForPattern, queryType.getType(), pattern ) );
            }

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
    public static String[] query( final WorkspaceStatus wsStatus,
                                  final KomodoType queryType,
                                  final String parentPath,
                                  final String pattern ) throws Exception {
        final String lexiconType = KomodoTypeRegistry.getInstance().getIdentifier( queryType ).getLexiconType();
        final UnitOfWork transaction = wsStatus.getTransaction();
        final WorkspaceManager wsMgr = WorkspaceManager.getInstance( wsStatus.getCurrentContext().getRepository(), transaction );
        final String[] searchResults = wsMgr.findByType( transaction, lexiconType, parentPath, pattern, false );

        if ( searchResults.length == 0 ) {
            return searchResults;
        }

        KomodoObject unknownObject=new ObjectImpl(wsStatus.getCurrentContext().getRepository(), searchResults[0], 0);
        KomodoObject resolvedObject=wsStatus.resolve(unknownObject);
        KomodoObjectLabelProvider labelProvider;
        if(resolvedObject == null){
        	labelProvider = wsStatus.getCurrentContextLabelProvider();
        }else{
            labelProvider = wsStatus.getObjectLabelProvider(resolvedObject);
        }
        final String[] result = new String[ searchResults.length ];
        int i = 0;
        for ( final String absolutePath : searchResults ) {
            result[i] = labelProvider.getDisplayPath( transaction, absolutePath, null );
            ++i;
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public TabCompletionModifier tabCompletion( final String lastArgument,
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
        }

        // no completions if more than one arg
        return TabCompletionModifier.AUTO;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( RelationalCommandsI18n.findHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( RelationalCommandsI18n.findExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( RelationalCommandsI18n.findUsage ) );
    }


}
