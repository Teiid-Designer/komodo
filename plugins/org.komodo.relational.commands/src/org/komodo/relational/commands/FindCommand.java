/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands;

import static org.komodo.relational.commands.RelationalCommandMessages.RESOURCE_BUNDLE;
import static org.komodo.relational.commands.RelationalCommandMessages.FindCommand.INVALID_TYPE;
import static org.komodo.relational.commands.RelationalCommandMessages.FindCommand.MISSING_TYPE_NAME;
import static org.komodo.relational.commands.RelationalCommandMessages.FindCommand.NO_OBJECTS_FOUND;
import static org.komodo.relational.commands.RelationalCommandMessages.FindCommand.TYPE_HEADER;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import org.komodo.relational.Messages;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.repository.KomodoTypeRegistry;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.KomodoObjectLabelProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoType;
import org.komodo.utils.StringUtils;

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
            final String typeName = requiredArgument( 0, getMessage( MISSING_TYPE_NAME ) );
            final KomodoType queryType = getQueryType( typeName );

            if ( queryType == null ) {
                result = new CommandResultImpl( false, getMessage( INVALID_TYPE, typeName ), null );
            } else {
                // may have a name pattern
                final String pattern = optionalArgument( 1 );

                // query
                final String[] foundObjectPaths = query( getWorkspaceStatus(), queryType, null, pattern );

                // print results
                printResults( queryType, foundObjectPaths );
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
        return 1;
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
                               final String[] foundObjectPaths ) throws Exception {
        if ( foundObjectPaths.length == 0 ) {
            print( MESSAGE_INDENT, getMessage(NO_OBJECTS_FOUND, queryType.getType() ) );
        } else {
            // print header
            final String header = getMessage(TYPE_HEADER, queryType.getType() );
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
    public static String[] query( final WorkspaceStatus wsStatus,
                                  final KomodoType queryType,
                                  final String parentPath,
                                  final String pattern ) throws Exception {
        final String lexiconType = KomodoTypeRegistry.getInstance().getIdentifier( queryType ).getLexiconType();
        final WorkspaceManager wsMgr = WorkspaceManager.getInstance( wsStatus.getCurrentContext().getRepository() );
        final String[] searchResults = wsMgr.findByType( wsStatus.getTransaction(), lexiconType, parentPath, pattern );

        if ( searchResults.length == 0 ) {
            return searchResults;
        }

        final KomodoObjectLabelProvider labelProvider = wsStatus.getLabelProvider();
        final String[] result = new String[ searchResults.length ];
        int i = 0;
        for ( final String absolutePath : searchResults ) {
            result[i] = labelProvider.getDisplayPath( absolutePath );
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

    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, Messages.getString( RESOURCE_BUNDLE, getClass().getSimpleName() + ".help", getName() ) ); //$NON-NLS-1$
    }

    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, Messages.getString( RESOURCE_BUNDLE, getClass().getSimpleName() + ".examples" ) ); //$NON-NLS-1$
    }

    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, Messages.getString( RESOURCE_BUNDLE, getClass().getSimpleName() + ".usage" ) ); //$NON-NLS-1$
    }


}
