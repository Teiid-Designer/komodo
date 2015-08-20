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
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.InvalidCommandArgumentException;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.constants.StringConstants;
import org.komodo.utils.KLog;

/**
 * Show Command.  Has various acceptable args.
 * show < properties | children | status | global | property | summary >
 *
 */
public class ShowChildrenCommand extends BuiltInShellCommand implements StringConstants {

    /**
     * The command name.
     */
    public static final String NAME = "show-children"; //$NON-NLS-1$

    private static final int DEFAULT_WIDTH = 25;

    /**
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     */
    public ShowChildrenCommand( final WorkspaceStatus wsStatus ) {
        super( wsStatus, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        // Validates arguments
        if (!validate(getArguments())) {
			return false;
		}

        WorkspaceStatus wsStatus = getWorkspaceStatus();

		try {
		    String pathArg = optionalArgument(0);
		    WorkspaceContext theContext = ContextUtils.getContextForPath(wsStatus, pathArg);

		    printChildren(theContext);
		} catch (InvalidCommandArgumentException e) {
		    throw e;
		} catch (Exception e) {
		    print( MESSAGE_INDENT, Messages.getString( Messages.ShowCommand.Failure, e.getLocalizedMessage() ) ); 
		    return false;
		}
        return true;
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
     * @see org.komodo.shell.BuiltInShellCommand#shouldCommit()
     */
    @Override
    protected boolean shouldCommit() {
        return false;
    }
    

    private String getFormat( final int column1Width,
                              final int column2Width ) {
        final StringBuilder result = new StringBuilder();
        result.append( "%-" ).append( column1Width + 5 ).append( "s%-" ).append( column2Width + 5 ).append( 's' ); //$NON-NLS-1$ //$NON-NLS-2$
        return result.toString();
    }

    private String getHeaderDelimiter( final int width ) {
        final StringBuilder dashes = new StringBuilder();

        for ( int i = 0; i < ( width ); ++i ) {
            dashes.append( HYPHEN );
        }

        return dashes.toString();
    }

    private void printChildren( final WorkspaceContext context ) throws Exception {
        final List< WorkspaceContext > children = context.getChildren();

        if ( children.isEmpty() ) {
            String noChildrenMsg = Messages.getString( Messages.ShowCommand.noChildrenMsg, context.getType(), context.getFullName() );
            print( MESSAGE_INDENT, noChildrenMsg );
            return;
        }

        int maxNameWidth = DEFAULT_WIDTH;
        int maxTypeWidth = DEFAULT_WIDTH;

        // loop through children getting name, type, and finding widest child name
        for ( int i = 0, size = children.size(); i < size; ++i ) {
            final String name = children.get( i ).getName();

            if ( maxNameWidth < name.length() ) {
                maxNameWidth = name.length();
            }

            final String type = children.get( i ).getType();

            if ( maxTypeWidth < type.length() ) {
                maxTypeWidth = type.length();
            }
        }

        // sort
        final Comparator< WorkspaceContext > sorter = new Comparator< WorkspaceContext >() {

            /**
             * {@inheritDoc}
             *
             * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
             */
            @Override
            public int compare( final WorkspaceContext thisContext,
                                final WorkspaceContext thatContext ) {
                try {
                    final String thisType = thisContext.getType();
                    int result = thisType.compareTo( thatContext.getType() );

                    if ( result == 0 ) {
                        return thisContext.getName().compareTo( thatContext.getName() );
                    }

                    return result;
                } catch ( final Exception e ) {
                    KLog.getLogger().error( "Error comparing WorkspaceContext objects", e ); //$NON-NLS-1$
                    return 0;
                }
            }

        };
        Collections.sort( children, sorter );

        // Print children header
        final String childrenHeader = Messages.getString( Messages.ShowCommand.ChildrenHeader, context.getType(), context.getFullName() );
        print( MESSAGE_INDENT, childrenHeader );

        final String format = getFormat( maxNameWidth, maxTypeWidth );
        print( MESSAGE_INDENT,
               String.format( format, Messages.getString( SHELL.CHILD_NAME_HEADER ), Messages.getString( SHELL.CHILD_TYPE_HEADER ) ) );
        print( MESSAGE_INDENT, String.format( format, getHeaderDelimiter( maxNameWidth ), getHeaderDelimiter( maxTypeWidth ) ) );

        // Print each child
        for ( final WorkspaceContext childContext : children ) {
            final String childName = childContext.getName();
            final String childType = childContext.getType();
            print( MESSAGE_INDENT, String.format( format, childName, childType ) );
        }
    }

    protected boolean validate(Arguments allArgs) throws Exception {
        // optional path arg
        if(!allArgs.isEmpty()) {
            // Optional path arg
            String pathArg = optionalArgument(0);
            if(!validatePath(pathArg)) {
                return false;
            }
        }

        return true;
    }

	/**
     * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion(String lastArgument, List<CharSequence> candidates) throws Exception {

        if (getArguments().isEmpty()) {
            // The arg is expected to be a path
            updateTabCompleteCandidatesForPath(candidates, getContext(), true, lastArgument);

            // Do not put space after it - may want to append more to the path
            return CompletionConstants.NO_APPEND_SEPARATOR;
            // Tab completion for "property" - expects a valid property for the current context.
        }

        return -1;
    }

}
