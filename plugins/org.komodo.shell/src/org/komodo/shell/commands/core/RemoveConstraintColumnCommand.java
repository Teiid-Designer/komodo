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
import static org.komodo.shell.Messages.RemoveConstraintColumnCommand.COLUMN_PATH_NOT_FOUND;
import static org.komodo.shell.Messages.RemoveConstraintColumnCommand.COLUMN_REF_REMOVED;
import static org.komodo.shell.Messages.RemoveConstraintColumnCommand.ERROR;
import static org.komodo.shell.Messages.RemoveConstraintColumnCommand.INVALID_COLUMN_PATH;
import static org.komodo.shell.Messages.RemoveConstraintColumnCommand.MISSING_COLUMN_PATH_ARG;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.TableConstraint;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.Messages;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.utils.StringUtils;

/**
 * Removes a {@link Column column reference} from a {@link TableConstraint table constraint}.
 */
public final class RemoveConstraintColumnCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "remove-column"; //$NON-NLS-1$

    /**
     * @param status
     *        the workspace status (cannot be <code>null</code>)
     */
    public RemoveConstraintColumnCommand( final WorkspaceStatus status ) {
        super( status, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#execute()
     */
    @Override
    public boolean execute() throws Exception {
        try {
            final String columnPathArg = requiredArgument( 0, Messages.getString( MISSING_COLUMN_PATH_ARG ) );

            // get selected column and remove
            final WorkspaceContext columnContext = ContextUtils.getContextForPath( getWorkspaceStatus(), columnPathArg );

            if ( columnContext == null ) {
                print( MESSAGE_INDENT, Messages.getString( COLUMN_PATH_NOT_FOUND, columnPathArg ) );
                return false;
            }

            final KomodoObject column = columnContext.getKomodoObj();

            if ( column instanceof Column ) {
                final TableConstraint constraint = getTableConstraint();
                constraint.removeColumn( getWorkspaceStatus().getTransaction(), ( Column )column );

                // Commit transaction
                getWorkspaceStatus().commit( RemoveConstraintColumnCommand.class.getSimpleName() );

                print( MESSAGE_INDENT, Messages.getString( COLUMN_REF_REMOVED, columnPathArg, getContext().getFullName() ) );
                return true;
            } else {
                print( MESSAGE_INDENT, Messages.getString( INVALID_COLUMN_PATH, columnPathArg ) );
                return false;
            }
        } catch ( final Exception e ) {
            print( MESSAGE_INDENT, Messages.getString( ERROR ) );
            print( MESSAGE_INDENT, "\t" + e.getLocalizedMessage() ); //$NON-NLS-1$
            return false;
        }
    }

    private TableConstraint getTableConstraint() {
        // initValidWsContextTypes() method assures execute is called only if current context is a TableConstraint
        final KomodoObject kobject = getContext().getKomodoObj();
        assert ( kobject instanceof TableConstraint );
        return ( TableConstraint )kobject;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.AbstractShellCommand#initValidWsContextTypes()
     */
    @Override
    public void initValidWsContextTypes() {
        final List< String > temp = new ArrayList<>();
        temp.add( KomodoType.PRIMARY_KEY.getType() );
        temp.add( KomodoType.UNIQUE_CONSTRAINT.getType() );
        temp.add( KomodoType.ACCESS_PATTERN.getType() );
        temp.add( KomodoType.FOREIGN_KEY.getType() );
        temp.add( KomodoType.INDEX.getType() );

        this.validWsContextTypes = Collections.unmodifiableList( temp );
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
            final TableConstraint constraint = getTableConstraint();
            final Column[] refCols = constraint.getColumns( getWorkspaceStatus().getTransaction() );

            // no tab-completion if no columns to remove
            if ( refCols.length == 0 ) {
                return -1;
            }

            // add matching paths of referenced columns
            final boolean noLastArg = StringUtils.isBlank( lastArgument );

            for ( final Column column : refCols ) {
                final String path = column.getAbsolutePath();

                if ( noLastArg || ( path.startsWith( lastArgument ) ) ) {
                    candidates.add( path );
                }
            }

            Collections.sort( candidates, new Comparator< CharSequence >() {

                /**
                 * {@inheritDoc}
                 *
                 * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
                 */
                @Override
                public int compare( final CharSequence thisPath,
                                    final CharSequence thatPath ) {
                    return thisPath.toString().compareTo( thatPath.toString() );
                }
            });

            return ( candidates.isEmpty() ? -1 : ( toString().length() + 1 ) );
        }

        // no completions if more than one arg
        return -1;
    }

}
