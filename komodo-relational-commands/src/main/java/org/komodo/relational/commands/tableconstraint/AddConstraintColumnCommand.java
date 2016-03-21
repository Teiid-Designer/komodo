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
package org.komodo.relational.commands.tableconstraint;

import java.util.Arrays;
import java.util.List;

import org.komodo.relational.model.Column;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TableConstraint;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.ui.KomodoObjectLabelProvider;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to add a column to a {@link TableConstraint}.
 */
public final class AddConstraintColumnCommand extends TableConstraintShellCommand {

    static final String NAME = "add-column"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddConstraintColumnCommand( final WorkspaceStatus status ) {
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
            final String columnPath = requiredArgument( 0, I18n.bind( TableConstraintCommandsI18n.missingColumnPathForAdd ) );

            // Validate the display Path
            String validationMsg = validatePath(columnPath);
            if(!validationMsg.equals(CompletionConstants.OK)) {
                return new CommandResultImpl(false, validationMsg, null);
            }

            // Get the Object at the supplied path
            KomodoObject possible = getWorkspaceStatus().getContextForDisplayPath(columnPath.trim());

            Column column = null;
            { // see if valid column
                try {
                    if ( Column.RESOLVER.resolvable( getTransaction(), possible ) ) {
                        column = Column.RESOLVER.resolve( getTransaction(), possible );
                    } else {
                        result = new CommandResultImpl( false,
                                                        I18n.bind( TableConstraintCommandsI18n.invalidColumnPath, columnPath ),
                                                        null );
                    }
                } catch ( final Exception e ) {
                    result = new CommandResultImpl( false,
                                                    I18n.bind( TableConstraintCommandsI18n.invalidColumnPath, columnPath ),
                                                    null );
                }
            }

            if(column!=null) {
                final TableConstraint constraint = getTableConstraint();

                // must be a column in the parent of the table constraint
                final KomodoObject parentTable = constraint.getParent( getTransaction() );

                if ( parentTable.equals( column.getParent( getTransaction() ) ) ) {
                    constraint.addColumn( getTransaction(), column );
                    result = new CommandResultImpl( I18n.bind( TableConstraintCommandsI18n.columnRefAdded,
                                                               columnPath,
                                                               getWorkspaceStatus().getCurrentContextDisplayPath( null ) ) );
                } else {
                    result = new CommandResultImpl( false,
                                                    I18n.bind( TableConstraintCommandsI18n.invalidColumn,
                                                                getWorkspaceStatus().getDisplayPath( column, null ),
                                                                constraint.getName( getTransaction() ) ),
                                                    null );
                }
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
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( TableConstraintCommandsI18n.addConstraintColumnHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( TableConstraintCommandsI18n.addConstraintColumnExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( TableConstraintCommandsI18n.addConstraintColumnUsage ) );
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
            final Repository.UnitOfWork uow = getTransaction();

            // can only add columns of the parent table
            final Table parent = getTableConstraint().getTable( uow );
            final Column[] columns = parent.getColumns( uow );

            if ( columns.length == 0 ) {
                return TabCompletionModifier.AUTO;
            }

            final KomodoObjectLabelProvider labelProvider =getWorkspaceStatus().getObjectLabelProvider(columns[0]);

            for ( final Column column : Arrays.asList( columns ) ) {
                final String absolutePath = column.getAbsolutePath();
                final String displayPath = labelProvider.getDisplayPath( getTransaction(), column, null );

                if ( StringUtils.isBlank( lastArgument )
                     || absolutePath.startsWith( lastArgument )
                     || displayPath.startsWith( lastArgument ) ) {
                    candidates.add( displayPath );
                }
            }
        }

        // no completions if more than one arg
        return TabCompletionModifier.AUTO;
    }

}
