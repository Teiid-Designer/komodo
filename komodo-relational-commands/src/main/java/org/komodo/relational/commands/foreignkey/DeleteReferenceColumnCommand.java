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
package org.komodo.relational.commands.foreignkey;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.repository.ObjectImpl;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to remove a reference column from a {@link ForeignKey foreign key}.
 */
public final class DeleteReferenceColumnCommand extends ForeignKeyShellCommand {

    static final String NAME = "delete-ref-column"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public DeleteReferenceColumnCommand( final WorkspaceStatus status ) {
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
            final String columnPathArg = requiredArgument( 0, I18n.bind( ForeignKeyCommandsI18n.missingColumnPathForDelete ) );

            // get reference of the column at the specified path
            Column column = null;
            { // see if valid column
                String repoPath = getWorkspaceStatus().getCurrentContextLabelProvider().getPath( getTransaction(), columnPathArg );

                if ( StringUtils.isBlank( repoPath ) ) {
                    repoPath = columnPathArg;
                }

                final KomodoObject possible = new ObjectImpl( getRepository(), repoPath, 0 );

                try {
                    if ( Column.RESOLVER.resolvable( getTransaction(), possible ) ) {
                        column = Column.RESOLVER.resolve( getTransaction(), possible );
                    } else {
                        result = new CommandResultImpl( false,
                                                        I18n.bind( ForeignKeyCommandsI18n.invalidColumnPath, columnPathArg ),
                                                        null );
                    }
                } catch ( final Exception e ) {
                    result = new CommandResultImpl( false,
                                                    I18n.bind( ForeignKeyCommandsI18n.invalidColumnPath, columnPathArg ),
                                                    e );
                }
            }

            if ( column != null ) {
                final ForeignKey foreignKey = getForeignKey();
                foreignKey.removeReferencesColumn( getTransaction(), column );

                result = new CommandResultImpl( I18n.bind( ForeignKeyCommandsI18n.columnRemoved,
                                                           columnPathArg,
                                                           getContext().getAbsolutePath() ) );
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
        print( indent, I18n.bind( ForeignKeyCommandsI18n.deleteReferenceColumnHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ForeignKeyCommandsI18n.deleteReferenceColumnExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ForeignKeyCommandsI18n.deleteReferenceColumnUsage ) );
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
            final ForeignKey foreignKey = getForeignKey();
            final Column[] refCols = foreignKey.getReferencesColumns( getTransaction() );

            // no tab-completion if no columns to remove
            if ( refCols.length == 0 ) {
                return TabCompletionModifier.AUTO;
            }

            // add matching paths of referenced columns
            final boolean noLastArg = StringUtils.isBlank( lastArgument );

            for ( final Column column : refCols ) {
                final String displayPath = getWorkspaceStatus().getCurrentContextLabelProvider().getDisplayPath( getTransaction(),
                                                                                                                 column,
                                                                                                                 null );
                final String absolutePath = column.getAbsolutePath();

                if ( noLastArg || displayPath.startsWith( lastArgument ) || absolutePath.startsWith( lastArgument ) ) {
                    candidates.add( displayPath );
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
            } );
        }

        // no completions if more than one arg
        return TabCompletionModifier.AUTO;
    }

}
