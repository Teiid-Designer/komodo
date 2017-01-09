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

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.komodo.relational.model.Column;
import org.komodo.relational.model.TableConstraint;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.ui.KomodoObjectLabelProvider;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to remove a column from a {@link TableConstraint}.
 */
public final class DeleteConstraintColumnCommand extends TableConstraintShellCommand {

    static final String NAME = "delete-column"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public DeleteConstraintColumnCommand( final WorkspaceStatus status ) {
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
            final String columnPathArg = requiredArgument( 0, I18n.bind( TableConstraintCommandsI18n.missingColumnPathForDelete ) );

            // Validate the display Path
            String validationMsg = validatePath(columnPathArg);
            if(!validationMsg.equals(CompletionConstants.OK)) {
                return new CommandResultImpl(false, validationMsg, null);
            }

            // Get the Object at the supplied path
            KomodoObject possible = getWorkspaceStatus().getContextForDisplayPath(columnPathArg.trim());

            Column column = null;
            { // see if valid column
                try {
                    if ( Column.RESOLVER.resolvable( getTransaction(), possible ) ) {
                        column = Column.RESOLVER.resolve( getTransaction(), possible );
                    } else {
                        result = new CommandResultImpl( false,
                                                        I18n.bind( TableConstraintCommandsI18n.invalidColumnPath, columnPathArg ),
                                                        null );
                    }
                } catch ( final Exception e ) {
                    result = new CommandResultImpl( false,
                                                    I18n.bind( TableConstraintCommandsI18n.invalidColumnPath, columnPathArg ),
                                                    null );
                }
            }

            if ( column != null ) {
                final TableConstraint constraint = getTableConstraint();
                constraint.removeColumn( getTransaction(), column );

                result = new CommandResultImpl( I18n.bind( TableConstraintCommandsI18n.columnRemoved,
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
        print( indent, I18n.bind( TableConstraintCommandsI18n.deleteConstraintColumnHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( TableConstraintCommandsI18n.deleteConstraintColumnExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( TableConstraintCommandsI18n.deleteConstraintColumnUsage ) );
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
            final TableConstraint constraint = getTableConstraint();
            final Column[] refCols = constraint.getColumns( getTransaction() );

            // no tab-completion if no columns to remove
            if ( refCols.length == 0 ) {
                return TabCompletionModifier.AUTO;
            }

            // add matching paths of referenced columns
            final boolean noLastArg = StringUtils.isBlank( lastArgument );
            if(refCols.length!=0){
				KomodoObjectLabelProvider provider = getWorkspaceStatus().getObjectLabelProvider(refCols[0]);
				for (final Column column : refCols) {
					final String displayPath = provider.getDisplayPath(getTransaction(), column, null);
					final String absolutePath = column.getAbsolutePath();

					if (noLastArg || displayPath.startsWith(lastArgument) || absolutePath.startsWith(lastArgument)) {
						candidates.add(displayPath);
					}
				}

				Collections.sort(candidates, new Comparator<CharSequence>() {

					/**
					 * {@inheritDoc}
					 *
					 * @see java.util.Comparator#compare(java.lang.Object,
					 *      java.lang.Object)
					 */
					@Override
					public int compare(final CharSequence thisPath, final CharSequence thatPath) {
						return thisPath.toString().compareTo(thatPath.toString());
					}
				});
			}
		}
		// no completions if more than one arg
		return TabCompletionModifier.AUTO;
	}

}
