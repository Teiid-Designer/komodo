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
package org.komodo.relational.commands.table;

import java.util.List;

import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.Table;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to delete a PrimaryKey from a Table.
 */
public final class DeletePrimaryKeyCommand extends TableShellCommand {

    static final String NAME = "delete-primary-key"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public DeletePrimaryKeyCommand( final WorkspaceStatus status ) {
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
            final Table table = getTable();
            if( table.getPrimaryKey(getTransaction())==null ) {
                return new CommandResultImpl( I18n.bind( TableCommandsI18n.noPkToRemove ));
            }
            table.removePrimaryKey( getTransaction()  );

            result = new CommandResultImpl( I18n.bind( TableCommandsI18n.primaryKeyDeleted ) );
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
        return 0;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( TableCommandsI18n.deletePrimaryKeyHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( TableCommandsI18n.deletePrimaryKeyExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( TableCommandsI18n.deletePrimaryKeyUsage ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public TabCompletionModifier tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        final Arguments args = getArguments();
        final UnitOfWork uow = getTransaction();
        final Table table = getTable();
        final PrimaryKey pk = table.getPrimaryKey( uow );

        if(pk == null){
        	return TabCompletionModifier.NO_AUTOCOMPLETION;
        }
        String pkName=pk.getName(uow);

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.add( pkName );
            } else {
                    if ( pkName.startsWith( lastArgument ) ) {
                        candidates.add( pkName );
                    }
                }
            }

        return TabCompletionModifier.AUTO;
    }
}
