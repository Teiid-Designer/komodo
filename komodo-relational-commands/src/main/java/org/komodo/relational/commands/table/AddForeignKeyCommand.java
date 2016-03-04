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

import org.komodo.relational.model.Table;
import org.komodo.repository.ObjectImpl;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to add a Foreign Key to a Table.
 */
public final class AddForeignKeyCommand extends TableShellCommand {

    static final String NAME = "add-foreign-key"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddForeignKeyCommand( final WorkspaceStatus status ) {
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
            final String fkName = requiredArgument( 0, I18n.bind( TableCommandsI18n.missingForeignKeyName ) );
            final String tableRefPath = requiredArgument( 1, I18n.bind( TableCommandsI18n.missingForeignKeyTableRefPath ) );

            Table referencedTable = null;

            { // see if valid table path
                String repoPath = getWorkspaceStatus().getCurrentContextLabelProvider().getPath( getTransaction(), tableRefPath );

                if ( StringUtils.isBlank( repoPath ) ) {
                    repoPath = tableRefPath;
                }

                final KomodoObject possible = new ObjectImpl( getRepository(), repoPath, 0 );

                try {
                    if ( Table.RESOLVER.resolvable( getTransaction(), possible ) ) {
                        referencedTable = Table.RESOLVER.resolve( getTransaction(), possible );
                    } else {
                        result = new CommandResultImpl( false, I18n.bind( TableCommandsI18n.invalidTablePath, tableRefPath ), null );
                    }
                } catch ( final Exception e ) {
                    result = new CommandResultImpl( false, I18n.bind( TableCommandsI18n.invalidTablePath, tableRefPath ), null );
                }
            }

            // create foreign key if found referenced table
            if ( referencedTable != null ) {
                final Table table = getTable();
                table.addForeignKey( getTransaction(), fkName, referencedTable );
                result = new CommandResultImpl( I18n.bind( TableCommandsI18n.foreignKeyAdded, fkName ) );
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
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( TableCommandsI18n.addForeignKeyHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( TableCommandsI18n.addForeignKeyExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( TableCommandsI18n.addForeignKeyUsage ) );
    }

}
