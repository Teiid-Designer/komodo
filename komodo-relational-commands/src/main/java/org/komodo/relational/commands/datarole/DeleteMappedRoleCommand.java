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
package org.komodo.relational.commands.datarole;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.komodo.relational.vdb.DataRole;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to delete a MappedRole from a DataRole.
 */
public final class DeleteMappedRoleCommand extends DataRoleShellCommand {

    static final String NAME = "delete-mapped-role"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public DeleteMappedRoleCommand( final WorkspaceStatus status ) {
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
            final String mappedRoleName = requiredArgument( 0, I18n.bind( DataRoleCommandsI18n.missingMappedRoleName ) );

            final DataRole dataRole = getDataRole();
            
            // Determine if mapped role exists before attempting to delete.
            String[] mappedRoles = dataRole.getMappedRoles(getTransaction());
            if(mappedRoles.length==0) {
                return new CommandResultImpl( false, I18n.bind( DataRoleCommandsI18n.deleteMappedRoleNoRolesError, dataRole.getName(getTransaction()) ), null );
            } else if(!Arrays.asList(mappedRoles).contains(mappedRoleName)) {
                return new CommandResultImpl( false, I18n.bind( DataRoleCommandsI18n.deleteMappedRoleNoMatchingRoleError, dataRole.getName(getTransaction()), mappedRoleName ), null );
            }
                                                                                                                                                                                            
            dataRole.removeMappedRole( getTransaction(), mappedRoleName );

            result = new CommandResultImpl( I18n.bind( DataRoleCommandsI18n.mappedRoleDeleted, mappedRoleName ) );
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
        print( indent, I18n.bind( DataRoleCommandsI18n.deleteMappedRoleHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( DataRoleCommandsI18n.deleteMappedRoleExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( DataRoleCommandsI18n.deleteMappedRoleUsage ) );
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
        final DataRole dataRole = getDataRole();
        final String[] mappedRoles = dataRole.getMappedRoles( uow );
        List<String> existingRoleNames = new ArrayList<String>(mappedRoles.length);
        for(String mappedRole : mappedRoles) {
            existingRoleNames.add(mappedRole);
        }

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( existingRoleNames );
            } else {
                for ( final String item : existingRoleNames ) {
                    if ( item.startsWith( lastArgument ) ) {
                        candidates.add( item );
                    }
                }
            }
        }
        return TabCompletionModifier.AUTO;
    }

}
