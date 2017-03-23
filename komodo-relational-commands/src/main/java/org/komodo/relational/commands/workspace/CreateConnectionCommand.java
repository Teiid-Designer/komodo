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
package org.komodo.relational.commands.workspace;

import org.komodo.relational.commands.RelationalCommandsI18n;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.KomodoObjectUtils;
import org.komodo.utils.i18n.I18n;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

/**
 * A shell command to create a connection object.
 */
public final class CreateConnectionCommand extends WorkspaceShellCommand {

    static final String NAME = "create-connection"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public CreateConnectionCommand( final WorkspaceStatus status ) {
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
            final String sourceName = requiredArgument( 0, I18n.bind( WorkspaceCommandsI18n.missingConnectionName ) );

            // Can optionally supply the type of connection (jdbc vs non-jdbc)
            final String isJdbc = optionalArgument( 1, Boolean.TRUE.toString() );

            if ( !KomodoObjectUtils.TRUE_STRING.equals( isJdbc ) && !KomodoObjectUtils.FALSE_STRING.equals( isJdbc ) ) {
                return new CommandResultImpl( I18n.bind( WorkspaceCommandsI18n.invalidConnectionIndicator, isJdbc ) );
            }

            final WorkspaceManager mgr = getWorkspaceManager(getTransaction());

            // Do not allow create if object with this name already exists
            if(mgr.hasChild(getTransaction(), sourceName, DataVirtLexicon.Connection.NODE_TYPE)) {
                return new CommandResultImpl( false, I18n.bind( RelationalCommandsI18n.cannotCreateChildAlreadyExistsError, sourceName, Connection.class.getSimpleName() ), null );
            }

            Connection connection = mgr.createConnection( getTransaction(), null, sourceName );
            connection.setJdbc(getTransaction(), Boolean.parseBoolean( isJdbc ));

            result = new CommandResultImpl( I18n.bind( WorkspaceCommandsI18n.connectionCreated, sourceName ) );
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
        print( indent, I18n.bind( WorkspaceCommandsI18n.createConnectionHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( WorkspaceCommandsI18n.createConnectionExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( WorkspaceCommandsI18n.createConnectionUsage ) );
    }

}
