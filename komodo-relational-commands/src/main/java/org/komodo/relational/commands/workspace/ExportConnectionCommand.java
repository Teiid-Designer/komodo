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

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.connection.Connection;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.constants.ExportConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

/**
 * A shell command to export a Connection from Workspace context.
 */
public final class ExportConnectionCommand extends RelationalShellCommand {

    static final String NAME = "export-connection"; //$NON-NLS-1$

    private static final String OVERWRITE_1 = "-o"; //$NON-NLS-1$
    private static final String OVERWRITE_2 = "--overwrite"; //$NON-NLS-1$
    private static final List< String > VALID_OVERWRITE_ARGS = Arrays.asList( new String[] { OVERWRITE_1, OVERWRITE_2 } );

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ExportConnectionCommand( final WorkspaceStatus status ) {
        super( status, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        final boolean workspaceContext = isWorkspaceContext();
        final int fileNameIndex = ( workspaceContext ? 1 : 0 );
        String connectionName = null;
        String fileName = null;

        try {
            if ( workspaceContext ) {
                connectionName = requiredArgument( 0, I18n.bind( WorkspaceCommandsI18n.missingConnectionName ) );
            } else {
                connectionName = getContext().getName( getTransaction() );
            }

            fileName = requiredArgument( fileNameIndex, I18n.bind( WorkspaceCommandsI18n.missingOutputFileName ) );

            // If there is no file extension, add .xml
            if ( fileName.indexOf( DOT ) == -1 ) {
                fileName = fileName + DOT + "xml"; //$NON-NLS-1$
            }

            final String overwriteArg = optionalArgument( ( fileNameIndex + 1 ), null );
            final boolean overwrite = !StringUtils.isBlank( overwriteArg );

            // make sure overwrite arg is valid
            if ( overwrite && !VALID_OVERWRITE_ARGS.contains( overwriteArg ) ) {
                return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.overwriteArgInvalid, overwriteArg ), null );
            }

            // Determine if the Connection exists
            if ( workspaceContext
                 && !getWorkspaceManager(getTransaction()).hasChild( getTransaction(), connectionName, DataVirtLexicon.Connection.NODE_TYPE ) ) {

                return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.connectionNotFound, connectionName ), null );
            }

            final Connection connectionToExport = getConnection( workspaceContext, connectionName );
            final File file = new File( fileName );

            // If file exists, must have overwrite option
            if(file.exists() && !overwrite) {
                return new CommandResultImpl( false,
                                              I18n.bind( WorkspaceCommandsI18n.fileExistsOverwriteDisabled, fileName ),
                                              null );
            }

            if ( file.createNewFile() || ( file.exists() && overwrite ) ) {
                final UnitOfWork uow = getTransaction();
                Properties properties = new Properties();
                properties.put( ExportConstants.USE_TABS_PROP_KEY, true );
                byte[] xmlBytes = connectionToExport.export( uow, properties );
                final String sourceXml = new String(xmlBytes);

                // Write the file
                try{
                    Files.write(Paths.get(file.getPath()), sourceXml.getBytes());
                    return new CommandResultImpl( I18n.bind( WorkspaceCommandsI18n.connectionExported,
                                                             connectionToExport.getName( uow ),
                                                             fileName,
                                                             overwrite ) );
                } catch ( final Exception e ) {
                    return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.errorWritingFile, fileName ), e );
                }
            }

            return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.outputFileError, fileName ), null );
        } catch ( final Exception e ) {
            return new CommandResultImpl( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return ( isWorkspaceContext() ? 3 : 2 );
    }

    private Connection getConnection( final boolean workspaceContext,
                                      final String connectionName ) throws KException {
        assert !StringUtils.isBlank( connectionName );
        KomodoObject kobject = null;

        if ( workspaceContext ) {
            kobject = getWorkspaceManager(getTransaction()).getChild( getTransaction(), connectionName, DataVirtLexicon.Connection.NODE_TYPE );
        } else {
            kobject = getContext();
        }

        assert ( kobject != null );
        return Connection.RESOLVER.resolve( getTransaction(), kobject );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        return ( isConnectionContext() || isWorkspaceContext() );
    }

    private boolean isConnectionContext() {
        try {
            return Connection.RESOLVER.resolvable( getTransaction(), getContext() );
        } catch ( final Exception e ) {
            return false;
        }
    }

    private boolean isWorkspaceContext() {
        final String path = getContext().getAbsolutePath();
        return getWorkspaceStatus().getLabelProvider().isWorkspacePath(path);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( WorkspaceCommandsI18n.exportConnectionHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( WorkspaceCommandsI18n.exportConnectionExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( WorkspaceCommandsI18n.exportConnectionUsage ) );
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

        if ( isWorkspaceContext() ) {
            // arg 0 = vdb name, arg 1 = output file name, arg 2 = overwrite
            final KomodoObject[] connections = getWorkspaceManager(getTransaction()).findConnections( getTransaction() );

            if ( args.isEmpty() && ( connections.length != 0 ) ) {
                for ( final KomodoObject connection : connections ) {
                    final String name = connection.getName( getTransaction() );

                    if ( ( lastArgument == null ) || name.startsWith( lastArgument ) ) {
                        candidates.add( name );
                    }
                }
            } else if ( args.size() == 2 ) {
                candidates.add( OVERWRITE_2 );
            }
        } else if ( args.size() == 1 ) { // Connection context
            // arg 0 = output file name (no completion), arg 1 = overwrite
            candidates.add( OVERWRITE_2 );
        }

        return TabCompletionModifier.AUTO;
    }

}
