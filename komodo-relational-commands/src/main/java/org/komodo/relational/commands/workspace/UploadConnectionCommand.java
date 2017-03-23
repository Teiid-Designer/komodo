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

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

import org.komodo.relational.connection.Connection;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;
import org.modeshape.jcr.api.JcrConstants;
import org.teiid.modeshape.sequencer.dataservice.ConnectionReader;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

/**
 * Loads a {@link Connection connection} from a local file.
 */
public final class UploadConnectionCommand extends WorkspaceShellCommand {

    static final String NAME = "upload-connection"; //$NON-NLS-1$

    private static final List< String > VALID_OVERWRITE_ARGS = Arrays.asList( new String[] { "-o", "--overwrite" } ); //$NON-NLS-1$ //$NON-NLS-2$;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public UploadConnectionCommand( final WorkspaceStatus status ) {
        super( status, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        try {
            final String fileName = requiredArgument( 0, I18n.bind( WorkspaceCommandsI18n.missingInputConnectionFilePath ) );
            final String overwriteArg = optionalArgument( 1, null );
            final boolean overwrite = !StringUtils.isBlank( overwriteArg );

            // make sure overwrite arg is valid
            if ( overwrite && !VALID_OVERWRITE_ARGS.contains( overwriteArg ) ) {
                return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.overwriteArgInvalid, overwriteArg ), null );
            }

            { // Validates the supplied fileNameArg is a valid, readable file, and has property extension
                final String validationResult = validateReadableFileArg( fileName );

                if ( !CompletionConstants.OK.equals( validationResult ) ) {
                    return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.inputFileError, fileName, validationResult ), null );
                }
            }

            // Reader validates the XML file and gets the connection name which we need to check for overwriting
            final Repository.UnitOfWork uow = getTransaction();
            final File connectionFile = new File(fileName);
            final ConnectionReader reader = new ConnectionReader();
            final org.teiid.modeshape.sequencer.dataservice.Connection connection = reader.read( new FileInputStream( connectionFile ) );
            final boolean hasConnection = getWorkspaceManager(uow).hasChild( uow,
                                                                          connection.getName(),
                                                                          DataVirtLexicon.Connection.NODE_TYPE );

            if ( hasConnection && !overwrite ) {
                return new CommandResultImpl( false,
                                              I18n.bind( WorkspaceCommandsI18n.connectionOverwriteDisabled,
                                                         fileName,
                                                         connection.getName() ),
                                              null );
            }

            // delete current if necessary
            if ( hasConnection && overwrite ) {
                getWorkspaceManager(uow).removeChild( uow, connection.getName() );
            }

            // upload data source file so that it will be sequenced
            final Connection ds = getWorkspaceManager(uow).createConnection( uow, null, connection.getName() );
            final KomodoObject fileNode = ds.addChild( uow, JcrConstants.JCR_CONTENT, JcrConstants.NT_RESOURCE );
            final byte[] content = Files.readAllBytes( Paths.get( fileName ) );
            final ByteArrayInputStream stream = new ByteArrayInputStream( content );
            fileNode.setProperty( uow, JcrConstants.JCR_DATA, stream );

            return new CommandResultImpl( I18n.bind( WorkspaceCommandsI18n.connectionUploaded, connection.getName() ) );
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
        return 2;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( WorkspaceCommandsI18n.uploadConnectionHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( WorkspaceCommandsI18n.uploadConnectionExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( WorkspaceCommandsI18n.uploadConnectionUsage ) );
    }

}
