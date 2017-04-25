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
package org.komodo.relational.importer.connection;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.Objects;

import org.komodo.importer.AbstractImporter;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.ExistingNodeOptions;
import org.komodo.importer.ImportType;
import org.komodo.importer.Messages;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.modeshape.jcr.api.JcrConstants;
import org.teiid.modeshape.sequencer.dataservice.ConnectionReader;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

/**
 * An importer for connections.
 */
public class ConnectionImporter extends AbstractImporter {

    /**
     * @param repository the repository where the objects will be persisted (cannot be <code>null</code>)
     */
    public ConnectionImporter(Repository repository) {
        super(Objects.requireNonNull( repository, "repository"), ImportType.DS); //$NON-NLS-1$
    }

    @Override
    protected boolean handleExistingNode( UnitOfWork transaction,
                                          KomodoObject parentObject,
                                          ImportOptions importOptions,
                                          ImportMessages importMessages ) {        return true;
    }

    @Override
    protected void executeImport( UnitOfWork transaction,
                                  String content,
                                  KomodoObject parentObject,
                                  ImportOptions importOptions,
                                  ImportMessages importMessages ) throws KException {
        try {
            final ConnectionReader reader = new ConnectionReader();
            final org.teiid.modeshape.sequencer.dataservice.Connection connection = reader.read( new ByteArrayInputStream( content.getBytes() ) );
            final boolean hasConnection = getWorkspaceManager(transaction).hasChild( transaction,
                                                                          connection.getName(),
                                                                          DataVirtLexicon.Connection.NODE_TYPE );

            boolean shouldSequence = false;

            // if already exists see if we should overwrite
            if ( hasConnection ) {
                final ExistingNodeOptions optionValue = ( ExistingNodeOptions )importOptions.getOption( ImportOptions.OptionKeys.HANDLE_EXISTING );

                switch ( optionValue ) {
                    case RETURN:
                    case CREATE_NEW:
                        importMessages.addErrorMessage( Messages.getString( Messages.IMPORTER.nodeExistsReturn, connection.getName() ) );
                        break;
                    case OVERWRITE:
                        shouldSequence = true;
                        final KomodoObject existing = parentObject.getChild( transaction, connection.getName() );
                        existing.remove( transaction );
                        break;
                    default:
                        break;
                }
            } else {
                shouldSequence = true;
            }

            if ( shouldSequence ) {
                // upload connection file so that it will be sequenced
                final Connection ds = getWorkspaceManager(transaction).createConnection( transaction, parentObject, connection.getName() );
                final KomodoObject fileNode = ds.addChild( transaction, JcrConstants.JCR_CONTENT, JcrConstants.NT_RESOURCE );

                ByteArrayInputStream contentStream = new ByteArrayInputStream(content.getBytes());
                fileNode.setProperty( transaction, JcrConstants.JCR_DATA, contentStream );
            }
        } catch ( final Exception e ) {
            if ( e instanceof KException ) {
                throw ( KException )e;
            }

            throw new KException( e );
        }
    }

    private WorkspaceManager getWorkspaceManager(UnitOfWork transaction) throws KException {
        return WorkspaceManager.getInstance(getRepository(), transaction);
    }

    /**
     * Perform the connection import using the specified xml Stream.
     *
     * @param transaction the transaction
     * @param stream the connection xml input stream
     * @param parentObject the parent object in which to place the vdb
     * @param importOptions the options for the import
     * @param importMessages the messages recorded during the import
     */
    public void importDS( UnitOfWork transaction,
                          InputStream stream,
                          KomodoObject parentObject,
                          ImportOptions importOptions,
                          ImportMessages importMessages ) {
        ArgCheck.isNotNull( stream );

        try {
            doImport( transaction, toString( stream ), parentObject, importOptions, importMessages );
        } catch ( Exception ex ) {
            importMessages.addErrorMessage( ex.getLocalizedMessage() );
        }
    }

    /**
     * Perform the connection import using the specified ds xml File.
     *
     * @param uow the transaction
     * @param dsXmlFile the ds xml file
     * @param parentObject the parent object in which to place the ds
     * @param importOptions the options for the import
     * @param importMessages the messages recorded during the import
     */
    public void importDS( UnitOfWork uow,
                          File dsXmlFile,
                          KomodoObject parentObject,
                          ImportOptions importOptions,
                          ImportMessages importMessages ) {
        if ( !validFile( dsXmlFile, importMessages ) ) return;

        try {
            importDS( uow, new FileInputStream( dsXmlFile ), parentObject, importOptions, importMessages );
        } catch ( Exception ex ) {
            // logging was done in other importDS method
        }
    }
}
