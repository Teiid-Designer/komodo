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
package org.komodo.relational.dataservice.internal;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.List;
import java.util.Objects;
import java.util.Properties;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.ExistingNodeOptions;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.relational.DeployStatus;
import org.komodo.relational.Messages;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.dataservice.ConnectionEntry;
import org.komodo.relational.dataservice.DataServiceEntry;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.dataservice.DataserviceManifest;
import org.komodo.relational.dataservice.DriverEntry;
import org.komodo.relational.dataservice.ServiceVdbEntry;
import org.komodo.relational.dataservice.VdbEntry;
import org.komodo.relational.resource.Driver;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.DocumentType;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.FileUtils;
import org.komodo.utils.StringUtils;
import org.modeshape.jcr.api.JcrConstants;
import org.teiid.modeshape.sequencer.dataservice.DataServiceManifest;
import org.teiid.modeshape.sequencer.dataservice.DataServiceManifestReader;

/**
 * Handles importing and exporting of {@link Dataservice data services}.
 */
public class DataserviceConveyor implements StringConstants {

    /**
     * Buffer size for the byte arrays
     */
    public static final int BUFFER_SIZE = 8192;

    private final Repository repository;

    /**
     * @param repository the associated repository (cannot be <code>null</code>)
     */
    public DataserviceConveyor(Repository repository) {
        this.repository = Objects.requireNonNull( repository, "repository" ); //$NON-NLS-1$
    }

    protected WorkspaceManager getWorkspaceManager(UnitOfWork transaction) throws KException {
        return WorkspaceManager.getInstance(repository, transaction);
    }

    private static String determineNewName(UnitOfWork transaction, KomodoObject parent, String nodeName) throws KException {
        for (int i = 0; i < 1000; ++i) {
            String newName = nodeName + UNDERSCORE + i;
            if (!parent.hasChild(transaction, newName))
                return newName;
        }

        throw new KException(org.komodo.importer.Messages.getString(
                                                                    org.komodo.importer.Messages.IMPORTER.newNameFailure, nodeName));
    }

    public static boolean handleExistingNode(UnitOfWork transaction, KomodoObject parent, ImportOptions importOptions,
                                         ImportMessages importMessages) throws KException {

        // dataservice name to create
        String dsName = importOptions.getOption(OptionKeys.NAME).toString();

        // No node with the requested name - ok to create
        if (!parent.hasChild(transaction, dsName))
            return true;

        // Option specifying how to handle when node exists with requested name
        ExistingNodeOptions exNodeOption = (ExistingNodeOptions)importOptions.getOption(OptionKeys.HANDLE_EXISTING);

        switch (exNodeOption) {
            // RETURN - Return 'false' - do not create a node.  Log a progress message
            case RETURN:
                importMessages.addProgressMessage(org.komodo.importer.Messages.getString(
                                                                                      org.komodo.importer.Messages.IMPORTER.nodeExistsReturn, dsName));
                return false;
            // CREATE_NEW - Return 'true' - will create a new data service with new unique name.  Log a progress message.
            case CREATE_NEW:
                String newName = determineNewName(transaction, parent, dsName);
                importMessages.addProgressMessage(org.komodo.importer.Messages.getString(
                                                                                         org.komodo.importer.Messages.IMPORTER.nodeExistCreateNew, dsName, newName));
                importOptions.setOption(OptionKeys.NAME, newName);
                break;
            // OVERWRITE - Return 'true' - deletes the existing data service so that new one can replace existing.
            case OVERWRITE:
                KomodoObject oldNode = parent.getChild(transaction, dsName);
                oldNode.remove(transaction);
        }

        return true;
    }

    private String extractDsName(File zFile) throws KException {
        ZipFile zipFile = null;

        try {
            zipFile = new ZipFile(zFile);
            Enumeration<? extends ZipEntry> entries = zipFile.entries();
            if (!entries.hasMoreElements())
                return null;

            while (entries.hasMoreElements()) {
                ZipEntry entry = entries.nextElement();
                ByteArrayOutputStream bos = null;
                InputStream zipStream = null;

                try {
                    String name = entry.getName();
                    if (!DataserviceManifest.MANIFEST.equals(name)) {
                        continue;
                    }

                    bos = new ByteArrayOutputStream();
                    final byte[] buf = new byte[BUFFER_SIZE];
                    int length;

                    zipStream = zipFile.getInputStream(entry);
                    while ((length = zipStream.read(buf, 0, buf.length)) >= 0) {
                        bos.write(buf, 0, length);
                    }

                    byte[] content = bos.toByteArray();
                    ByteArrayInputStream entryStream = new ByteArrayInputStream(content);

                    DataServiceManifestReader reader = new DataServiceManifestReader();
                    DataServiceManifest manifest = reader.read(entryStream);
                    return manifest.getName();

                } finally {
                    if (bos != null)
                        bos.close();

                    if (zipStream != null)
                        zipStream.close();
                }
            }

        } catch (Exception ex) {
            throw new KException(ex);
        } finally {
            try {
                if (zipFile != null)
                    zipFile.close();
            } catch (IOException e) {
            }
        }

        return null;
    }

    private void overrideName(File zipFile, ImportOptions importOptions) throws Exception {
        String dsName = extractDsName(zipFile);
        if (dsName == null)
            return;

        importOptions.setOption(OptionKeys.NAME, dsName);
    }

    /**
     * Import a {@link Dataservice} from the given source {@link InputStream}
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param srcStream the source stream of the data service
     * @param parent the parent of the imported data service
     * @param importOptions options for the import procedure
     * @param importMessages holder to contain any status messages from the import procedure
     * @throws KException if error occurs
     */
    public void dsImport(UnitOfWork transaction, InputStream srcStream, KomodoObject parent, ImportOptions importOptions,
                         ImportMessages importMessages) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull(srcStream, "Source Stream");

        //
        // Since we need to extract the name of the dataservice from the manifest
        // we must write the stream to a temporary file so that it can be read twice, ie.
        // 1st to extract the name, 2nd when its sequenced
        //
        long timestamp = System.currentTimeMillis();
        File zipFile = new File(FileUtils.tempDirectory(), timestamp + ZIP_SUFFIX);
        zipFile.deleteOnExit();

        try {
            FileUtils.write(srcStream, zipFile);
            overrideName(zipFile, importOptions);

            String dsName = importOptions.getOption(OptionKeys.NAME).toString();
            if (dsName == null)
                throw new Exception(org.komodo.importer.Messages.getString(
                                                                           org.komodo.importer.Messages.IMPORTER.noNameFailure));

            boolean doImport = handleExistingNode(transaction, parent, importOptions, importMessages);
            if (!doImport) {
                // Handling existing node advises not to continue
                return;
            }

            WorkspaceManager mgr = getWorkspaceManager(transaction);
            Dataservice dataservice = mgr.createDataservice( transaction,
                                                             parent,
                                                             ( String )importOptions.getOption( ImportOptions.OptionKeys.NAME ) );

            // save content so that sequencer will start
            final KomodoObject fileNode = dataservice.addChild( transaction, JcrConstants.JCR_CONTENT, JcrConstants.NT_RESOURCE );
            fileNode.setProperty( transaction, JcrConstants.JCR_DATA, new FileInputStream(zipFile) );
        } catch (Exception ex) {
            throw new KException(ex);
        }
    }

    /**
     * Export the given data service
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param dataService
     *        the data service to export
     * @param exportProperties
     *        export properties to be used during the export procedure
     * @return the array of bytes of the exported data service
     * @throws KException
     *         if errors occurs
     */
    public byte[] export( final UnitOfWork transaction,
                          final Dataservice dataService,
                          final Properties exportProperties ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        ZipOutputStream zipStream = null;

        try ( final ByteArrayOutputStream bos = new ByteArrayOutputStream() ) {
            final DataServiceEntry< ? >[] entries = dataService.getChildren( transaction );

            if ( entries.length == 0 ) {
                return bos.toByteArray();
            }

            zipStream = new ZipOutputStream(bos);

            //
            // Generate manifest for data service
            //
            DataserviceManifest manifest = new DataserviceManifest( transaction, dataService );
            byte[] manifestBytes = manifest.export( transaction, new Properties() );

            ZipEntry manZipEntry = new ZipEntry( DataserviceManifest.MANIFEST );
            zipStream.putNextEntry( manZipEntry );
            zipStream.write( manifestBytes );
            zipStream.closeEntry();

            for ( final DataServiceEntry< ? > entry : entries ) {
                final byte[] content = entry.export( transaction, new Properties() );
                final String entryName = entry.getEntryPath( transaction );
                final ZipEntry zipEntry = new ZipEntry( entryName );

                zipStream.putNextEntry( zipEntry );
                zipStream.write( content );
                zipStream.flush();
                zipStream.closeEntry();
            }

            //
            // Required to ensure the zip is not corrupt
            //
            zipStream.flush();
            zipStream.finish();
            zipStream.close();

            return bos.toByteArray();

        } catch ( Exception ex ) {
            throw new KException( ex );
        } finally {
            if ( zipStream != null ) {
                try {
                    zipStream.finish();
                } catch ( IOException e ) {
                    // nothing to do
                }

                try {
                    zipStream.close();
                } catch ( IOException e ) {
                    // nothing to do
                }
            }
        }
    }

    private void deployConnection( final UnitOfWork uow,
                                   final ConnectionEntry entry,
                                   final TeiidInstance teiidInstance,
                                   final DeployStatus status ) throws Exception {
        final Connection connection = entry.getReference( uow );
        final String connectionName = connection.getName( uow );
        status.addProgressMessage( Messages.getString( Messages.DataserviceConveyor.DATA_SERVICE_CONNECTION_START_DEPLOY,
                                                       connectionName ) );

        final String jndiName = entry.getJndiName( uow );
        final String sourceType = connection.getDriverName( uow );
        final Properties properties = connection.getPropertiesForServerDeployment( uow, teiidInstance );

        final TeiidDataSource teiidDataSrc = teiidInstance.getOrCreateDataSource( connectionName,
                                                                                  jndiName,
                                                                                  sourceType,
                                                                                  properties );
        if ( teiidDataSrc == null ) {
            String errorMsg = Messages.getString( Messages.DataserviceConveyor.DATA_SERVICE_DATA_SOURCE_FAILED_TO_DEPLOY,
                                                  connectionName );
            status.addErrorMessage( errorMsg );
        }

        status.addProgressMessage( Messages.getString( Messages.DataserviceConveyor.DATA_SERVICE_CONNECTION_SUCCESSFULLY_DEPLOYED,
                                                       connectionName ) );
    }

    private void deployDriver( final UnitOfWork uow,
                               final Driver driver,
                               final TeiidInstance teiidInstance,
                               final DeployStatus status ) throws Exception {
        final String driverName = driver.getName( uow );
        status.addProgressMessage( Messages.getString( Messages.DataserviceConveyor.DATA_SERVICE_DRIVER_START_DEPLOY,
                                                       driverName ) );

        final InputStream content = driver.getContent( uow );
        final DocumentType driverType = driver.getDocumentType( uow );
        final File driverFile = File.createTempFile( driverName, driverType.toString() );
        FileUtils.write( content, driverFile );

        teiidInstance.deployDriver( driverName, driverFile );
        status.addProgressMessage( Messages.getString( Messages.DataserviceConveyor.DATA_SERVICE_DRIVER_SUCCESSFULLY_DEPLOYED,
                                                       driverName ) );
    }

    private void deployVdb( final UnitOfWork uow,
                            final VdbEntry entry,
                            final TeiidInstance teiidInstance,
                            final DeployStatus status ) throws Exception {
        final Vdb vdb = entry.getReference( uow );
        final String vdbName = vdb.getName( uow );
        status.addProgressMessage( Messages.getString( Messages.DataserviceConveyor.DATA_SERVICE_VDB_START_DEPLOY, vdbName ) );

        // Get VDB content
        final byte[] vdbXml = vdb.export( uow, null );

        if ( ( vdbXml == null ) || ( vdbXml.length == 0 ) ) {
            String errorMsg = Messages.getString( Messages.DataserviceConveyor.DATA_SERVICE_VDB_CONTENTS_FAILURE, vdbName );
            status.addErrorMessage( errorMsg );
            return;
        }

        // Get Vdb deployment name
        String vdbDeploymentName = null;
        if ( vdb.hasProperty( uow, "deployment-name" ) ) { //$NON-NLS-1$
            vdbDeploymentName = vdb.getProperty( uow, "deployment-name" ).getStringValue( uow ); //$NON-NLS-1$
        }
        if(StringUtils.isEmpty(vdbDeploymentName)) {
        	vdbDeploymentName = vdb.getName(uow) + TeiidVdb.DYNAMIC_VDB_SUFFIX;
        }

        final InputStream stream = new ByteArrayInputStream( vdbXml );
        teiidInstance.deployDynamicVdb( vdbDeploymentName, stream );

        status.addProgressMessage( Messages.getString( Messages.DataserviceConveyor.DATA_SERVICE_VDB_SUCCESSFULLY_DEPLOYED,
                                                       vdbName ) );

        
        TeiidVdb teiidVdb = teiidInstance.getVdb( vdbDeploymentName );
        if ( teiidVdb == null ) {
            status.addProgressMessage( "Warning: Vdb " + vdbName + " not yet completed deployment" );
            return;
        }

        if ( teiidVdb.isActive() ) {
            status.addProgressMessage( "Vdb " + vdbName + " deployed to teiid and is active" );
        } else if ( teiidVdb.isLoading() ) {
            status.addProgressMessage( "Vdb " + vdbName + " deployed but still loading" );
        }

        List< String > vdbErrors = teiidVdb.getValidityErrors();
        if ( vdbErrors.isEmpty() )
            status.addProgressMessage( "Vdb " + vdbName + " deployed and is valid" );
        else
            status.addProgressMessage( "Vdb " + vdbName + " deployed but has validity errors" );

        for ( String vdbError : vdbErrors ) {
            status.addErrorMessage( vdbError );
        }
    }

    /**
     * Deploy the {@link Dataservice} to the teiid instance
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of {@link State#NOT_STARTED})
     * @param dataservice
     *        the data service to be deployed (cannot be <code>null</code>)
     * @param teiid
     *        the Teiid being deployed to (cannot be <code>null</code>)
     * @return the deployment status (never <code>null</code>)
     */
    public DeployStatus deploy(UnitOfWork transaction, Dataservice dataservice, Teiid teiid) {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull(dataservice, "dataservice"); //$NON-NLS-1$
        ArgCheck.isNotNull(teiid, "teiid"); //$NON-NLS-1$

        DeployStatus status = new DeployStatus();
        TeiidInstance teiidInstance = teiid.getTeiidInstance(transaction);

        try {
            final String dsName = dataservice.getName( transaction );
            status.addProgressMessage( Messages.getString( Messages.DataserviceConveyor.DATA_SERVICE_START_DEPLOY, dsName ) );

            // TODO deploy resources
            // TODO deploy metadata files
            // TODO deploy UDFs

            { // Deploy the drivers
                final DriverEntry[] entries = dataservice.getDriverEntries( transaction );

                if ( entries.length != 0 ) {
                    for ( final DriverEntry entry : entries ) {
                        if ( entry.getReference( transaction ) == null ) {
                            continue; // nothing to deploy
                        }

                        boolean deploy = false;

                        switch ( entry.getPublishPolicy( transaction ) ) {
                            case ALWAYS:
                                deploy = true;
                                break;
                            case IF_MISSING:
                                deploy = true; // TODO determine if not already deployed
                                break;
                            case NEVER:
                            default:
                                break;
                        }

                        if ( deploy ) {
                            final Driver driver = entry.getReference( transaction );
                            deployDriver( transaction, driver, teiidInstance, status );
                        }
                    }
                }
            }

            { // Deploy the connections
                final ConnectionEntry[] entries = dataservice.getConnectionEntries( transaction );

                if ( entries.length != 0 ) {
                    for ( final ConnectionEntry entry : entries ) {
                        if ( entry.getReference( transaction ) == null ) {
                            continue; // nothing to deploy
                        }

                        boolean deploy = false;

                        switch ( entry.getPublishPolicy( transaction ) ) {
                            case ALWAYS:
                                deploy = true;
                                break;
                            case IF_MISSING:
                                deploy = true; // TODO determine if not already deployed
                                break;
                            case NEVER:
                            default:
                                break;
                        }

                        if ( deploy ) {
                            deployConnection( transaction, entry, teiidInstance, status );
                        }
                    }
                }
            }

            { // deploy service VDB
                final Vdb serviceVdb = dataservice.getServiceVdb( transaction );

                if ( serviceVdb != null ) {
                    final ServiceVdbEntry entry = dataservice.getServiceVdbEntry( transaction );
                    boolean deploy = false;

                    switch ( entry.getPublishPolicy( transaction ) ) {
                        case ALWAYS:
                            deploy = true;
                            break;
                        case IF_MISSING:
                            deploy = true; // TODO determine if not already deployed
                            break;
                        case NEVER:
                        default:
                            break;
                    }

                    if ( deploy ) {
                        deployVdb( transaction, entry, teiidInstance, status );
                    }
                // No Service VDB - log error
                } else {
                    String errorMsg = Messages.getString(Messages.DataserviceConveyor.DATA_SERVICE_VDB_NOT_FOUND,dsName);
                    status.addErrorMessage(errorMsg);
                    return status;
                }
                
            }

            //
            // TODO need to guarantee the ordering of the vdbs
            //
            { // Deploy the VDBs
                final VdbEntry[] entries = dataservice.getVdbEntries( transaction );

                if ( entries.length != 0 ) {
                    for ( final VdbEntry entry : entries ) {
                        if ( entry.getReference( transaction ) == null ) {
                            continue; // nothing to deploy
                        }

                        boolean deploy = false;

                        switch ( entry.getPublishPolicy( transaction ) ) {
                            case ALWAYS:
                                deploy = true;
                                break;
                            case IF_MISSING:
                                deploy = true; // TODO determine if not already deployed
                                break;
                            case NEVER:
                            default:
                                break;
                        }

                        if ( deploy ) {
                            deployVdb( transaction, entry, teiidInstance, status );
                        }
                    }
                }
            }

            status.addProgressMessage( Messages.getString( Messages.DataserviceConveyor.DATA_SERVICE_SUCCESSFULLY_DEPLOYED,
                                                           dsName ) );
        } catch (Exception ex) {
            status.addErrorMessage(ex);
        }

        return status;
    }
}