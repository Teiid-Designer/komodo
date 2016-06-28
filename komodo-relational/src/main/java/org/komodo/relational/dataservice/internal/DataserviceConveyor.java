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
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;
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
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.dataservice.DataserviceManifest;
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.driver.Driver;
import org.komodo.relational.importer.ddl.DdlImporter;
import org.komodo.relational.importer.dsource.DatasourceImporter;
import org.komodo.relational.importer.vdb.VdbImporter;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.DocumentType;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.DataSourceDriver;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.spi.utils.KeyInValueHashMap;
import org.komodo.spi.utils.KeyInValueHashMap.KeyFromValueAdapter;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.FileUtils;

public class DataserviceConveyor implements StringConstants {

    /**
     * Buffer size for the byte arrays
     */
    public static final int BUFFER_SIZE = 8192;

    private final Repository repository;

    public DataserviceConveyor(Repository repository) {
        this.repository = repository;
    }

    protected WorkspaceManager getWorkspaceManager() throws KException {
        return WorkspaceManager.getInstance(repository);
    }

    protected String determineNewName(UnitOfWork transaction, KomodoObject parent, String nodeName) throws KException {
        for (int i = 0; i < 1000; ++i) {
            String newName = nodeName + UNDERSCORE + i;
            if (!parent.hasChild(transaction, newName))
                return newName;
        }

        throw new KException(org.komodo.importer.Messages.getString(
                                                                    org.komodo.importer.Messages.IMPORTER.newNameFailure, nodeName));
    }

    protected boolean handleExistingNode(UnitOfWork transaction, KomodoObject parent, ImportOptions importOptions,
                                         ImportMessages importMessages) throws KException {

        // dataservice name to create
        String dsName = importOptions.getOption(OptionKeys.NAME).toString();

        // No node with the requested name - ok to create
        if (!parent.hasChild(transaction, dsName))
            return true;

        // Option specifying how to handle when node exists with requested name
        ExistingNodeOptions exNodeOption = (ExistingNodeOptions)importOptions.getOption(OptionKeys.HANDLE_EXISTING);

        switch (exNodeOption) {
            // RETURN - Return 'false' - do not create a node.  Log an error message
            case RETURN:
                importMessages.addErrorMessage(org.komodo.importer.Messages.getString(
                                                                                      org.komodo.importer.Messages.IMPORTER.nodeExistsReturn));
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
                    if (! DataserviceManifest.MANIFEST.equals(name)) {
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

                    return DataserviceManifest.extractName(entryStream);

                } finally {
                    if (bos != null)
                        bos.close();

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

        long timestamp = System.currentTimeMillis();
        File zFile = new File(FileUtils.tempDirectory(), timestamp + DOT + ZIP);
        ZipFile zipFile = null;

        try {
            FileUtils.write(srcStream, zFile);
            overrideName(zFile, importOptions);

            String dsName = importOptions.getOption(OptionKeys.NAME).toString();
            if (dsName == null)
                throw new Exception(org.komodo.importer.Messages.getString(
                                                                           org.komodo.importer.Messages.IMPORTER.noNameFailure));

            boolean doImport = handleExistingNode(transaction, parent, importOptions, importMessages);
            if (!doImport) {
                // Handling existing node advises not to continue
                return;
            }

            zipFile = new ZipFile(zFile);
            Enumeration<? extends ZipEntry> entries = zipFile.entries();
            if (!entries.hasMoreElements())
                return;

            WorkspaceManager mgr = getWorkspaceManager();
            Dataservice dataservice = mgr.createDataservice(transaction, parent, dsName);

            while (entries.hasMoreElements()) {
                ZipEntry entry = entries.nextElement();
                ByteArrayOutputStream bos = null;
                InputStream zipStream = null;

                try {
                    String name = entry.getName();

                    bos = new ByteArrayOutputStream();
                    final byte[] buf = new byte[BUFFER_SIZE];
                    int length;

                    zipStream = zipFile.getInputStream(entry);
                    while ((length = zipStream.read(buf, 0, buf.length)) >= 0) {
                        bos.write(buf, 0, length);
                    }

                    byte[] content = bos.toByteArray();
                    ByteArrayInputStream entryStream = new ByteArrayInputStream(content);

                    if (DataserviceManifest.MANIFEST.equals(name)) {
                        DataserviceManifest manifest = new DataserviceManifest(transaction, dataservice);
                        manifest.read(transaction, entryStream);
                    } else if (name.endsWith(DocumentType.VDB_XML.toString())) {
                        VdbImporter importer = new VdbImporter(repository);
                        ImportOptions options = new ImportOptions();
                        importer.importVdb(transaction, entryStream, dataservice, options, importMessages);
                    } else if (name.endsWith(DocumentType.TDS.toString())) {
                        DatasourceImporter importer = new DatasourceImporter(repository);
                        ImportOptions options = new ImportOptions();
                        importer.importDS(transaction, entryStream, dataservice, options, importMessages);
                    } else if (name.endsWith(DocumentType.DDL.toString())) {
                        DdlImporter importer = new DdlImporter(repository);
                        ImportOptions options = new ImportOptions();
                        importer.importDdl(transaction, entryStream, dataservice, options, importMessages);
                    }

                } finally {
                    if (bos != null)
                        bos.close();

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

            zFile.delete();
        }
    }

    /**
     * Export the given data service
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param dataService the data service to export
     * @param exportProperties export properties to be used during the export procedure
     * @return the array of bytes of the exported data service
     * @throws KException if errors occurs
     */
    public byte[] dsExport(UnitOfWork transaction, Dataservice dataService, Properties exportProperties) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        ZipOutputStream zipStream = null;

        try {
            KomodoObject[] children = dataService.getChildren(transaction);
            if (children == null) {
                return bos.toByteArray();
            }

            WorkspaceManager mgr = WorkspaceManager.getInstance(dataService.getRepository());
            zipStream = new ZipOutputStream(bos);

            //
            // Generate manifest for data service
            //
            DataserviceManifest manifest = new DataserviceManifest(transaction, dataService);
            byte[] manifestBytes = manifest.export(transaction, new Properties());

            ZipEntry manZipEntry = new ZipEntry(DataserviceManifest.MANIFEST);
            zipStream.putNextEntry(manZipEntry);
            zipStream.write(manifestBytes);
            zipStream.closeEntry();

            //
            // Add all exportable children
            //
            for (KomodoObject child : children) {
                Exportable exportable = mgr.resolve(transaction, child, Exportable.class);
                if (exportable == null)
                    continue;

                String name = exportable.getName(transaction);
                byte[] content = exportable.export(transaction, new Properties());

                String entryName = exportable.getDocumentType(transaction).fileName(name);
                ZipEntry zipEntry = new ZipEntry(entryName);

                zipStream.putNextEntry(zipEntry);
                zipStream.write(content);
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

        } catch (Exception ex) {
            throw new KException(ex);
        } finally {
            if (zipStream != null) {
                try {
                    zipStream.finish();
                } catch (IOException e) {
                }

                try {
                    zipStream.close();
                } catch (IOException e) {
                }
            }

            if (bos != null) {
                try {
                    bos.close();
                } catch (IOException e) {
                }
            }
        }
    }

    /**
     * Index the given array of objects by name
     *
     * @param transaction the transaction required to obtain the name of the object
     * @param objects the objects to be indexed
     * @return a map of the indexed objects
     */
    private <T extends KomodoObject> Map<String, T> index(UnitOfWork transaction, T[] objects) {
        if (objects == null)
            return Collections.emptyMap();

        KeyFromValueAdapter<String, T> adapter = new KeyFromValueAdapter<String, T>() {
            @Override
            public String getKey(T value) {
                try {
                    return value.getName(transaction);
                } catch (Exception e) {
                    throw new IllegalStateException(e);
                }
            }
        };

        KeyInValueHashMap<String, T> index = new KeyInValueHashMap<>(adapter);

        for (T object : objects) {
            index.add(object);
        }

        return index;
    }

    /**
     * Deploy the {@link Dataservice} to the teiid instance
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param dataservice the data service to be deployed
     */
    public DeployStatus deploy(UnitOfWork transaction, Dataservice dataservice, Teiid teiid) {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull(dataservice, "data service");
        ArgCheck.isNotNull(teiid, "teiid");

        DeployStatus status = new DeployStatus();
        TeiidInstance teiidInstance = teiid.getTeiidInstance(transaction);

        try {
            String dsName = dataservice.getName(transaction);

            Map<String, Driver> driverIndex = index(transaction, dataservice.getDrivers(transaction));
            Map<String, Datasource> dataSrcIndex = index(transaction, dataservice.getDataSources(transaction));
            Map<String, Vdb> vdbIndex = index(transaction, dataservice.getVdbs(transaction));

            //
            // Deploy the drivers
            //
            String[] driverPlan = dataservice.getDriverPlan(transaction);
            for (String driverName : driverPlan) {
                Driver driver = driverIndex.get(driverName);
                if (driver == null) {
                    String errorMsg = Messages.getString(Messages.DataserviceConveyor.DATA_SERVICE_DRIVER_NOT_FOUND,
                                                         driverName,
                                                         dsName);
                    status.addErrorMessage(errorMsg);
                    return status;
                }

                InputStream content = driver.getContent(transaction);
                DocumentType driverType = driver.getDocumentType(transaction);
                File driverFile = File.createTempFile(driverName, driverType.toString());
                FileUtils.write(content, driverFile);
                teiidInstance.deployDriver(driverName, driverFile);

                DataSourceDriver theDsDriver = null;
                Collection<DataSourceDriver> drivers = teiidInstance.getDataSourceDrivers();
                for (DataSourceDriver dsDriver : drivers) {
                    //
                    // Could be more than 1 driver deployed, eg. mysql jar provides 2
                    //
                    if (dsDriver.getName().startsWith(driverName)) {
                        theDsDriver = dsDriver;
                        break;
                    }
                }

                if (theDsDriver == null) {
                    String errorMsg = Messages.getString(Messages.DataserviceConveyor.DATA_SERVICE_DRIVER_FAILED_TO_DEPLOY,
                                                         driverName,
                                                         dsName);
                    status.addErrorMessage(errorMsg);
                    return status;
                }
            }

            //
            // Deploy the data sources
            //
            String[] dataSrcPlan = dataservice.getDataSourcePlan(transaction);
            for (String dataSrcName : dataSrcPlan) {
                Datasource dataSrc = dataSrcIndex.get(dataSrcName);
                if (dataSrc == null) {
                    String errorMsg = Messages.getString(Messages.DataserviceConveyor.DATA_SERVICE_DATA_SOURCE_NOT_FOUND,
                                                         dataSrcName,
                                                         dsName);
                    status.addErrorMessage(errorMsg);
                    return status;
                }

                String jndiName = dataSrc.getJndiName(transaction);
                String sourceType = dataSrc.getDriverName(transaction);
                Properties properties = dataSrc.getPropertiesForServerDeployment(transaction, teiidInstance);

                TeiidDataSource teiidDataSrc = teiidInstance.getOrCreateDataSource(dataSrcName,
                                                                                       jndiName,
                                                                                       sourceType,
                                                                                       properties);
                if (teiidDataSrc == null) {
                    String errorMsg = Messages.getString(Messages.DataserviceConveyor.DATA_SERVICE_DATA_SOURCE_FAILED_TO_DEPLOY,
                                                         dataSrcName);
                    status.addErrorMessage(errorMsg);
                    return status;
                }
            }

            //
            // Deploy the vdbs
            //
            String[] vdbPlan = dataservice.getVdbPlan(transaction);
            for (String vdbName : vdbPlan) {
                Vdb vdb = vdbIndex.get(vdbName);
                if (vdb == null) {
                    String errorMsg = Messages.getString(Messages.DataserviceConveyor.DATA_SERVICE_VDB_NOT_FOUND,
                                                         vdbName,
                                                         dsName);
                    status.addErrorMessage(errorMsg);
                    return status;
                }

                // Get VDB content
                byte[] vdbXml = vdb.export(transaction, null);
                if (vdbXml == null || vdbXml.length == 0) {
                    String errorMsg = Messages.getString(Messages.DataserviceConveyor.DATA_SERVICE_VDB_CONTENTS_FAILURE,
                                                         vdbName,
                                                         dsName);
                    status.addErrorMessage(errorMsg);
                    return status;
                }

                String vdbDeploymentName = vdbName + VDB_DEPLOYMENT_SUFFIX;
                InputStream stream = new ByteArrayInputStream(vdbXml);
                teiidInstance.deployDynamicVdb(vdbName, stream);

                TeiidVdb teiidVdb = teiidInstance.getVdb(vdbDeploymentName);
                List<String> vdbErrors = teiidVdb.getValidityErrors();
                for (String vdbError : vdbErrors) {
                    status.addErrorMessage(vdbError);
                }

                if (!vdbErrors.isEmpty())
                    return status;
            }

        } catch (Exception ex) {
            status.addErrorMessage(ex);
        }

        return status;
    }
}
