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
import java.util.Enumeration;
import java.util.Properties;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.ExistingNodeOptions;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.importer.Messages;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.importer.ddl.DdlImporter;
import org.komodo.relational.importer.vdb.VdbImporter;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.DocumentType;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
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

        throw new KException(Messages.getString(Messages.IMPORTER.newNameFailure, nodeName));
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
                importMessages.addErrorMessage(Messages.getString(Messages.IMPORTER.nodeExistsReturn));
                return false;
            // CREATE_NEW - Return 'true' - will create a new data service with new unique name.  Log a progress message.
            case CREATE_NEW:
                String newName = determineNewName(transaction, parent, dsName);
                importMessages.addProgressMessage(Messages.getString(Messages.IMPORTER.nodeExistCreateNew, dsName, newName));
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

    public void dsImport(UnitOfWork transaction, InputStream srcStream, KomodoObject parent, ImportOptions importOptions,
                         ImportMessages importMessages) throws KException {
        ArgCheck.isNotNull(srcStream, "Source Stream");

        long timestamp = System.currentTimeMillis();
        File zFile = new File(FileUtils.tempDirectory(), timestamp + DOT + ZIP);
        ZipFile zipFile = null;

        try {
            FileUtils.write(srcStream, zFile);
            overrideName(zFile, importOptions);

            String dsName = importOptions.getOption(OptionKeys.NAME).toString();
            if (dsName == null)
                throw new Exception(Messages.getString(Messages.IMPORTER.noNameFailure));

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
                    } else if (name.endsWith(DocumentType.XML.toString())) {
                        VdbImporter importer = new VdbImporter(repository);
                        ImportOptions options = new ImportOptions();
                        importer.importVdb(transaction, entryStream, dataservice, options, importMessages);
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

    public byte[] dsExport(UnitOfWork transaction, DataserviceImpl dataService, Properties exportProperties) throws KException {
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
            DataserviceManifest manifest = dataService.createManifest(transaction, new Properties());
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
                String ext = exportable.getDocumentType(transaction).toString();
                byte[] content = exportable.export(transaction, new Properties());

                String entryName = name + DOT + ext;
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
}
