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
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;
import org.apache.commons.io.IOUtils;
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

    protected String determineNewName(UnitOfWork transaction,
                                      KomodoObject parent, String nodeName) throws KException {
        for (int i = 0; i < 1000; ++i) {
            String newName = nodeName + UNDERSCORE + i;
            if (! parent.hasChild(transaction, newName))
                return newName;
        }

        throw new KException(Messages.getString(Messages.IMPORTER.newNameFailure, nodeName));
    }

    protected boolean handleExistingNode(UnitOfWork transaction, KomodoObject parent,
                                         ImportOptions importOptions, ImportMessages importMessages)
        throws KException {

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

    public void dsImport(UnitOfWork transaction, InputStream srcStream,
                         KomodoObject parent, ImportOptions importOptions,
                         ImportMessages importMessages) throws KException {
        ArgCheck.isNotNull(srcStream, "Source Stream");

        boolean doImport = handleExistingNode(transaction, parent, importOptions, importMessages);
        if (! doImport) {
            // Handling existing node advises not to continue
            return;
        }

        ZipInputStream zipStream = null;
        try {
            zipStream = new ZipInputStream(srcStream);
            if (zipStream.available() == 0)
                return;

            String dsName = importOptions.getOption(OptionKeys.NAME).toString();
            WorkspaceManager mgr = getWorkspaceManager();
            Dataservice dataservice = mgr.createDataservice(transaction, parent, dsName);

            ZipEntry entry;
            while ((entry = zipStream.getNextEntry()) != null) {
                ByteArrayOutputStream bos = null;

                try {
                    String name = entry.getName();

                    bos = new ByteArrayOutputStream();
                    final byte[] buf = new byte[BUFFER_SIZE];
                    int length;

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

                    zipStream.closeEntry();
                }
            }

        } catch (Exception ex) {
            throw new KException(ex);
        } finally {
            if (zipStream != null)
                IOUtils.closeQuietly(zipStream);
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
                String ext = exportable.getDocumentType().toString();
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
                } catch (IOException e) {}

                IOUtils.closeQuietly(zipStream);
            }

            if (bos != null)
                IOUtils.closeQuietly(bos);
        }
    }
}
