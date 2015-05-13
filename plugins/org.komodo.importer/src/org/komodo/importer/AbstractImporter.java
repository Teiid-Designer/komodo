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
package org.komodo.importer;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileReader;
import java.io.InputStream;
import java.util.concurrent.TimeUnit;
import org.komodo.importer.ImportOptions.ExistingNodeOptions;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.importer.Messages.IMPORTER;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.repository.SynchronousCallback;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.FileUtils;
import org.komodo.utils.StringUtils;

/**
 *
 */
public abstract class AbstractImporter implements StringConstants {

    private static final String EXISTING_TRANSACTION_NAME = "handle-existing-object"; //$NON-NLS-1$

    private static final String IMPORT_TRANSACTION_NAME = "import-object"; //$NON-NLS-1$

    protected static final String OLD = HYPHEN + "OLD"; //$NON-NLS-1$

    private Repository repository;

    protected ImportType importType;

    /**
     * @param repository the repository
     * @param importType the import type
     */
    public AbstractImporter(Repository repository, ImportType importType) {
        this.repository = repository;
        this.importType = importType;
    }

    protected KomodoObject getWorkspace(UnitOfWork transaction) throws KException {
        ArgCheck.isNotNull(transaction);

        return getRepository().komodoWorkspace(transaction);
    }

    protected WorkspaceManager getWorkspaceManager() throws KException {
        return WorkspaceManager.getInstance(getRepository());
    }

    /**
     * @return the repository
     */
    public Repository getRepository() {
        return this.repository;
    }

    /*
     * Commit the transaction
     */
    protected void commitTransaction(UnitOfWork transaction) throws Exception {
        if (transaction == null)
            return;

        transaction.commit();
    }

    protected boolean validFile(File file, ImportMessages importMessages) {
        if (!file.exists() || file.isDirectory()) {
            importMessages.addErrorMessage(Messages.getString(IMPORTER.errorFileNotFoundMsg, file.getName()));
            return false;
        }
        if (!file.canRead()) {
            importMessages.addErrorMessage(Messages.getString(IMPORTER.errorFileNotReadableMsg, file.getName()));
            return false;
        }

        return true;
    }

    protected String toString(File aFile) throws Exception {
        StringBuilder builder;
        FileReader fileReader = null;
        try {
            fileReader = new FileReader(aFile);

            // Read the file contents
            char[] buf = new char[FileUtils.DEFAULT_BUFFER_SIZE];
            builder = new StringBuilder();
            for (int charTot = fileReader.read(buf); charTot >= 0; charTot = fileReader.read(buf))
                builder.append(buf, 0, charTot);

            return builder.toString();
        } finally {
            if (fileReader != null)
                fileReader.close();
        }
    }

    protected String toString(InputStream inputStream) throws Exception {
        BufferedInputStream bis = new BufferedInputStream(inputStream);
        ByteArrayOutputStream buf = new ByteArrayOutputStream();
        int result = bis.read();
        while (result != -1) {
            byte b = (byte)result;
            buf.write(b);
            result = bis.read();
        }

        return buf.toString();
    }

    protected boolean handleExistingNode(UnitOfWork transaction,
                                                                      ImportOptions importOptions,
                                                                      ImportMessages importMessages) throws KException {
        String nodeName = importOptions.getOption(OptionKeys.NAME).toString();
        ExistingNodeOptions exNodeOption = (ExistingNodeOptions)importOptions.getOption(OptionKeys.HANDLE_EXISTING);

        if (! getWorkspace(transaction).hasChild(transaction, nodeName))
            return true;

        switch (exNodeOption) {
            case RETURN:
                importMessages.addErrorMessage(Messages.getString(Messages.IMPORTER.nodeExistsReturn));
                return false;
            case CREATE_NEW:
                String newName = determineNewName(nodeName);
                importMessages.addProgressMessage(Messages.getString(Messages.IMPORTER.nodeExistCreateNew, nodeName, newName));
                importOptions.setOption(OptionKeys.NAME, newName);
                break;
            case OVERWRITE:
                KomodoObject oldNode = getWorkspace(transaction).getChild(transaction, nodeName);
                //
                // Have to use a different transaction at this point
                // due to MODE-2463. Does mean that we cannot
                // rollback the transaction until this is fixed
                //
                oldNode.remove(transaction);
        }

        return true;
    }

    protected abstract KomodoObject executeImport(UnitOfWork transaction,
                                                                                    String content,
                                                                                    ImportOptions importOptions,
                                                                                    ImportMessages importMessages) throws KException;

    protected KomodoObject prepareImport(String content, ImportOptions importOptions, ImportMessages importMessages) throws Exception {

        if(StringUtils.isEmpty(content)) {
            importMessages.addErrorMessage(Messages.getString(IMPORTER.errorEmptyMsg));
            return null;
        }

        ArgCheck.isNotNull(importType);

        SynchronousCallback callback = new SynchronousCallback();
        UnitOfWork transaction = getRepository().createTransaction(EXISTING_TRANSACTION_NAME, false, callback);

        boolean doImport = handleExistingNode(transaction, importOptions, importMessages);

        //
        // Commit the operations performed in handling existing node
        //
        transaction.commit();

        //
        // Wait for the sequencers to do something if anything
        //
        callback.await(3, TimeUnit.MINUTES);

        if (! doImport) {
            // Handling existing node advises not to continue
            return null;
        }

        //
        // Create object in workspace
        //
        callback = new SynchronousCallback();
        transaction = getRepository().createTransaction(IMPORT_TRANSACTION_NAME, false, callback);

        KomodoObject resultNode = executeImport(transaction, content, importOptions, importMessages);

        //
        // Commit the operations performed in handling existing node
        //
        transaction.commit();

        //
        // Wait for the sequencers to do something if anything
        //
        callback.await(3, TimeUnit.MINUTES);

        return resultNode;
    }

    protected String determineNewName(String nodeName) throws KException {
        KomodoObject workspace = getWorkspace(null);
        for (int i = 0; i < 1000; ++i) {
            String newName = nodeName + UNDERSCORE + i;
            if (! workspace.hasChild(null, newName))
                return newName;
        }

        throw new KException(Messages.getString(Messages.IMPORTER.newNameFailure, nodeName));
    }
}
