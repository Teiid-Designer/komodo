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
import org.komodo.importer.Messages.IMPORTER;
import org.komodo.relational.workspace.WorkspaceManager;
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

    private static final String TRANSACTION_NAME = "import-object"; //$NON-NLS-1$

    private Repository repository;

    private UnitOfWork transaction;

    protected ImportType importType;

    //
    // Defines whether we own the transaction and can commit
    // at the end of the import
    //
    private boolean ownTransaction = true;

    /**
     * @param repository the repository
     * @param importType the import type
     * @param transaction the transaction
     */
    public AbstractImporter(Repository repository, ImportType importType, UnitOfWork transaction) {
        this.repository = repository;
        this.importType = importType;
        if (transaction != null) {
            this.transaction = transaction;
            ownTransaction = false;
        }
    }

    protected KomodoObject getWorkspace(UnitOfWork transaction) throws KException {
        ArgCheck.isNotNull(transaction);

        return getRepository().komodoWorkspace(transaction);
    }

    protected WorkspaceManager getWorkspaceManager() {
        return WorkspaceManager.getInstance(getRepository());
    }

    /**
     * @return the repository
     */
    public Repository getRepository() {
        return this.repository;
    }

    /**
     * @return the transaction
     * @throws KException if error occurs
     */
    protected UnitOfWork getTransaction() throws KException {
        if (transaction == null) {
            transaction = repository.createTransaction(TRANSACTION_NAME, false, null);
        }

        return this.transaction;
    }

    /*
     * Only commit the transaction if it was my transaction to being with
     */
    protected void commitTransaction() throws Exception {
        if (ownTransaction && transaction != null)
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

    protected abstract KomodoObject executeImport(String content, ImportOptions importOptions, UnitOfWork transaction) throws KException;

    protected KomodoObject prepareImport(String content, ImportOptions importOptions, ImportMessages importMessages) throws Exception {

        if(StringUtils.isEmpty(content)) {
            importMessages.addErrorMessage(Messages.getString(IMPORTER.errorEmptyMsg));
            return null;
        }

        ArgCheck.isNotNull(importType);

        /*
         * Create object in workspace
         */
        UnitOfWork transaction = getTransaction();
        KomodoObject resultNode = null;

        resultNode = executeImport(content, importOptions, transaction);

        commitTransaction();

        //
        // Once committed the sequencers should take over and sequence the file
        //

        return resultNode;
    }
}
