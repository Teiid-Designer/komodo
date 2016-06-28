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

    /**
     * @return the repository
     */
    public Repository getRepository() {
        return this.repository;
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

    /**
     * Determines how to handle node creation if child to be created already exists.  The ExistingNodeOptions specify the behavior.
     * If no Option is set, then the default behavior is to overwrite any existing nodes.
     * @param transaction the transaction
     * @param parentObject the parent object
     * @param importOptions the import options
     * @param importMessages the import messages
     * @return 'true' to continue, 'false' to bail
     * @throws KException the exception
     */
    protected abstract boolean handleExistingNode(UnitOfWork transaction,
                                                                                              KomodoObject parentObject,
                                                                                              ImportOptions importOptions,
                                                                                              ImportMessages importMessages) throws KException;


    protected abstract void executeImport(UnitOfWork transaction,
                                                                                    String content,
                                                                                    KomodoObject parentObject,
                                                                                    ImportOptions importOptions,
                                                                                    ImportMessages importMessages) throws KException;

    protected void doImport(UnitOfWork transaction, String content, KomodoObject parentObject, ImportOptions importOptions, ImportMessages importMessages) throws Exception {

        if(StringUtils.isEmpty(content)) {
            importMessages.addErrorMessage(Messages.getString(IMPORTER.errorEmptyMsg));
            return;
        }

        ArgCheck.isNotNull(importType);

        // --------------------------------------------------------------
        // Determine whether to continue, based on ImportOptions...
        // --------------------------------------------------------------
        boolean doImport = handleExistingNode(transaction, parentObject, importOptions, importMessages);
        if (! doImport) {
            // Handling existing node advises not to continue
            return;
        }

        // --------------------------------------------------------------
        // Execute the import
        // --------------------------------------------------------------
        executeImport(transaction, content, parentObject, importOptions, importMessages);
    }
    
    protected String determineNewName(UnitOfWork transaction, String nodeName) throws KException {
        KomodoObject workspace = getWorkspace(transaction);
        for (int i = 0; i < 1000; ++i) {
            String newName = nodeName + UNDERSCORE + i;
            if (! workspace.hasChild(transaction, newName))
                return newName;
        }

        throw new KException(Messages.getString(Messages.IMPORTER.newNameFailure, nodeName));
    }
}
