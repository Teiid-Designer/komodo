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
package org.komodo.importer.vdb;

import java.io.File;
import java.io.InputStream;
import org.komodo.importer.AbstractImporter;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;

/**
 *
 */
public class VdbImporter extends AbstractImporter {

    /**
     * constructor
     *
     * @param repository repository into which ddl should be imported
     *
     * @param transaction the transaction used for importing the DDL schema 
     */
    public VdbImporter(Repository repository, UnitOfWork transaction) {
        super(repository, transaction);
    }

    /**
     * constructor
     *
     * @param repository repository into which ddl should be imported
     * 
     */
    public VdbImporter(Repository repository) {
        super(repository, null);
    }

    /**
     * Perform the vdb import using the specified xml Stream.
     *
     * @param vdbStream the vdb xml input stream
     * @param importOptions the options for the import
     * @param importMessages the messages recorded during the import
     * @return newly created root vdb node
     */
    public KomodoObject importVdb(InputStream vdbStream, ImportOptions importOptions, ImportMessages importMessages) {
        ArgCheck.isNotNull(vdbStream);

        KomodoObject ko = null;
        try {
            ko = prepareImport(toString(vdbStream), importOptions, importMessages);
        } catch (Exception ex) {
            importMessages.addErrorMessage(ex.getLocalizedMessage());
        }

        return ko;
    }

    /**
     * Perform the vdb import using the specified vdb xml File.
     *
     * @param vdbXmlFile the vdb xml file
     * @param importOptions the options for the import
     * @param importMessages the messages recorded during the import
     * @return newly created root vdb node 
     */
    public KomodoObject importVdb(File vdbXmlFile, ImportOptions importOptions, ImportMessages importMessages) {
        KomodoObject ko = null;

        if (!validFile(vdbXmlFile, importMessages))
            return ko;

        try {
            ko = prepareImport(toString(vdbXmlFile), importOptions, importMessages);
        } catch (Exception ex) {
            importMessages.addErrorMessage(ex.getLocalizedMessage());
        }

        return ko;
    }
}
