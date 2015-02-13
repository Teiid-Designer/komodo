/*************************************************************************************
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership. Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 ************************************************************************************/
package org.komodo.importer.ddl;

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
 * Importer implementation for importing from DDL Schema.
 */
public class DdlImporter extends AbstractImporter {

    /**
     * constructor
     *
     * @param repository repository into which ddl should be imported
     *
     * @param transaction the transaction used for importing the DDL schema
     */
    public DdlImporter(Repository repository, UnitOfWork transaction) {
        super(repository, transaction);
    }

    /**
     * constructor
     *
     * @param repository repository into which ddl should be imported
     *
     */
    public DdlImporter(Repository repository) {
        this(repository, null);
    }

    /**
     * Perform the model import using the specified DDL File.  The DDL constructs must be valid to put directly beneath a model.
     * @param ddlFile the DDL file
     * @param importOptions the options for the import
     * @param importMessages the messages recorded during the import
     * @return newly created root ddl node
     */
    public KomodoObject importDdl(File ddlFile, ImportOptions importOptions, ImportMessages importMessages) {
        KomodoObject ko = null;

        if (!validFile(ddlFile, importMessages))
            return ko;

        try {
            ko = prepareImport(toString(ddlFile), importOptions, importMessages);
        } catch (Exception ex) {
            importMessages.addErrorMessage(ex.getLocalizedMessage());
        }

        return ko;
    }

    /**
     * Perform the model import using the specified DDL Stream.  The DDL constructs must be valid to put directly beneath a model.
     * @param ddlStream the DDL input stream
     * @param importOptions the options for the import
     * @param importMessages the messages recorded during the import
     * @return newly created root ddl node
     */
    public KomodoObject importDdl(InputStream ddlStream, ImportOptions importOptions, ImportMessages importMessages) {
        ArgCheck.isNotNull(ddlStream);

        KomodoObject ko = null;
        try {
            ko = prepareImport(toString(ddlStream), importOptions, importMessages);
        } catch (Exception ex) {
            importMessages.addErrorMessage(ex.getLocalizedMessage());
        }

        return ko;
    }

    /**
     * Perform the model import using the specified DDL.  The DDL constructs must be valid to put directly beneath a model.
     * @param ddl the DDL
     * @param importOptions the options for the import
     * @param importMessages the messages recorded during the import
     * @return newly created root ddl node
     */
    public KomodoObject importDdl(String ddl, ImportOptions importOptions, ImportMessages importMessages) {
        KomodoObject ko = null;
        try {
            ko = prepareImport(ddl, importOptions, importMessages);
        } catch (Exception ex) {
            importMessages.addErrorMessage(ex.getLocalizedMessage());
        }

        return ko;
    }
}
