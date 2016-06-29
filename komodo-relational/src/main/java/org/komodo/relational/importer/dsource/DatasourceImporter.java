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
package org.komodo.relational.importer.dsource;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.EnumSet;
import org.komodo.importer.AbstractImporter;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.ExistingNodeOptions;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.importer.ImportType;
import org.komodo.importer.Messages;
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.datasource.internal.DatasourceParser;
import org.komodo.relational.datasource.internal.DatasourceParser.ParserOption;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;

public class DatasourceImporter extends AbstractImporter {

    public DatasourceImporter(Repository repository) {
        super(repository, ImportType.DS);
    }

    @Override
    protected boolean handleExistingNode(UnitOfWork transaction,
                                                                             KomodoObject parentObject,
                                                                              ImportOptions importOptions,
                                                                              ImportMessages importMessages) throws KException {
     // VDB name to create
        String dsrcName = importOptions.getOption(OptionKeys.NAME).toString();

        // No node with the requested name - ok to create
        if (! parentObject.hasChild(transaction, dsrcName))
            return true;

        // Option specifying how to handle when node exists with requested name
        ExistingNodeOptions exNodeOption = (ExistingNodeOptions)importOptions.getOption(OptionKeys.HANDLE_EXISTING);

        switch (exNodeOption) {
        // RETURN - Return 'false' - do not create a node.  Log an error message
        case RETURN:
            importMessages.addErrorMessage(Messages.getString(Messages.IMPORTER.nodeExistsReturn));
            return false;
        // CREATE_NEW - Return 'true' - will create a new VDB with new unique name.  Log a progress message.
        case CREATE_NEW:
            String newName = determineNewName(transaction, dsrcName);
            importMessages.addProgressMessage(Messages.getString(Messages.IMPORTER.nodeExistCreateNew, dsrcName, newName));
            importOptions.setOption(OptionKeys.NAME, newName);
            break;
        // OVERWRITE - Return 'true' - deletes the existing VDB so that new one can replace existing.
        case OVERWRITE:
            KomodoObject oldNode = parentObject.getChild(transaction, dsrcName);
            oldNode.remove(transaction);
        }

        return true;
    }

    @Override
    protected void executeImport(UnitOfWork transaction,
                                                             String content,
                                                             KomodoObject parentObject,
                                                             ImportOptions importOptions,
                                                             ImportMessages importMessages) throws KException {

        DatasourceParser parser = new DatasourceParser();
        EnumSet<ParserOption> options = EnumSet.allOf(ParserOption.class);

        ByteArrayInputStream contentStream = new ByteArrayInputStream(content.getBytes());

        Datasource[] dataSources = parser.parse(transaction, parentObject, contentStream, options);
        if (dataSources != null && dataSources.length > 0) {
            importMessages.addProgressMessage(org.komodo.importer.Messages.getString(
                                                                                     org.komodo.importer.Messages.IMPORTER.dataSourceImported));
        }
    }

    /**
     * @param dsStream the ds input stream
     * @return the name of the data source specified in the xml
     */
    public static String extractDsName(InputStream dsStream) {
        if (dsStream == null)
            return null;

        try {
            DatasourceParser parser = new DatasourceParser();
            String[] dsNames = parser.validate(dsStream);
            if (dsNames.length == 0)
                return null;

            return dsNames[0];

        } catch (Exception ex) {
            // Don't need to worry about the exception
            return null;
        }
    }

    /**
     * Extracts the name attribute from the ds xml file and sets it into
     * the import options to synchronise the imported node name with
     * the vdb:name property.
     *
     * @param dsStream
     * @param importOptions
     * @throws Exception
     */
    private void overrideName(InputStream dsStream, ImportOptions importOptions) throws Exception {
        String dsName = extractDsName(dsStream);
        if (dsName == null)
            return;

        importOptions.setOption(OptionKeys.NAME, dsName);
    }

    /**
     * Perform the data source import using the specified xml Stream.
     *
     * @param uow the transaction
     * @param stream the data source xml input stream
     * @param parentObject the parent object in which to place the vdb
     * @param importOptions the options for the import
     * @param importMessages the messages recorded during the import
     */
    public void importDS(UnitOfWork transaction,
                                             InputStream stream,
                                             KomodoObject parentObject,
                                             ImportOptions importOptions,
                                             ImportMessages importMessages) {
        ArgCheck.isNotNull(stream);

        try {
            String dsXml = toString(stream);
            ByteArrayInputStream vdbNameStream = new ByteArrayInputStream(dsXml.getBytes("UTF-8")); //$NON-NLS-1$
            overrideName(vdbNameStream, importOptions);

            doImport(transaction, dsXml, parentObject, importOptions, importMessages);
        } catch (Exception ex) {
            importMessages.addErrorMessage(ex.getLocalizedMessage());
        }
    }

    /**
     * Perform the data source import using the specified ds xml File.
     *
     * @param uow the transaction
     * @param dsXmlFile the ds xml file
     * @param parentObject the parent object in which to place the ds
     * @param importOptions the options for the import
     * @param importMessages the messages recorded during the import
     */
    public void importDS(UnitOfWork uow, File dsXmlFile,
                                              KomodoObject parentObject, ImportOptions importOptions,
                                              ImportMessages importMessages) {
        if (!validFile(dsXmlFile, importMessages))
            return;

        try {
            overrideName(new FileInputStream(dsXmlFile), importOptions);

            doImport(uow, toString(dsXmlFile), parentObject, importOptions, importMessages);
        } catch (Exception ex) {
            importMessages.addErrorMessage(ex.getLocalizedMessage());
        }
    }
}
