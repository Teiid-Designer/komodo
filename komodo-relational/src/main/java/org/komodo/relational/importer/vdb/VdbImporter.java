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
package org.komodo.relational.importer.vdb;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.komodo.importer.AbstractImporter;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.ExistingNodeOptions;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.importer.ImportType;
import org.komodo.importer.Messages;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.modeshape.jcr.JcrLexicon;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 *
 */
public class VdbImporter extends AbstractImporter {

    /**
     * constructor
     *
     * @param repository repository into which ddl should be imported
     *
     */
    public VdbImporter(Repository repository) {
        super(repository, ImportType.VDB);
    }

    @Override
    protected void executeImport(UnitOfWork transaction,
                                                                     String content,
                                                                     KomodoObject parentObject,
                                                                     ImportOptions importOptions,
                                                                     ImportMessages importMessages) throws KException {

        String vdbName = importOptions.getOption(OptionKeys.NAME).toString();
        String vdbFilePath = importOptions.getOption(OptionKeys.VDB_FILE_PATH).toString();

        Vdb vdb = getWorkspaceManager(transaction).createVdb(transaction, parentObject, vdbName, vdbFilePath);
        KomodoObject fileNode = vdb.addChild(transaction, JcrLexicon.CONTENT.getString(), null);
        fileNode.setProperty(transaction, JcrLexicon.DATA.getString(), content);
    }

    protected WorkspaceManager getWorkspaceManager(UnitOfWork transaction) throws KException {
        return WorkspaceManager.getInstance( getRepository(), transaction );
    }

    @Override
    protected boolean handleExistingNode(UnitOfWork transaction,
                                                                             KomodoObject parentObject,
                                                                             ImportOptions importOptions,
                                                                             ImportMessages importMessages) throws KException {

        // VDB name to create
        String vdbName = importOptions.getOption(OptionKeys.NAME).toString();

        // No node with the requested name - ok to create
        if (!parentObject.hasChild(transaction, vdbName))
            return true;

        // Option specifying how to handle when node exists with requested name
        ExistingNodeOptions exNodeOption = (ExistingNodeOptions)importOptions.getOption(OptionKeys.HANDLE_EXISTING);

        switch (exNodeOption) {
            // RETURN - Return 'false' - do not create a node.  Log an error message
            case RETURN:
                importMessages.addErrorMessage(Messages.getString(Messages.IMPORTER.nodeExistsReturn, vdbName));
                return false;
            // CREATE_NEW - Return 'true' - will create a new VDB with new unique name.  Log a progress message.
            case CREATE_NEW:
                String newName = determineNewName(transaction, vdbName);
                importMessages.addProgressMessage(Messages.getString(Messages.IMPORTER.nodeExistCreateNew, vdbName, newName));
                importOptions.setOption(OptionKeys.NAME, newName);
                break;
            // OVERWRITE - Return 'true' - deletes the existing VDB so that new one can replace existing.
            case OVERWRITE:
                KomodoObject oldNode = parentObject.getChild(transaction, vdbName);
                oldNode.remove(transaction);
        }

    	return true;
    }

    /**
     * @param vdbStream the vdb input stream
     * @return the name of the vdb specified in the xml
     */
    public static String extractVdbName(InputStream vdbStream) {
        if (vdbStream == null)
            return null;

        try {
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            Document doc = db.parse(vdbStream);

            Element docElement = doc.getDocumentElement();
            String docNodeName = docElement.getNodeName();
            String vdbTag = KomodoType.VDB.getAliases().iterator().next();

            if (! vdbTag.equals(docNodeName)) {
                return null;
            }

            return docElement.getAttribute("name"); //$NON-NLS-1$
        } catch (Exception ex) {
            // Don't need to worry about the exception
            return null;
        }
    }

    /**
     * Extracts the name attribute from the vdb xml file and sets it into
     * the import options to synchronise the imported node name with
     * the vdb:name property.
     *
     * @param vdbStream
     * @param importOptions
     * @throws Exception
     */
    private void overrideName(InputStream vdbStream, ImportOptions importOptions) throws Exception {
        String vdbName = extractVdbName(vdbStream);
        if (vdbName == null)
            return;

        importOptions.setOption(OptionKeys.NAME, vdbName);
    }

    /**
     * Perform the vdb import using the specified xml Stream.
     *
     * @param uow the transaction
     * @param vdbStream the vdb xml input stream
     * @param parentObject the parent object in which to place the vdb
     * @param importOptions the options for the import
     * @param importMessages the messages recorded during the import
     */
    public void importVdb(UnitOfWork uow, InputStream vdbStream, KomodoObject parentObject, ImportOptions importOptions, ImportMessages importMessages) {
        ArgCheck.isNotNull(vdbStream);

        try {
            String vdbXml = toString(vdbStream);
            ByteArrayInputStream vdbNameStream = new ByteArrayInputStream(vdbXml.getBytes("UTF-8")); //$NON-NLS-1$
            overrideName(vdbNameStream, importOptions);

            doImport(uow, vdbXml, parentObject, importOptions, importMessages);
        } catch (Exception ex) {
            importMessages.addErrorMessage(ex.getLocalizedMessage());
        }
    }

    /**
     * Perform the vdb import using the specified vdb xml File.
     *
     * @param uow the transaction
     * @param vdbXmlFile the vdb xml file
     * @param parentObject the parent object in which to place the vdb
     * @param importOptions the options for the import
     * @param importMessages the messages recorded during the import
     */
    public void importVdb(UnitOfWork uow, File vdbXmlFile, KomodoObject parentObject, ImportOptions importOptions, ImportMessages importMessages) {
        if (!validFile(vdbXmlFile, importMessages)) return;

        try {
            overrideName(new FileInputStream(vdbXmlFile), importOptions);

            doImport(uow, toString(vdbXmlFile), parentObject, importOptions, importMessages);
        } catch (Exception ex) {
            importMessages.addErrorMessage(ex.getLocalizedMessage());
        }
    }
}
