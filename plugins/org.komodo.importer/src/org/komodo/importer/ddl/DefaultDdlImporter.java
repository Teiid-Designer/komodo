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

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileReader;
import java.io.InputStream;
import org.komodo.importer.ddl.ImportOptions.OptionKeys;
import org.komodo.importer.ddl.Messages.DDL_IMPORTER;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Schema;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.FileUtils;
import org.komodo.utils.ModelType;
import org.komodo.utils.StringUtils;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlParser;

/**
 * DefaultDdlImportService is the default implementation for importing from DDL Schema.
 */
public class DefaultDdlImporter implements DdlImporter {

    private static final String TRANSACTION_NAME = "find-create-schema-parent"; //$NON-NLS-1$

    private Repository repository;

    private UnitOfWork transaction;

    /**
     * constructor
     *
     * @param repository repository into which ddl should be imported
     *
     * @param transaction the transaction used for importing the DDL schema 
     */
    public DefaultDdlImporter(Repository repository, UnitOfWork transaction) {
        this.repository = repository;
        this.transaction = transaction;
    }

    /**
     * constructor
     *
     * @param repository repository into which ddl should be imported
     * 
     */
    public DefaultDdlImporter(Repository repository) {
        this(repository, null);
    }

    private KomodoObject prepareImport(String ddl, ImportOptions importOptions, ImportMessages importMessages) throws Exception {

        if(StringUtils.isEmpty(ddl)) {
            importMessages.addErrorMessage(Messages.getString(DDL_IMPORTER.errorEmptyDdlMsg));
            return null;
        }
        
        if (transaction == null)
            transaction = repository.createTransaction(TRANSACTION_NAME, false, null);

        /*
         * Create model object in library
         */
        WorkspaceManager wkspManager = WorkspaceManager.getInstance(repository);
        KomodoObject workspace = repository.komodoWorkspace(transaction);
        KomodoObject resultNode = null;

        switch(importOptions.getImportType()) {
            case MODEL:
                String modelName = importOptions.getOption(OptionKeys.MODEL_NAME).toString();
                ModelType.Type modelType = (ModelType.Type) importOptions.getOption(OptionKeys.MODEL_TYPE);

                Model model = wkspManager.createModel(transaction, workspace, modelName);
                model.setModelType(transaction, modelType.toString());
                model.setModelDefinition(transaction, ddl);
                resultNode = model;
                break;

            case SCHEMA:
                String schemaName = importOptions.getOption(OptionKeys.SCHEMA_NAME).toString();
                Schema schema = wkspManager.createSchema(transaction, workspace, schemaName);
                schema.setRendition(transaction, ddl);
                resultNode = schema;
                break;
            default:
                break;
        }

        // Label this node with the type of parser
        if (resultNode != null)
            resultNode.setProperty(transaction, StandardDdlLexicon.PARSER_ID, TeiidDdlParser.ID);

        transaction.commit();

        // Once committed the ddl sequencer should take over and sequence the file
        // under modelNode/ddl-statements, which is a sibling of modelNode/fileNode

        return resultNode;
    }

    private String readDdl(File ddlFile) throws Exception {
        StringBuilder builder;
        FileReader fileReader = null;
        try {
            fileReader = new FileReader(ddlFile);

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

    private String readDdl(InputStream inputStream) throws Exception {
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

    private KomodoObject prepareModel(File ddlFile, ImportOptions importOptions, ImportMessages importMessages) throws Exception {
        return prepareImport(readDdl(ddlFile), importOptions, importMessages);
    }

    @Override
    public KomodoObject importDdl(File ddlFile, ImportOptions importOptions, ImportMessages importMessages) {
        KomodoObject ko = null;

        if (!ddlFile.exists() || ddlFile.isDirectory()) {
            importMessages.addErrorMessage(Messages.getString(DDL_IMPORTER.errorDdlFileNotFoundMsg, ddlFile.getName()));
            return ko;
        }
        if (!ddlFile.canRead()) {
            importMessages.addErrorMessage(Messages.getString(DDL_IMPORTER.errorDdlFileNotReadableMsg, ddlFile.getName()));
            return ko;
        }

        try {
            ko = prepareModel(ddlFile, importOptions, importMessages);
        } catch (Exception ex) {
            importMessages.addErrorMessage(ex.getLocalizedMessage());
        }

        return ko;
    }

    @Override
    public KomodoObject importDdl(InputStream ddlStream, String name, ImportOptions importOptions, ImportMessages importMessages) {
        assert (ddlStream != null);
        assert (name != null);

        KomodoObject ko = null;
        try {
            ko = prepareImport(readDdl(ddlStream), importOptions, importMessages);
        } catch (Exception ex) {
            importMessages.addErrorMessage(ex.getLocalizedMessage());
        }

        return ko;
    }

    @Override
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
