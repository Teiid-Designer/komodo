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
package org.komodo.relational.importer.ddl;

import java.io.File;
import java.io.InputStream;
import org.komodo.importer.AbstractImporter;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.importer.ImportType;
import org.komodo.importer.Messages;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Schema;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.ModelType;
import org.teiid.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlParser;

/**
 * Importer implementation for importing from DDL Schema.
 */
public class DdlImporter extends AbstractImporter {

    /**
     * constructor
     *
     * @param repository repository into which ddl should be imported
     */
    public DdlImporter(Repository repository) {
        // set default import type to model
        super(repository, ImportType.MODEL);
    }

    @Override
    protected void executeImport(UnitOfWork transaction, String content,
    		                             KomodoObject parentObject, ImportOptions importOptions,
    		                             ImportMessages importMessages) throws KException {

    	// Get the parentObject type that we are importing into
    	KomodoType kType = parentObject.getTypeIdentifier(transaction);

        switch(kType) {
            case MODEL:
            {
            	Model model = Model.RESOLVER.resolve(transaction, parentObject);
                ModelType.Type modelType = (ModelType.Type) importOptions.getOption(OptionKeys.MODEL_TYPE);
                model.setModelType(transaction, Model.Type.valueOf(modelType.toString()));
                model.setModelDefinition(transaction, content);
                model.setProperty(transaction, StandardDdlLexicon.PARSER_ID, TeiidDdlParser.ID);
                return;
            }
            case SCHEMA:
            {
            	Schema schema = Schema.RESOLVER.resolve(transaction, parentObject);
                schema.setRendition(transaction, content);
                schema.setProperty(transaction, StandardDdlLexicon.PARSER_ID, TeiidDdlParser.ID);
                return;
            }
            default:
                throw new UnsupportedOperationException("DDL Import parent object should be either a model or schema"); //$NON-NLS-1$
        }
    }

    /**
	 * @throws KException
	 */
    @Override
    protected boolean handleExistingNode(UnitOfWork transaction,
                                                                             KomodoObject parentObject,
                                                                             ImportOptions importOptions,
                                                                             ImportMessages importMessages) throws KException {

    	return true;
    }

    /**
     * Perform the DDL import using the specified DDL File.  The DDL constructs must be valid to put directly beneath the parentObject.
     * @param uow the transaction
     * @param ddlFile the DDL file
     * @param parentObject the target parent object to place the new objects
     * @param importOptions the options for the import
     * @param importMessages the messages recorded during the import
     */
    public void importDdl(UnitOfWork uow, File ddlFile, KomodoObject parentObject, ImportOptions importOptions, ImportMessages importMessages) {
        if (!validFile(ddlFile, importMessages))
            return;

        try {
            doImport(uow, toString(ddlFile), parentObject, importOptions, importMessages);
        } catch (Exception ex) {
            importMessages.addErrorMessage(ex.getLocalizedMessage());
        }
    }

    /**
     * Perform the model import using the specified DDL Stream.  The DDL constructs must be valid to put directly beneath a model.
     * @param uow the transaction
     * @param ddlStream the DDL input stream
     * @param parentObject the target parent object to place the new objects
     * @param importOptions the options for the import
     * @param importMessages the messages recorded during the import
     */
    public void importDdl(UnitOfWork uow, InputStream ddlStream, KomodoObject parentObject, ImportOptions importOptions, ImportMessages importMessages) {
        ArgCheck.isNotNull(ddlStream);

        try {
            doImport(uow, toString(ddlStream), parentObject, importOptions, importMessages);
        } catch (Exception ex) {
            String errorMsg = ex.getLocalizedMessage() != null ? ex.getLocalizedMessage() : ex.getClass().getSimpleName();
            importMessages.addErrorMessage(errorMsg);
        }
    }

    /**
     * Perform the model import using the specified DDL.  The DDL constructs must be valid to put directly beneath a model.
     * @param uow the transaction
     * @param ddl the DDL
     * @param parentObject the target parent object to place the new objects
     * @param importOptions the options for the import
     * @param importMessages the messages recorded during the import
     */
    public void importDdl(UnitOfWork uow, String ddl, KomodoObject parentObject, ImportOptions importOptions, ImportMessages importMessages) {
        try {
            doImport(uow, ddl, parentObject, importOptions, importMessages);
        } catch (Exception ex) {
            importMessages.addErrorMessage(ex.getLocalizedMessage());
        }
    }

    /**
     * Set the import type. DDL importer does NOT support the VDB import type
     *
     * @param importType the type of import
     */
    public void setImportType(ImportType importType) {
        if (ImportType.VDB.equals(importType))
            throw new UnsupportedOperationException(Messages.getString(Messages.IMPORTER.ddlDoesNotSupportVDB));

        this.importType = importType;
    }
}
