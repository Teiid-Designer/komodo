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
package org.komodo.ddl.importer;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.komodo.ddl.importer.Messages.DDL_IMPORTER;
import org.komodo.ddl.importer.node.DerbyImporter;
import org.komodo.ddl.importer.node.OracleImporter;
import org.komodo.ddl.importer.node.PostgresImporter;
import org.komodo.ddl.importer.node.StandardImporter;
import org.komodo.ddl.importer.node.TeiidDdlImporter;
import org.komodo.relational.compare.DifferenceGenerator;
import org.komodo.relational.compare.DifferenceReport;
import org.komodo.relational.model.Model;
import org.komodo.utils.FileUtils;
import org.komodo.utils.StringUtil;
import org.modeshape.common.text.ParsingException;
import org.modeshape.common.text.Position;
import org.modeshape.sequencer.ddl.DdlParsers;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.node.AstNode;

/**
 * DefaultDdlImportService is the default implementation for importing Relational Models from DDL.
 */
public class DefaultDdlImportService implements DdlImportService {

	private static DdlImportService instance;
	private Map<String,DdlNodeImporter> REGISTERED_IMPORTERS;

    /**
     * Get the instance
     * @return the singleton instance
     */
    public static DdlImportService getInstance( ) {
        if (instance == null) {
            instance = new DefaultDdlImportService( );
        }

        return instance;
    }
    
    /**
     * ModelImporterImpl constructor
     */
    private DefaultDdlImportService( ) {
    	loadRegisteredImporters();
    }
    
    private void loadRegisteredImporters() {
    	REGISTERED_IMPORTERS = new HashMap<String,DdlNodeImporter>();
    	REGISTERED_IMPORTERS.put(SQL92_DIALECT, new StandardImporter());
    	REGISTERED_IMPORTERS.put(ORACLE_DIALECT, new OracleImporter());
    	REGISTERED_IMPORTERS.put(DERBY_DIALECT, new DerbyImporter());
    	REGISTERED_IMPORTERS.put(POSTGRES_DIALECT, new PostgresImporter());
    	REGISTERED_IMPORTERS.put(TEIID_DIALECT, new TeiidDdlImporter());
    }

	/* (non-Javadoc)
	 * @see org.komodo.ddl.importer.ModelImporter#importDdl(java.lang.String,org.komodo.ddl.importer.ImportOptions,org.komodo.ddl.importer.ImportMessages)
	 */
	@Override
	public Model importDdl(String ddl,ImportOptions importOptions,ImportMessages importMessages) {
		return importDdl(ddl,null,importOptions,importMessages);

	}

	/* (non-Javadoc)
	 * @see org.komodo.ddl.importer.ModelImporter#importDdl(java.io.File,org.komodo.ddl.importer.ImportOptions,org.komodo.ddl.importer.ImportMessages)
	 */
	@Override
	public Model importDdl(File ddlFile,ImportOptions importOptions,ImportMessages importMessages) {
        if (!ddlFile.exists() || ddlFile.isDirectory()) {
        	importMessages.addErrorMessage(Messages.getString(DDL_IMPORTER.errorDdlFileNotFoundMsg,ddlFile.getName()));
        	return null;
        }
        if (!ddlFile.canRead()) {
        	importMessages.addErrorMessage(Messages.getString(DDL_IMPORTER.errorDdlFileNotReadableMsg,ddlFile.getName()));
        	return null;
        }

		// Read DDL from the file
		StringBuilder builder;
		try {
			FileReader fileReader = new FileReader(ddlFile);

			// Read the file contents
			char[] buf = new char[FileUtils.DEFAULT_BUFFER_SIZE];
			builder = new StringBuilder();
			for (int charTot = fileReader.read(buf); charTot >= 0; charTot = fileReader.read(buf))
				builder.append(buf, 0, charTot);

			fileReader.close();
		} catch (FileNotFoundException fndEx) {
			importMessages.addErrorMessage(Messages.getString(DDL_IMPORTER.errorDdlFileNotFoundMsg,ddlFile.getName()));
			return null;
		} catch (IOException ioEx) {
			importMessages.addErrorMessage(Messages.getString(DDL_IMPORTER.errorReadingDdlFileMsg,ddlFile.getName()));
			return null;
		}
		
		return importDdl(builder.toString(),ddlFile.getName(),importOptions,importMessages);
	}
	
	/**
	 * Import model from DDL string, appending any error messages to the supplied messages
	 * @param ddlString the DDL
	 * @param ddlFileName the ddl filename, null if not known.  if supplied, may impact scoring.
	 * @param messages the messages
	 * @return the relational model
	 */
	private Model importDdl(String ddlString, String ddlFileName, ImportOptions importOptions, ImportMessages importMessages) {
		Model relationalModel = null;
		if(StringUtil.isEmpty(ddlString)) {
			importMessages.addErrorMessage(Messages.getString(DDL_IMPORTER.errorEmptyDdlMsg));
			return null;
		}
        // Use specified parser if it has been set
        AstNode rootNode = null;
        DdlParsers parsers = new DdlParsers();
        try {
        	// DDL Parser specified
        	if(!StringUtil.isEmpty(importOptions.getParser())) {
        		rootNode = parsers.parseUsing(ddlString,importOptions.getParser());
        	// No DDL parser is specified - user DdlParsers which will score the best fit
        	} else {
        		rootNode = parsers.parse(ddlString, ddlFileName);
        	}
        // If parsing exception is encountered, throw DdlImportException
        } catch (ParsingException e) {
        	String parseMessage = e.getMessage();
        	importMessages.setParseErrorMessage(parseMessage);
        	Position position = e.getPosition();
        	importMessages.setParseErrorColNumber(position.getColumn());
        	importMessages.setParseErrorLineNumber(position.getLine());
        	importMessages.setParseErrorIndex(position.getIndexInContent());
        	if(!StringUtil.isEmpty(importOptions.getParser())) {
        		importMessages.setParserId(importOptions.getParser());
        	} else if(rootNode!=null) {
                String parserId = (String) rootNode.getProperty(StandardDdlLexicon.PARSER_ID);
                importMessages.setParserId(parserId);
        	}
        	return null;
        }
        String parserId = (String) rootNode.getProperty(StandardDdlLexicon.PARSER_ID);
        
        DdlNodeImporter nodeImporter = REGISTERED_IMPORTERS.get(parserId);
        if (nodeImporter == null) {
        	importMessages.addErrorMessage(Messages.getString(DDL_IMPORTER.noDDLImporterRegisteredMsg, parserId));
        	return null;
        }
        
        try {
			relationalModel = nodeImporter.importNode(rootNode,importOptions,importMessages);
		} catch (Exception ex) {
        	importMessages.addErrorMessage(Messages.getString(DDL_IMPORTER.errorConvertingAstToRelationalMsg, ex.getMessage()));
        	return null;
		}
        
		return relationalModel;
	}
	
	/* (non-Javadoc)
	 * @see org.komodo.ddl.importer.ModelImporter#getDifferenceReport(org.komodo.relational.model.Model, org.komodo.relational.model.Model)
	 */
	@Override
	public DifferenceReport getDifferenceReport(Model originalModel, Model targetModel) {
        return DifferenceGenerator.compare(originalModel,targetModel);
	}

}