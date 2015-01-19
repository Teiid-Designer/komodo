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
import java.io.InputStream;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;

/**
 *  DdlImportService interface
 */
public interface DdlImporter extends StringConstants {

	/**
	 * Perform the model import using the specified DDL.  The DDL constructs must be valid to put directly beneath a model.
	 * @param ddl the DDL
	 * @param importOptions the options for the import
	 * @param importMessages the messages recorded during the import
	 * @return newly created root ddl node
	 */
	KomodoObject importDdl(String ddl, ImportOptions importOptions, ImportMessages importMessages);

	/**
     * Perform the model import using the specified DDL Stream.  The DDL constructs must be valid to put directly beneath a model.
     * @param ddlStream the DDL input stream
     * @param name the name of the ddl file or other such customary name
     * @param importOptions the options for the import
     * @param importMessages the messages recorded during the import
	 * @return newly created root ddl node
     */
    KomodoObject importDdl(InputStream ddlStream, String name, ImportOptions importOptions, ImportMessages importMessages);

	/**
	 * Perform the model import using the specified DDL File.  The DDL constructs must be valid to put directly beneath a model.
	 * @param ddlFile the DDL file
	 * @param importOptions the options for the import
	 * @param importMessages the messages recorded during the import
	 * @return newly created root ddl node 
	 */
	KomodoObject importDdl(File ddlFile, ImportOptions importOptions, ImportMessages importMessages);

}
