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

import org.komodo.utils.ModelType;

/**
 * ImportOptions - hold options that may be set for the import
 */
public class ImportOptions {

	private static final String DEFAULT_MODEL_NAME = "importedModel"; //$NON-NLS-1$
	
	private String parserType; 
    private ModelType.Type modelType = ModelType.Type.getType("PHYSICAL");  //$NON-NLS-1$
    private String modelName = DEFAULT_MODEL_NAME;  
    private boolean createModelEntitiesForUnsupportedDdl = false;
    private boolean setModelEntityDescription = false;

    /**
     * Set the ModelType being generated
     * @param modelType the modelType to set
     */
    public void setModelType(ModelType.Type modelType) {
        this.modelType = modelType;
    }

    /**
     * Get the ModelType being generated
     * @return the modelType
     */
    public ModelType.Type getModelType() {
        return this.modelType;
    }

	/**
	 * Set the parser to be used for the import.  If the parser is not specified,
	 * scoring in performed which determines the best fit parser to use.
	 * @param parserType the parser type to use
	 */
	public void setParser(String parserType) {
		this.parserType = parserType;
	}

	/**
	 * Get the parser which is currently set.  
	 * @return the parser type which is set
	 */
	public String getParser( ) {
		return this.parserType;
	}
	
    /**
     * Set the Model name
     * @param modelName the modelName to set
     */
    public void setModelName(String modelName) {
        this.modelName = modelName;
    }

    /**
     * Get the Model name
     * @return the modelName
     */
    public String getModelName() {
        return this.modelName;
    }

    /**
     * Get the createModelEntitiesForUnsupportedDdl state
     * @return the isCreateModelEntitiesForUnsupportedDdl
     */
    public boolean isCreateModelEntitiesForUnsupportedDdl() {
        return createModelEntitiesForUnsupportedDdl;
    }

    /**
     * Set the createModelEntitiesForUnsupportedDdl state
     * @param createModelEntitiesForUnsupportedDdl option for create entities for unsupported ddl
     */
    public void setCreateModelEntitiesForUnsupportedDdl(boolean createModelEntitiesForUnsupportedDdl) {
        this.createModelEntitiesForUnsupportedDdl = createModelEntitiesForUnsupportedDdl;
    }

    /**
     * Get the setModelEntityDescription state
     * @return the setModelEntityDescription
     */
    public boolean isSetModelEntityDescription() {
        return setModelEntityDescription;
    }

    /**
     * Set the setModelEntityDescription state
     * @param setModelEntityDescription option for set model entity descriptions
     */
    public void setModelEntityDescription(boolean setModelEntityDescription) {
        this.setModelEntityDescription = setModelEntityDescription;
    }

}
