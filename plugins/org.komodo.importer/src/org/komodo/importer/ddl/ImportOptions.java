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

import java.util.HashMap;
import java.util.Map;
import org.komodo.utils.ModelType;

/**
 * ImportOptions - hold options that may be set for the import
 */
public class ImportOptions {

	private static final String DEFAULT_MODEL_NAME = "importedModel"; //$NON-NLS-1$

	private static final String DEFAULT_SCHEMA_NAME = "importedSchema"; //$NON-NLS-1$

	/**
	 * The type of import to be executed
	 */
	public static enum ImportType {
	    /**
	     * Import ddl into a default model
	     */
	    MODEL,

	    /**
	     * Import ddl as a schema fragment
	     */
	    SCHEMA;
	}

	/**
	 * Option keys applicable to import ddl into a model
	 */
	public static enum OptionKeys {
	    /**
	     * Name of model
	     */
	    MODEL_NAME(DEFAULT_MODEL_NAME),

	    /**
	     * Type of model, default is PHYSICAL
	     */
	    MODEL_TYPE(ModelType.Type.PHYSICAL),

        /**
         * Name of schema fragment
         */
        SCHEMA_NAME(DEFAULT_SCHEMA_NAME);

	    private Object defaultValue;

        /**
	     * Constructor
	     */
	    private OptionKeys(Object defaultValue) {
            this.defaultValue = defaultValue;
	    }

        /**
         * @return default value of this model option
         */
        public Object defaultValue() {
            return defaultValue;
        }
	}

	// Import as model by default
    private ImportType importType = ImportType.MODEL;

    private final Map<OptionKeys, Object> options = new HashMap<OptionKeys, Object>();

    /**
     * @return the importType
     */
    public ImportType getImportType() {
        return this.importType;
    }

    /**
     * @param importType type of import that should be executed
     */
    public void setImportType(ImportType importType) {
        this.importType = importType;
    }

    /**
     * @param optionName
     * @return the option for the given model option
     */
    public Object getOption(OptionKeys optionName) {
        if (optionName == null)
            return null;

        Object value = options.get(optionName);
        if (value != null)
            return value;

        // Value is null so return defaults if applicable
        return optionName.defaultValue();
    }

    /**
     * @param optionName
     * @param value
     */
    public void setOption(OptionKeys optionName, Object value) {
        options.put(optionName, value);
    }

}
