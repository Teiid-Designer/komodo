/*************************************************************************************
 * Copyright (c) 2014 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.komodo.spi.runtime;

import java.util.Properties;

/**
 *
 */
public interface ITeiidDataSource {
	
	public interface ERROR_CODES {
		// Data Source issues
		int JDBC_DRIVER_SOURCE_NOT_FOUND = 101; // JDBC Source for Driver class {0} was not found on teiid instance {1}
		int DATA_SOURCE_TYPE_DOES_NOT_EXIST_ON_TEIID = 102; // Data Source Type {0} does not exist on teiid instance {1}
		int DATA_SOURCE_COULD_NOT_BE_CREATED = 103; // errorCreatingDataSource = Data Source {0} could not be created for type {1}
		int NO_CONNECTION_PROVIDER = 104; //
		int NO_CONNECTION_PROFILE_DEFINED_IN_MODEL = 105; // 
		int NO_TEIID_RELATED_PROPERTIES_IN_PROFILE = 106; // 
		int COULD_NOT_GET_OR_CREATE_DATASOURCE = 107; // 
	}

    /**
     * @return display name of data source
     */
    String getDisplayName();

    /**
     * @return real name of data source, maybe different from display name
     */
    String getName();

    /**
     * Returns the data source type name
     * 
     * @return the type
     */
    String getType();

    /**
     * @return properties of data source
     */
    Properties getProperties();

    /**
     * @param name
     * 
     * @return value of named property
     */
    String getPropertyValue(String name);

    /**
     * Set the profile name
     * 
     * @param name
     */
    void setProfileName(String name);

    /**
     * @return profile name
     */
    String getProfileName();

    /**
     * @return isPreview
     */
    boolean isPreview();

    /**
     * @param isPreview Sets isPreview to the specified value.
     */
    void setPreview(boolean isPreview);

}
