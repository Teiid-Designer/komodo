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

/**
 *
 */
public interface ITeiidJdbcInfo extends ITeiidConnectionInfo {

    /**
     * The default Teiid JDBC port number. Value is {@value} .
     */
    public static final String DEFAULT_PORT = "31000"; //$NON-NLS-1$
    /**
     * The default Teiid Admin secure protocol flag. Value is {@value} .
     */
    public static final boolean DEFAULT_SECURE = false;
    /**
     * The default username for the Teiid Instance
     */
    public static final String DEFAULT_JDBC_USERNAME = "user"; //$NON-NLS-1$
    /**
     * The default password for the Teiid Instance
     */
    public static final String DEFAULT_JDBC_PASSWORD = "user"; //$NON-NLS-1$

}
