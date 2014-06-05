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
 * @since 8.0
 */
public interface ITeiidAdminInfo extends ITeiidConnectionInfo {

    /**
     * The default Teiid Admin persist password flag. Value is {@value} .
     */
    public static final boolean DEFAULT_PERSIST_PASSWORD = true;
    /**
     * The default Teiid Admin port number. Value is {@value} .
     */
    public static final String DEFAULT_PORT = "9999"; //$NON-NLS-1$
    /**
     * The default Teiid Admin port number on teiid 7.7 teiid instances.
     */
    public static final String DEFAULT_LEGACY_PORT = "31443"; //$NON-NLS-1$
    
    /**
     * The default Teiid Admin secure protocol flag. Value is {@value} .
     */
    public static final boolean DEFAULT_SECURE = true;
    
    /**
     * The default username for the Teiid Instance
     */
    public static final String DEFAULT_ADMIN_USERNAME = "admin"; //$NON-NLS-1$
    /**
     * The default password for the Teiid Instance
     */
    public static final String DEFAULT_ADMIN_PASSWORD = "admin"; //$NON-NLS-1$

}
