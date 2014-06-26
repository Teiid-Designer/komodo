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
package org.komodo.eclipse.spi.datatools.connectivity;



/**
 * Interface that should be implemented by classes providing
 * implementations of secure storage.
 * 
 *
 */
public interface ISecureStorageProvider {
    
    /**
     * Retrieve the value stored against the given key from 
     * the secure storage.
     * 
     * @param nodeKey 
     * @param key
     * @return value stored against key
     * @throws Exception 
     */
    String getFromSecureStorage(String nodeKey, String key) throws Exception;
    
    /**
     * Store the given value against the key in eclipse's secure
     * storage.
     * 
     * @param nodeKey 
     * @param key
     * @param value
     * @throws Exception
     */
    void storeInSecureStorage(String nodeKey, String key, String value) throws Exception;
}
