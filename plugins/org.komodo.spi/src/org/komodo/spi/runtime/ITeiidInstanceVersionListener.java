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

import org.komodo.spi.runtime.version.ITeiidVersion;

/**
 *
 */
public interface ITeiidInstanceVersionListener {

    /**
     * Instance has been changed
     *
     * @param instance
     */
    void instanceChanged(ITeiidInstance instance);

    /**
     * Version of Teiid Instance has been changed
     * 
     * @param version
     */
    void versionChanged(ITeiidVersion version);
    
}
