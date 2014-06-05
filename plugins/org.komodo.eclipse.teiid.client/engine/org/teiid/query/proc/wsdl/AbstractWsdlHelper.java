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
package org.teiid.query.proc.wsdl;

import org.komodo.spi.runtime.version.ITeiidServerVersion;
import org.teiid.language.SQLConstants;

/**
 *
 */
public abstract class AbstractWsdlHelper {

    private final ITeiidServerVersion teiidVersion;

    /**
     * @param teiidVersion
     */
    public AbstractWsdlHelper(ITeiidServerVersion teiidVersion) {
        super();
        this.teiidVersion = teiidVersion;
    }

    public ITeiidServerVersion getTeiidVersion() {
        return teiidVersion;
    }

    /**
     * Converts any name string to a valid SQL symbol segment
     * Basically looks to see if name is a reserved word and if so, returns the name in double-quotes
     * 
     * @param name
     * @return
     */
    protected String convertSqlNameSegment(String name) {       
        if( SQLConstants.isReservedWord(teiidVersion, name) ) {
            return '\"' + name + '\"';
        }
        
        return name;
    }

}
