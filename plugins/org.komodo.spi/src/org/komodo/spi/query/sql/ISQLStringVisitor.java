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
package org.komodo.spi.query.sql;

import org.komodo.spi.query.sql.lang.ILanguageObject;

/**
 *
 */
public interface ISQLStringVisitor<LO extends ILanguageObject> extends ILanguageVisitor {

    /**
     * Should the visitor fail to evaluate then this
     * text is returned
     */
    public static final String UNDEFINED = "<undefined>"; //$NON-NLS-1$
    
    /**
     * Find the string representation of the given object
     * 
     * @param languageObject
     * 
     * @return SQL string
     */
    String returnSQLString(LO languageObject);
}
