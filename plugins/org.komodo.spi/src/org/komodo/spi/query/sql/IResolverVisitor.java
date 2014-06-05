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

import java.util.Collection;

import org.komodo.spi.query.metadata.IQueryMetadataInterface;
import org.komodo.spi.query.sql.lang.ILanguageObject;
import org.komodo.spi.query.sql.symbol.IGroupSymbol;

/**
 *
 */
public interface IResolverVisitor<LO extends ILanguageObject, GS extends IGroupSymbol> {
    
    public static final String SHORT_NAME = "shortName"; //$NON-NLS-1$

    @Deprecated
    void setProperty(String propertyName, Object value);

    void resolveLanguageObject(LO obj, IQueryMetadataInterface metadata) throws Exception;

    void resolveLanguageObject(LO obj, Collection<GS> groups, IQueryMetadataInterface metadata) throws Exception;
}
