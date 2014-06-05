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
package org.komodo.spi.query.sql.symbol;

import java.util.List;

import org.komodo.spi.query.sql.ILanguageVisitor;
import org.komodo.spi.query.sql.lang.ILanguageObject;

/**
 *
 */
public interface IMultipleElementSymbol<E extends IElementSymbol, LV extends ILanguageVisitor>
    extends ILanguageObject<LV> {

    /**
     * Get the element symbols referred to by this multiple element symbol
     * 
     * @return List of {@link ElementSymbol}s, may be null
     */
    List<E> getElementSymbols();

}
