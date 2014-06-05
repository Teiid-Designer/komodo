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

import java.util.List;

import org.komodo.spi.query.sql.lang.ILanguageObject;
import org.komodo.spi.query.sql.symbol.IReference;

/**
 *
 */
public interface IReferenceCollectorVisitor<LO extends ILanguageObject, R extends IReference> {

    /**
     * Get the references from obj in a collection.
     * 
     * @param obj Language object
     * 
     * @return List of {@link IReference}
     */
    List<R> findReferences(LO obj);
}
