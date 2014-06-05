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
package org.teiid.query.sql.symbol;

import org.komodo.spi.annotation.Removed;
import org.komodo.spi.runtime.version.TeiidVersion.Version;
import org.teiid.query.parser.LanguageVisitor;
import org.teiid.query.sql.lang.Node;
import org.teiid.query.sql.lang.SingleElementSymbol;

/**
 *
 */
@SuppressWarnings( "unused" )
public interface WindowFunction extends Node, SingleElementSymbol, Expression {

    /**
     * @return the function
     */
    AggregateSymbol getFunction();

    /**
     * @param function the function to set
     */
    void setFunction(AggregateSymbol function);

    /**
     * @return the windowSpecification
     */
    WindowSpecification getWindowSpecification();

    /**
     * @param windowSpecification the windowSpecification to set
     */
    void setWindowSpecification(WindowSpecification windowSpecification);

    /**
     * @return name
     */
    @Removed(Version.TEIID_8_0)
    String getName();

    /**
     * @param name
     */
    @Removed(Version.TEIID_8_0)
    void setName(String name);

    @Override
    Class<?> getType();

    /** Accept the visitor. **/
    @Override
    void acceptVisitor(LanguageVisitor visitor);

}
