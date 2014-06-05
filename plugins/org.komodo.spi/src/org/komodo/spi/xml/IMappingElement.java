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
package org.komodo.spi.xml;


/**
 *
 */
public interface IMappingElement<A extends IMappingAttribute, N extends IMappingNode>
    extends IMappingBaseNode<N> {

    /**
     * @param nameInSource
     */
    void setNameInSource(String nameInSource);

    /**
     * @param defaultValue
     */
    void setDefaultValue(String defaultValue);

    /**
     * @param fixedValue
     */
    void setValue(String fixedValue);

    /**
     * @param nillable
     */
    void setNillable(boolean nillable);

    /**
     * @param buitInType
     */
    void setType(String buitInType);

    /**
     * @param xsiTypeTextNormalization
     */
    void setNormalizeText(String xsiTypeTextNormalization);

    /**
     * @param attribute
     */
    void addAttribute(A attribute);

    /**
     * @param text
     */
    void addCommentNode(String text);

}
