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
public interface IMappingAttribute extends IMappingNode {

    /**
     * The XML Schema namespace for attribute instances
     */
    public static final String NAMESPACE_DECLARATION_ATTRIBUTE_NAMESPACE = "xmlns"; //$NON-NLS-1$
    
    /**
     * @param nameInSource
     */
    void setNameInSource(String nameInSource);
    
    /**
     * @param defaultValue
     */
    void setDefaultValue(String defaultValue);
    
    /**
     * @param value
     */
    void setValue(String value);

    /**
     * @param b
     */
    void setOptional(boolean b);

    /**
     * @param b
     */
    void setAlwaysInclude(boolean b);
    
    /**
     * @param excludeFromDocument
     */
    @Override
	void setExclude(boolean excludeFromDocument);
    
    /**
     * @param normalization
     */
    void setNormalizeText(String normalization);

}
