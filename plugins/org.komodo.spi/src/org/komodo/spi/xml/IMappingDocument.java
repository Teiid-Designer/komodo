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
public interface IMappingDocument<N extends IMappingNode> extends IMappingBaseNode<N> {

    /**
     * @return the root node
     */
    IMappingBaseNode getRootNode();

    /**
     * Generate a string from the mapping stream
     * 
     * @return xml mapping string
     * @throws Exception 
     */
    String getMappingString() throws Exception;

}
