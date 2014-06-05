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
 * @param <N> 
 *
 */
public interface IMappingBaseNode<N extends IMappingNode> extends IMappingNode {

    /**
     * Add a child to this node
     * 
     * @param childNode
     */
    void addChildNode(N childNode);
    
    /**
     * @param minOccurrences
     */
    void setMinOccurrs(int minOccurrences);

    /**
     * @param maxOccurrences
     */
    void setMaxOccurrs(int maxOccurrences);
    
    /**
     * @param source
     */
    void setSource(String source);
    
    /**
     * @param stagingTable
     */
    void addStagingTable(String stagingTable);
}
