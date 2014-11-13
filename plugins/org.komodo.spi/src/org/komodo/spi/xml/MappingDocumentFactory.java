/*************************************************************************************
 * JBoss, Home of Professional Open Source.
* See the COPYRIGHT.txt file distributed with this work for information
* regarding copyright ownership. Some portions may be licensed
* to Red Hat, Inc. under one or more contributor license agreements.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
 ************************************************************************************/
package org.komodo.spi.xml;

import java.io.InputStream;




/**
 *
 */
public interface MappingDocumentFactory {

    /**
     * Add a namespace to the given element
     * 
     * @param element
     * @param prefix 
     * @param uri
     */
    void addNamespace(MappingElement element, String prefix, String uri);

    /**
     * Load a mapping document from the input stream and assign 
     * the document the given name.
     * 
     * @param inputStream
     * @param documentName
     * @return new mapping document
     * 
     * @throws Exception
     */
    MappingDocument loadMappingDocument(InputStream inputStream, String documentName) throws Exception;
    
    /**
     * Create an XML Mapping Document
     * 
     * @param encoding 
     * @param formatted 
     * 
     * @return instance of {@link MappingDocument} 
     */
    MappingDocument createMappingDocument(String encoding, boolean formatted);

    /**
     * Create a mapping element
     * 
     * @param name
     * @param nsPrefix
     * 
     * @return instance of {@link MappingElement}
     */
    MappingElement createMappingElement(String name, String nsPrefix);

    /**
     * Create a recursive mapping element
     * 
     * @param name
     * @param nsPrefix
     * @param recursionMappingClass
     * 
     * @return instance of {@link MappingRecursiveElement}
     */
    MappingRecursiveElement createMappingRecursiveElement(String name,
                                                           String nsPrefix,
                                                           String recursionMappingClass);

    /**
     * Create a mapping attribute
     * 
     * @param name
     * @param nsPrefix
     * 
     * @return instance of {@link MappingAttribute}
     */
    MappingAttribute createMappingAttribute(String name, String nsPrefix);

    /**
     * Create a mapping criteria node
     * 
     * @param criteria
     * @param isDefault
     * 
     * @return instance of {@link MappingCriteriaNode}
     */
    MappingCriteriaNode createMappingCriteriaNode(String criteria, boolean isDefault);
    
    /**
     * Create a mapping choice node
     * 
     * @param exceptionOnDefault
     * 
     * @return instance of {@link MappingChoiceNode}
     */
    MappingChoiceNode createMappingChoiceNode(boolean exceptionOnDefault);

    /**
     * Create a mapping sequence node
     * 
     * @return instance of {@link MappingSequenceNode}
     */
    MappingSequenceNode createMappingSequenceNode();

    /**
     * Create a mapping all node
     *
     * @return instance of {@link MappingAllNode}
     */
    MappingAllNode createMappingAllNode();

}
