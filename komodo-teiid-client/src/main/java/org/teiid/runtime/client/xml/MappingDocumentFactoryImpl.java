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
package org.teiid.runtime.client.xml;

import java.io.InputStream;

import org.komodo.spi.xml.MappingAllNode;
import org.komodo.spi.xml.MappingAttribute;
import org.komodo.spi.xml.MappingChoiceNode;
import org.komodo.spi.xml.MappingCriteriaNode;
import org.komodo.spi.xml.MappingDocument;
import org.komodo.spi.xml.MappingDocumentFactory;
import org.komodo.spi.xml.MappingElement;
import org.komodo.spi.xml.MappingRecursiveElement;
import org.komodo.spi.xml.MappingSequenceNode;
import org.teiid.query.mapping.xml.MappingAllNodeImpl;
import org.teiid.query.mapping.xml.MappingAttributeImpl;
import org.teiid.query.mapping.xml.MappingChoiceNodeImpl;
import org.teiid.query.mapping.xml.MappingCriteriaNodeImpl;
import org.teiid.query.mapping.xml.MappingDocumentImpl;
import org.teiid.query.mapping.xml.MappingElementImpl;
import org.teiid.query.mapping.xml.MappingLoaderImpl;
import org.teiid.query.mapping.xml.MappingNodeConstants;
import org.teiid.query.mapping.xml.MappingRecursiveElementImpl;
import org.teiid.query.mapping.xml.MappingSequenceNodeImpl;
import org.teiid.query.mapping.xml.Namespace;
import org.teiid.query.parser.TeiidClientParser;

/**
 *
 */
public class MappingDocumentFactoryImpl implements MappingDocumentFactory {

    private final TeiidClientParser teiidParser;

    /**
     * @param teiidParser
     */
    public MappingDocumentFactoryImpl(TeiidClientParser teiidParser) {
        this.teiidParser = teiidParser;
    }

    /**
     * @return the queryFactory
     */
    public TeiidClientParser getTeiidParser() {
        return this.teiidParser;
    }

    @Override
    public MappingDocument loadMappingDocument(InputStream inputStream, String documentName) throws Exception {
        MappingLoaderImpl reader = new MappingLoaderImpl(getTeiidParser());
        MappingDocumentImpl mappingDoc = null;
        mappingDoc = reader.loadDocument(inputStream);
        mappingDoc.setName(documentName);
        
        return mappingDoc;
    }
    
    @Override
    public MappingDocument createMappingDocument(String encoding, boolean formatted) {
        return new MappingDocumentImpl(getTeiidParser(), encoding, formatted);
    }
    
    private Namespace getNamespace(final String prefix) {
        if (prefix != null) 
            return new Namespace(prefix);
        
        return MappingNodeConstants.NO_NAMESPACE;
    }
    
    private Namespace getNamespace(final String prefix, final String uri) {
        return new Namespace(prefix, uri);
    }
    
    @Override
    public void addNamespace(MappingElement element, String prefix, String uri) {
        Namespace namespace = getNamespace(prefix, uri);
        ((MappingElementImpl) element).addNamespace(namespace);
    }

    @Override
    public MappingElement createMappingElement(String name, String nsPrefix) {
        Namespace namespace = getNamespace(nsPrefix);
        return new MappingElementImpl(getTeiidParser(), name, namespace);
    }

    @Override
    public MappingRecursiveElement createMappingRecursiveElement(String name,
                                                                  String nsPrefix,
                                                                  String recursionMappingClass) {
        Namespace namespace = getNamespace(nsPrefix);
        return new MappingRecursiveElementImpl(getTeiidParser(), name, namespace, recursionMappingClass);
    }

    @Override
    public MappingAttribute createMappingAttribute(String name, String nsPrefix) {
        Namespace namespace = getNamespace(nsPrefix);
        return new MappingAttributeImpl(getTeiidParser(), name, namespace);
    }

    @Override
    public MappingCriteriaNode createMappingCriteriaNode(String criteria, boolean isDefault) {
        return new MappingCriteriaNodeImpl(getTeiidParser(), criteria, isDefault); 
    }

    @Override
    public MappingChoiceNode createMappingChoiceNode(boolean exceptionOnDefault) {
        return new MappingChoiceNodeImpl(getTeiidParser(), exceptionOnDefault);
    }
    
    @Override
    public MappingAllNode createMappingAllNode() {
        return new MappingAllNodeImpl(getTeiidParser());
    }
    
    @Override
    public MappingSequenceNode createMappingSequenceNode() {
        return new MappingSequenceNodeImpl(getTeiidParser());
    }
  
}
