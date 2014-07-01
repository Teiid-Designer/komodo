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

import org.komodo.spi.xml.IMappingAllNode;
import org.komodo.spi.xml.IMappingAttribute;
import org.komodo.spi.xml.IMappingChoiceNode;
import org.komodo.spi.xml.IMappingCriteriaNode;
import org.komodo.spi.xml.IMappingDocument;
import org.komodo.spi.xml.IMappingDocumentFactory;
import org.komodo.spi.xml.IMappingElement;
import org.komodo.spi.xml.IMappingRecursiveElement;
import org.komodo.spi.xml.IMappingSequenceNode;
import org.teiid.query.mapping.xml.MappingAllNode;
import org.teiid.query.mapping.xml.MappingAttribute;
import org.teiid.query.mapping.xml.MappingChoiceNode;
import org.teiid.query.mapping.xml.MappingCriteriaNode;
import org.teiid.query.mapping.xml.MappingDocument;
import org.teiid.query.mapping.xml.MappingElement;
import org.teiid.query.mapping.xml.MappingLoader;
import org.teiid.query.mapping.xml.MappingNodeConstants;
import org.teiid.query.mapping.xml.MappingRecursiveElement;
import org.teiid.query.mapping.xml.MappingSequenceNode;
import org.teiid.query.mapping.xml.Namespace;
import org.teiid.query.parser.TeiidParser;

/**
 *
 */
public class MappingDocumentFactory implements IMappingDocumentFactory {

    private final TeiidParser teiidParser;

    /**
     * @param teiidParser
     */
    public MappingDocumentFactory(TeiidParser teiidParser) {
        this.teiidParser = teiidParser;
    }

    /**
     * @return the queryFactory
     */
    public TeiidParser getTeiidParser() {
        return this.teiidParser;
    }

    @Override
    public IMappingDocument loadMappingDocument(InputStream inputStream, String documentName) throws Exception {
        MappingLoader reader = new MappingLoader(getTeiidParser());
        MappingDocument mappingDoc = null;
        mappingDoc = reader.loadDocument(inputStream);
        mappingDoc.setName(documentName);
        
        return mappingDoc;
    }
    
    @Override
    public IMappingDocument createMappingDocument(String encoding, boolean formatted) {
        return new MappingDocument(getTeiidParser(), encoding, formatted);
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
    public void addNamespace(IMappingElement element, String prefix, String uri) {
        Namespace namespace = getNamespace(prefix, uri);
        ((MappingElement) element).addNamespace(namespace);
    }

    @Override
    public IMappingElement createMappingElement(String name, String nsPrefix) {
        Namespace namespace = getNamespace(nsPrefix);
        return new MappingElement(getTeiidParser(), name, namespace);
    }

    @Override
    public IMappingRecursiveElement createMappingRecursiveElement(String name,
                                                                  String nsPrefix,
                                                                  String recursionMappingClass) {
        Namespace namespace = getNamespace(nsPrefix);
        return new MappingRecursiveElement(getTeiidParser(), name, namespace, recursionMappingClass);
    }

    @Override
    public IMappingAttribute createMappingAttribute(String name, String nsPrefix) {
        Namespace namespace = getNamespace(nsPrefix);
        return new MappingAttribute(getTeiidParser(), name, namespace);
    }

    @Override
    public IMappingCriteriaNode createMappingCriteriaNode(String criteria, boolean isDefault) {
        return new MappingCriteriaNode(getTeiidParser(), criteria, isDefault); 
    }

    @Override
    public IMappingChoiceNode createMappingChoiceNode(boolean exceptionOnDefault) {
        return new MappingChoiceNode(getTeiidParser(), exceptionOnDefault);
    }
    
    @Override
    public IMappingAllNode createMappingAllNode() {
        return new MappingAllNode(getTeiidParser());
    }
    
    @Override
    public IMappingSequenceNode createMappingSequenceNode() {
        return new MappingSequenceNode(getTeiidParser());
    }
  
}
