/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */

package org.teiid.query.mapping.xml;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;

import org.komodo.spi.annotation.Removed;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.komodo.spi.xml.MappingDocument;
import org.teiid.core.util.ArgCheck;
import org.teiid.query.parser.TeiidParser;
import org.teiid.runtime.client.Messages;



/** 
 * A Mapping Node document object.
 */
public class MappingDocumentImpl extends MappingBaseNodeImpl implements MappingDocument<MappingNodeImpl> {
    
    MappingBaseNodeImpl root;
    boolean formatted;
    String encoding;
    String name;
    
    public MappingDocumentImpl(TeiidParser teiidParser, boolean formatted) {
        this(teiidParser, MappingNodeConstants.Defaults.DEFAULT_DOCUMENT_ENCODING, formatted);
    }
            
    public MappingDocumentImpl(TeiidParser teiidParser, String encoding, boolean formatted) {
        super(teiidParser);
        if (encoding == null) {
            encoding = MappingNodeConstants.Defaults.DEFAULT_DOCUMENT_ENCODING;
        }
        setDocumentEncoding(encoding);
        setFormatted(formatted);
    }
    
    public void acceptVisitor(MappingVisitor visitor) {
        visitor.visit(this);
    }

    public String getName() {
        return name;
    }
    
    public void setName(String name) {
        this.name = name;
    }
    
    public String getFullyQualifiedName() {
        return name;
    }

	@Removed(Version.TEIID_8_0)
    public String getCanonicalName() {
        return name.toUpperCase();
    }

    public MappingBaseNodeImpl getRootNode() {
        return root;
    }
    
    /**
     * A tag root is the first visual node on the document. A document can contain a "source" node
     * at root, but what ever is the first maping element that is the tag root.
     * @return
     */
    public MappingElementImpl getTagRootElement() {
        if (this.root instanceof MappingSourceNodeImpl) {
            return (MappingElementImpl)this.root.getNodeChildren().get(0);
        }
        return (MappingElementImpl)this.root;
    }
    
    void setRoot(MappingBaseNodeImpl root) {
        if (root != null) {
            this.root = root;
            this.getChildren().clear();
            this.addChild(root);
        }
    }    
          
    public String getDocumentEncoding() {
        return this.encoding;
    }
    
    public boolean isFormatted() {
        return this.formatted;
    }
    
    public boolean isDocumentNode() {
        return true;
    }
    
    public void setDocumentEncoding(String encoding) {
        this.encoding = encoding;
        setProperty(MappingNodeConstants.Properties.DOCUMENT_ENCODING, this.encoding);
    }
    
    public void setFormatted(boolean formatted) {
        this.formatted = formatted;
        setProperty(MappingNodeConstants.Properties.FORMATTED_DOCUMENT, Boolean.valueOf(this.formatted));
    }    
    
    /**
     * Make sure the cardinality is set correctly
     */
    private void fixCardinality(MappingElementImpl root) {
        root.setMaxOccurrs(1);
        root.setMinOccurrs(1);
    }     
    
    public void addAllNode(MappingAllNodeImpl elem) {
         throw new RuntimeException(Messages.gs(Messages.TEIID.TEIID30452));
    }

    public void addChoiceNode(MappingChoiceNodeImpl elem) {
         throw new RuntimeException(Messages.gs(Messages.TEIID.TEIID30452));
    }

    public void addSequenceNode(MappingSequenceNodeImpl elem) {
         throw new RuntimeException(Messages.gs(Messages.TEIID.TEIID30452));
    }
    
    public void addChildElement(MappingElementImpl elem) {
    	ArgCheck.isNotNull(elem);
        fixCardinality(elem);
        setRoot(elem);
    }    
    
    public void addSourceNode(MappingSourceNodeImpl elem) {
        ArgCheck.isNotNull(elem);
        setRoot(elem);
    }
    
    /** 
     * @see org.teiid.query.mapping.xml.MappingNodeImpl#clone()
     */
    public MappingDocumentImpl clone() {
		MappingDocumentImpl clone = (MappingDocumentImpl) super.clone();
		clone.root = (MappingBaseNodeImpl) clone.getChildren().iterator().next();
		return clone;
    }  
    
    @Override
    public String getMappingString() throws Exception{
     // Output the mapping objects to a stream
        String result = null;
        OutputStream moStream = null;
        try {
            moStream = new ByteArrayOutputStream();
            final PrintWriter pw = new PrintWriter(moStream, true);
            final MappingOutputter outputter = new MappingOutputter();
            outputter.write(this, pw); // TODO FIX/REPLACE??? , isNewlines(), isIndent());
            pw.flush();

            result = moStream.toString();
        } catch (final Exception e) {
            throw e;
        } finally {
            if (moStream != null) {
                try {
                    moStream.close();
                } catch (final IOException e1) {
                    throw e1;
                }
                moStream = null;
            }
        }
        return result;
    }
    
}
