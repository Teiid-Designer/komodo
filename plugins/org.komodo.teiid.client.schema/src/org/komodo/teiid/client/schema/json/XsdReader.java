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
package org.komodo.teiid.client.schema.json;

import java.util.Iterator;
import javax.xml.parsers.SAXParserFactory;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import com.sun.xml.xsom.XSAnnotation;
import com.sun.xml.xsom.XSAttGroupDecl;
import com.sun.xml.xsom.XSAttributeDecl;
import com.sun.xml.xsom.XSAttributeUse;
import com.sun.xml.xsom.XSComplexType;
import com.sun.xml.xsom.XSContentType;
import com.sun.xml.xsom.XSElementDecl;
import com.sun.xml.xsom.XSFacet;
import com.sun.xml.xsom.XSIdentityConstraint;
import com.sun.xml.xsom.XSModelGroup;
import com.sun.xml.xsom.XSModelGroupDecl;
import com.sun.xml.xsom.XSNotation;
import com.sun.xml.xsom.XSParticle;
import com.sun.xml.xsom.XSSchema;
import com.sun.xml.xsom.XSSchemaSet;
import com.sun.xml.xsom.XSSimpleType;
import com.sun.xml.xsom.XSTerm;
import com.sun.xml.xsom.XSWildcard;
import com.sun.xml.xsom.XSXPath;
import com.sun.xml.xsom.parser.AnnotationContext;
import com.sun.xml.xsom.parser.AnnotationParser;
import com.sun.xml.xsom.parser.AnnotationParserFactory;
import com.sun.xml.xsom.parser.XSOMParser;
import com.sun.xml.xsom.visitor.XSVisitor;

/**
 *
 */
public class XsdReader implements XSVisitor {

    private class XsdAnnotationParser extends AnnotationParser {
        private StringBuilder documentation = new StringBuilder();

        @Override
        public ContentHandler getContentHandler(AnnotationContext context, String parentElementName, ErrorHandler handler, EntityResolver resolver) {
            return new ContentHandler() {
                private boolean parsingDocumentation = false;

                @Override
                public void characters(char[] ch, int start, int length) throws SAXException {
                    if (parsingDocumentation) {
                        documentation.append(ch, start, length);
                    }
                }

                @Override
                public void startElement(String uri, String localName, String qName, Attributes atts) throws SAXException {
                    if (localName.equals("documentation")) {
                        parsingDocumentation = true;
                    }
                }

                @Override
                public void endElement(String uri, String localName, String name) throws SAXException {
                    if (localName.equals("documentation")) {
                        parsingDocumentation = false;
                    }
                }

                @Override
                public void ignorableWhitespace(char[] ch, int start, int length) throws SAXException {
                }

                @Override
                public void skippedEntity(String name) throws SAXException {
                }

                @Override
                public void setDocumentLocator(Locator locator) {
                }

                @Override
                public void startDocument() throws SAXException {
                }

                @Override
                public void endDocument() throws SAXException {
                }

                @Override
                public void startPrefixMapping(String prefix, String uri) throws SAXException {
                }

                @Override
                public void endPrefixMapping(String prefix) throws SAXException {
                }

                @Override
                public void processingInstruction(String target, String data) throws SAXException {
                }
            };
        }

        @Override
        public Object getResult(Object existing) {
            return documentation.toString().trim();
        }
    }

    private class AnnotationFactory implements AnnotationParserFactory {
        @Override
        public AnnotationParser create() {
            return new XsdAnnotationParser();
        }
    }

    private StringBuffer buffer = new StringBuffer();

    @Override
    public void wildcard(XSWildcard wc) {
    }

    @Override
    public void modelGroupDecl(XSModelGroupDecl decl) {
    }

    @Override
    public void modelGroup(XSModelGroup group) {
    }

    @Override
    public void simpleType(XSSimpleType simpleType) {
    }

    @Override
    public void empty(XSContentType empty) {
    }

    @Override
    public void annotation(XSAnnotation ann) {
        if(ann == null)
            return;

        Object annoObj = ann.getAnnotation();
        System.out.println(annoObj);
    }

    @Override
    public void attGroupDecl(XSAttGroupDecl decl) {
    }

    @Override
    public void facet(XSFacet facet) {
    }

    @Override
    public void notation(XSNotation notation) {
    }

    @Override
    public void identityConstraint(XSIdentityConstraint decl) {
    }

    @Override
    public void xpath(XSXPath xp) {
    }

    @Override
    public void particle(XSParticle particle) {
        XSTerm term = particle.getTerm();
        if(term.isModelGroup()){
            XSModelGroup xsModelGroup = term.asModelGroup();
            XSParticle[] particles = xsModelGroup.getChildren();
            for(XSParticle p : particles ){
                XSTerm pterm = p.getTerm();
                pterm.visit(this);
            }
        }
    }

    @Override
    public void attributeDecl(XSAttributeDecl decl) {
        System.out.println(decl.getName());
    }

    @Override
    public void attributeUse(XSAttributeUse use) {
        XSAttributeDecl attributeDecl = use.getDecl();
        attributeDecl.visit(this);
    }

    @Override
    public void complexType(XSComplexType type) {
        Iterator<? extends XSAttributeUse> attrUses = type.getAttributeUses().iterator();
        while(attrUses.hasNext()) {
            attrUses.next().visit(this);
        }

        type.getContentType().visit(this);
    }

    @Override
    public void elementDecl(XSElementDecl decl) {
        System.out.println(decl.getName());
        annotation(decl.getAnnotation());

        decl.getType().visit(this);
    }

    @Override
    public void schema(XSSchema schema) {
        Iterator<XSElementDecl> i = schema.iterateElementDecls();
        while (i.hasNext()) {
            XSElementDecl element = i.next();
            element.visit(this);
        }
    }

    public String read() throws Exception {

        XSOMParser parser = new XSOMParser(SAXParserFactory.newInstance());
        parser.setAnnotationParser(new AnnotationFactory());
        parser.parse(this.getClass().getResourceAsStream("teiid-vdb.xsd")); //$NON-NLS-1$

        XSSchemaSet schemas = parser.getResult();
        for (XSSchema schema : schemas.getSchemas()) {
            schema.visit(this);
        }

        return buffer.toString();
    }

    public static void main(String[] args) throws Exception {
        XsdReader reader = new XsdReader();
        System.out.println(reader.read());
    }
}
