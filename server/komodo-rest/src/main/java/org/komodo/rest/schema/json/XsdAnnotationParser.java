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
package org.komodo.rest.schema.json;

import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.Locator;
import com.sun.xml.xsom.parser.AnnotationContext;
import com.sun.xml.xsom.parser.AnnotationParser;

class XsdAnnotationParser extends AnnotationParser {

    private static final String DOCUMENTATION = "documentation"; //$NON-NLS-1$

    private StringBuilder docBuffer = new StringBuilder();

    @Override
    public ContentHandler getContentHandler(AnnotationContext context, String parentElementName, ErrorHandler handler, EntityResolver resolver) {
        return new ContentHandler() {
            private boolean parsingDocumentation = false;

            @Override
            public void characters(char[] ch, int start, int length) {
                if (parsingDocumentation) {
                    docBuffer.append(ch, start, length);
                }
            }

            @Override
            public void startElement(String uri, String localName, String qName, Attributes atts) {
                if (localName.equals(DOCUMENTATION)) {
                    parsingDocumentation = true;
                }
            }

            @Override
            public void endElement(String uri, String localName, String name) {
                if (localName.equals(DOCUMENTATION)) {
                    parsingDocumentation = false;
                }
            }

            @Override
            public void ignorableWhitespace(char[] ch, int start, int length) {
                // Not required
            }

            @Override
            public void skippedEntity(String name) {
                // Not required
            }

            @Override
            public void setDocumentLocator(Locator locator) {
                // Not required
            }

            @Override
            public void startDocument() {
                // Not required
            }

            @Override
            public void endDocument() {
                // Not required
            }

            @Override
            public void startPrefixMapping(String prefix, String uri) {
                // Not required
            }

            @Override
            public void endPrefixMapping(String prefix) {
                // Not required
            }

            @Override
            public void processingInstruction(String target, String data) {
                // Not required
            }
        };
    }

    @Override
    public Object getResult(Object existing) {
        return docBuffer.toString().trim();
    }
}
