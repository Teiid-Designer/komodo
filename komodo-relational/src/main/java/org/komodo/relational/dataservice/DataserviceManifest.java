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
package org.komodo.relational.dataservice;

import java.io.InputStream;
import java.io.StringWriter;
import java.util.Properties;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.stream.XMLStreamWriter;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.dataservice.internal.DataserviceImpl;
import org.komodo.relational.vdb.Vdb.VdbManifest;
import org.komodo.spi.KException;
import org.komodo.spi.repository.DocumentType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.FileUtils;
import org.komodo.utils.KLog;
import org.modeshape.jcr.api.JcrConstants;
import org.w3c.dom.Document;

public class DataserviceManifest implements VdbManifest {

    public static final String MANIFEST = "META-INF/dataservice.xml";

    protected static final KLog LOGGER = KLog.getLogger();

    private final Dataservice dataService;

    private String xml;

    public DataserviceManifest(final UnitOfWork transaction, final Dataservice dataService)
        throws KException {
        this.dataService = dataService;
    }

    private void writeNewLine(XMLStreamWriter writer) throws XMLStreamException {
        writer.writeCharacters(DataserviceImpl.NEW_LINE);
    }

    private void writeTab(XMLStreamWriter writer) throws XMLStreamException {
        writer.writeCharacters(DataserviceImpl.TAB);
    }

    @Override
    public String getName(UnitOfWork transaction) throws KException {
        return dataService.getName(transaction);
    }

    @Override
    public DocumentType getDocumentType(UnitOfWork uow) throws KException {
        return DocumentType.XML;
    }

    private static XMLStreamReader createReader(InputStream srcStream) throws Exception {
        XMLInputFactory factory = XMLInputFactory.newInstance();
        factory.setProperty(XMLInputFactory.IS_NAMESPACE_AWARE, false);
        return factory.createXMLStreamReader(srcStream);
    }

    /**
     * @param srcStream
     * @return Finds the data service name and returns it from the xml
     * @throws KException
     */
    public static String extractName(InputStream srcStream) throws KException {
        XMLStreamReader reader = null;
        try {
            reader = createReader(srcStream);

            String elementName = null;
            while (reader.hasNext()) {
                switch (reader.getEventType()) {
                    case XMLStreamReader.START_ELEMENT:
                        elementName = reader.getLocalName();
                        break;
                    case XMLStreamReader.END_ELEMENT:
                        elementName = null;
                        break;
                    case XMLStreamReader.CHARACTERS:
                        String text = reader.getText();

                        if (elementName == null)
                            break;

                        if (text.trim().isEmpty())
                            break;

                        if (JcrConstants.JCR_NAME.equals(elementName))
                            return text;

                    default:
                        // Nothing required
                }
                reader.next();
            }

        } catch (Exception ex) {
            throw new KException(ex);
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (Exception ex) {
                    // Do Nothing
                }
            }
        }

        return null;
    }

    public void read(UnitOfWork transaction, InputStream srcStream) throws KException {
        XMLStreamReader reader = null;
        try {
            reader = createReader(srcStream);

            String elementName = null;
            while (reader.hasNext()) {
                switch (reader.getEventType()) {
                    case XMLStreamReader.START_ELEMENT:
                        elementName = reader.getLocalName();
                        break;
                    case XMLStreamReader.END_ELEMENT:
                        elementName = null;
                        break;
                    case XMLStreamReader.CHARACTERS:
                        String text = reader.getText();

                        if (elementName == null)
                            break;

                        if (text.trim().isEmpty())
                            break;

                        if (JcrConstants.JCR_NAME.equals(elementName))
                            break; // Ignore as already been set

                        dataService.setProperty(transaction, elementName, text);
                        break;
                    default:
                        // Nothing required
                }

                reader.next();
            }

        } catch (Exception ex) {
            throw new KException(ex);
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (Exception ex) {
                    // Do Nothing
                }
            }
        }
    }

    @Override
    public byte[] export(UnitOfWork transaction, Properties properties) throws KException {
        StringWriter writer = new StringWriter();

        XMLStreamWriter xsw = null;
        try {
            XMLOutputFactory xof = XMLOutputFactory.newInstance();
            xsw = xof.createXMLStreamWriter(writer);

            xsw.writeStartDocument("UTF-8", "1.0"); //$NON-NLS-1$ //$NON-NLS-2$
            writeNewLine(xsw);

            // Root DataService element
            xsw.writeStartElement(KomodoLexicon.DataService.NODE_TYPE);
            writeNewLine(xsw);

            writeTab(xsw);
            xsw.writeStartElement(JcrConstants.JCR_NAME);
            xsw.writeCharacters(getName(transaction));
            xsw.writeEndElement();
            writeNewLine(xsw);

            String[] names = dataService.getPropertyNames(transaction);
            for (String name : names) {
                Property property = dataService.getProperty(transaction, name);
                Object value = property.getValue(transaction);

                if (value == null)
                    continue;

                writeTab(xsw);
                xsw.writeStartElement(name);
                xsw.writeCharacters(value.toString());
                xsw.writeEndElement();
                writeNewLine(xsw);
            }

            // End DataService element
            xsw.writeEndElement();
            writeNewLine(xsw);

            xsw.writeEndDocument();

        } catch (final Exception e) {
            throw new KException(e);
        } finally {
            if (xsw != null) {
                try {
                    xsw.close();
                } catch (Exception ex) {
                    // Do Nothing
                }
            }
        }

        // Create an XML Document from the filled writer
        this.xml = writer.toString().trim();

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug("VdbImpl#VdbManifestImpl: transaction = {0}, xml = {1}", //$NON-NLS-1$
                         transaction.getName(),
                         this.xml);
        }
        return this.xml == null ? new byte[0] : this.xml.getBytes();
    }

    @Override
    public Document asDocument() throws KException {
        return FileUtils.createDocument(xml);
    }
}
