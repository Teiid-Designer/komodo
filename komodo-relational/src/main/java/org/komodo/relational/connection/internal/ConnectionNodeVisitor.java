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
package org.komodo.relational.connection.internal;

import java.io.StringWriter;
import java.util.Properties;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;

import org.komodo.relational.connection.Connection;
import org.komodo.spi.KException;
import org.komodo.spi.constants.ExportConstants;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

/**
 * Visitor that will walk a Connection node and convert it to Connection xml
 */
public class ConnectionNodeVisitor implements StringConstants {

    private static final int TAB1 = 1;
    private static final int TAB2 = 2;
    private static final int NEW_LINE1 = 1;

    private final StringWriter strWriter;
    private XMLStreamWriter writer;
    private final Connection connection;
    private boolean showTabs = false;

    /**
     * Create new visitor that writes to the given xml stream writer
     *
     * @param uow the transaction
     * @param connection the connection
     * @param exportProperties the properties for export
     * @throws KException if error
     */
    public ConnectionNodeVisitor(final UnitOfWork uow, final Connection connection, final Properties exportProperties) throws KException {
        super();
        this.connection = connection;

        if( exportProperties != null && !exportProperties.isEmpty() ) {
            boolean useTabs = exportProperties.containsKey(ExportConstants.USE_TABS_PROP_KEY);
            setShowTabs(useTabs);
        }

        this.strWriter = new StringWriter();
        this.writer = null;
        try {
			final XMLOutputFactory xof = XMLOutputFactory.newInstance();
			this.writer = xof.createXMLStreamWriter(strWriter);

	        connection(uow, this.connection);
		} catch (Exception e) {
		    throw new KException(e);
		} finally {
		    if(this.writer!=null) {
		        try {
                    this.writer.close();
                } catch (XMLStreamException ex) {
                    throw new KException(ex);
                }
		    }
		}
    }

    /**
     * Determines whether to tab the xml into a more readable format.
     * @param showTabs <code>true</code> will tab the generated xml.
     */
    public void setShowTabs( boolean showTabs ) {
        this.showTabs = showTabs;
    }

    /**
     * Get the XML representation of the Connection
     * @return the xml string
     */
    public String getXml() {
    	if( strWriter != null ) {
    		return this.strWriter.toString();
    	}

    	return EMPTY_STRING;
    }

    private void connection(final UnitOfWork uow, Connection connection) throws Exception {
        // Start new document
        writeStartDocument();

        // -----------------------------------------------------------------
        // Data source element
        // -----------------------------------------------------------------
        final String dsType = ( connection.isJdbc( uow ) ? DataVirtLexicon.ConnectionXmlId.JDBC_CONNECTION
                                                         : DataVirtLexicon.ConnectionXmlId.RESOURCE_CONNECTION );
        writeStartElement( dsType );
        // Name attribute
        writeAttribute(DataVirtLexicon.ConnectionXmlId.NAME_ATTR, connection.getName(uow));

        writeNewLine(NEW_LINE1);

        // JNDI element
        if ( this.showTabs ) {
            writeTab( TAB1 );
        }

        writeStartElement( DataVirtLexicon.ConnectionXmlId.JNDI_NAME );
        writeCharacters( connection.getJndiName( uow ) );
        writeEndElement();

        // driver name element
        if ( this.showTabs ) {
            writeTab( TAB1 );
        }

        writeStartElement( DataVirtLexicon.ConnectionXmlId.DRIVER_NAME );
        writeCharacters( connection.getDriverName( uow ) );
        writeEndElement();

        // Write property elements
        String[] propNames = connection.getPropertyNames(uow);
        for(String propName : propNames) {
            Property prop = connection.getProperty(uow, propName);
            if(prop!=null) {
                // Do not export properties that have there own element
                if( isKnownProperty(propName) ) {
                    continue;
                }
                String propValue = prop.getStringValue(uow);
                if(!StringUtils.isBlank(propValue) ) {
                    if(showTabs) writeTab(TAB2);
                    writePropertyElement(propName, propValue);
                }
            }
        }

        // driver class element
        if ( !connection.isJdbc( uow ) ) {
            if ( this.showTabs ) {
                writeTab( TAB1 );
            }

            writeStartElement( DataVirtLexicon.ConnectionXmlId.CLASSNAME );
            writeCharacters( connection.getClassName( uow ) );
            writeEndElement();
        }

        // -----------------------------------------------------------------
        // End Data source element
        // -----------------------------------------------------------------
        writeEndElement();

        writeEndDocument();
    }

    private boolean isKnownProperty( final String propName ) {
        return ( propName.equals( DataVirtLexicon.Connection.CLASS_NAME )
                 || propName.equals( DataVirtLexicon.Connection.DRIVER_NAME )
                 || propName.equals( DataVirtLexicon.Connection.TYPE )
                 || propName.equals( DataVirtLexicon.Connection.JNDI_NAME ) );
    }

    private void writeNewLine(int total) throws XMLStreamException {
        for (int i = 0; i < total; ++i)
            writer.writeCharacters(NEW_LINE);
    }

    private void writeNewLine() throws XMLStreamException {
        writeNewLine(1);
    }

    private void writeTab(int total) throws XMLStreamException {
    	for (int i = 0; i < total; ++i)
    		writer.writeCharacters(TAB);
    }

    private void writeStartDocument() throws XMLStreamException {
        writer.writeStartDocument("UTF-8", "1.0"); //$NON-NLS-1$ //$NON-NLS-2$
        writeNewLine();
    }

    private void writeStartElement(String tag) throws XMLStreamException {
        writer.writeStartElement(tag);
    }

    private void writeAttribute(String name, String value) throws XMLStreamException {
        writer.writeAttribute(name, value);
    }

    private void writeCharacters(String characters) throws XMLStreamException {
        writer.writeCharacters(characters);
    }

    private void writeEndElement() throws XMLStreamException {
        writer.writeEndElement();
        writeNewLine();
    }

    private void writePropertyElement(String propName, String propValue) throws XMLStreamException {
        writeStartElement(DataVirtLexicon.ConnectionXmlId.PROPERTY);
        writeAttribute(DataVirtLexicon.ConnectionXmlId.NAME_ATTR, propName);
        writeCharacters(propValue);
        writeEndElement();
    }

    private void writeEndDocument() throws XMLStreamException {
        writer.writeEndDocument();
        writer.close();
    }

}
