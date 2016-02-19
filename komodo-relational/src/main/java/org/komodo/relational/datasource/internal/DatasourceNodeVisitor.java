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
package org.komodo.relational.datasource.internal;

import java.io.StringWriter;
import java.util.Properties;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.datasource.Datasource;
import org.komodo.spi.KException;
import org.komodo.spi.constants.ExportConstants;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;

/**
 * Visitor that will walk a Datasource node and convert it to Datasource xml
 */
public class DatasourceNodeVisitor implements StringConstants {

    private static final int TAB1 = 1;
    private static final int TAB2 = 2;
    private static final int NEW_LINE1 = 1;
    
    private final StringWriter strWriter;
    private XMLStreamWriter writer;
    private final Datasource dataSource;
    private boolean showTabs = false;

    /**
     * Create new visitor that writes to the given xml stream writer
     *
     * @param uow the transaction
     * @param dataSource the Datasource
     * @param exportProperties the properties for export
     * @throws KException if error
     */
    public DatasourceNodeVisitor(final UnitOfWork uow, final Datasource dataSource, final Properties exportProperties) throws KException {
        super();
        this.dataSource = dataSource;
        
        if( exportProperties != null && !exportProperties.isEmpty() ) {
            boolean useTabs = exportProperties.containsKey(ExportConstants.USE_TABS_PROP_KEY);
            setShowTabs(useTabs);
        }
        
        this.strWriter = new StringWriter();
        this.writer = null;
        try {
			final XMLOutputFactory xof = XMLOutputFactory.newInstance();
			this.writer = xof.createXMLStreamWriter(strWriter);
	        
	        dataSource(uow, this.dataSource);
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
     * Get the XML representation of the Datasource
     * @return the xml string
     */
    public String getXml() {
    	if( strWriter != null ) {
    		return this.strWriter.toString();
    	}
    	
    	return EMPTY_STRING;
    }

    private void dataSource(final UnitOfWork uow, Datasource dataSource) throws Exception {
        // Start new document
        writeStartDocument();

        // ==============================
        // DatasourceSet element
        // ==============================
        writeStartElement(Datasource.XML_ELEM_DATASOURCE_SET);
        writeNewLine(NEW_LINE1);

        // -----------------------------------------------------------------
        // Datasource element
        // -----------------------------------------------------------------
        if(showTabs) writeTab(TAB1);
        writeStartElement(Datasource.XML_ELEM_DATASOURCE);
        // Name attribute
        writeAttribute(Datasource.XML_ATTR_NAME, dataSource.getName(uow));
        // jdbc attribute
        writeAttribute(Datasource.XML_ATTR_JDBC, Boolean.toString(dataSource.isJdbc(uow)));

        writeNewLine(NEW_LINE1);

        // Write property elements
        int nsPrefixLength = (KomodoLexicon.Namespace.PREFIX+StringConstants.COLON).length();
        
        String[] propNames = dataSource.getPropertyNames(uow);
        for(String propName : propNames) {
            Property prop = dataSource.getProperty(uow, propName);
            if(prop!=null) {
                // Do not export properties with the "tko:" prefix
                if( isKnownProperty(propName) ) {
                    propName = propName.substring(nsPrefixLength);
                }
                String propValue = prop.getStringValue(uow);
                if(!StringUtils.isBlank(propValue) && !propName.equals(Datasource.XML_ATTR_JDBC) ) {
                    if(showTabs) writeTab(TAB2);
                    writePropertyElement(propName, propValue);
                }
            }
        }
        if(showTabs) writeTab(TAB1);
        writeEndElement();
        // -----------------------------------------------------------------
        // End Datasource element
        // -----------------------------------------------------------------

        writeEndElement();
        // ==============================
        // End DatasourceSet element
        // ==============================
        
        writeEndDocument();
    }
    
    private boolean isKnownProperty ( final String propName ) {
        if( propName.equals(KomodoLexicon.DataSource.CLASS_NAME) ||
            propName.equals(KomodoLexicon.DataSource.DRIVER_NAME) ||
            propName.equals(KomodoLexicon.DataSource.JDBC) ||
            propName.equals(KomodoLexicon.DataSource.JNDI_NAME) ||
            propName.equals(KomodoLexicon.DataSource.PREVIEW) ||
            propName.equals(KomodoLexicon.DataSource.PROFILE_NAME) ) return true;
        
        return false;
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
        writeStartElement(Datasource.XML_ELEM_PROPERTY);
        writeAttribute(Datasource.XML_ATTR_NAME, propName);
        writeCharacters(propValue);
        writeEndElement();
    }

    private void writeEndDocument() throws XMLStreamException {
        writer.writeEndDocument();
        writer.close();
    }

}
