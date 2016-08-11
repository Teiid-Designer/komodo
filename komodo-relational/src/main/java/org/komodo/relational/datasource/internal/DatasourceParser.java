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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.Messages;
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * The <code>DatasourceParser</code> parses a data source definition file.  
 * This parser generates the Data sources in the repository and return the array of generated data sources.
 */
public class DatasourceParser {

    /**
     * Parser options for determining behaviour of the parser
     */
    public static enum ParserOption {

        /**
         * Create data sources beneath parent while parsing
         */
        CREATE_REPO_SOURCES,

        /**
         * If a data source already exists beneath parent then replace it
         */
        REPLACE_REPO_SOURCE;
    }

    private final String DATASOURCE_SCHEMA_FILE = "datasource.xsd"; //$NON-NLS-1$

    private static final KLog LOGGER = KLog.getLogger();
    
    private Handler handler;
    /**
     * The parser of the data sources file
     */
    private SAXParser parser;

    /**
     * Constructs a parser.
     * @param replaceExisting 
     *        <code>true</code> to replace sources if sources with same names already exist
     * @throws KException if there were problems with the data sources schema file
     */
    public DatasourceParser() throws KException {
        initParser( );
    }
    
    private void initParser( ) throws KException {
        InputStream schemaStream = getClass().getClassLoader().getResourceAsStream(DATASOURCE_SCHEMA_FILE);
        File schemaFile = null;
        try {
            schemaFile = File.createTempFile("datasourceSchemaFile", ".xsd"); //$NON-NLS-1$  //$NON-NLS-2$
            Files.copy(schemaStream, schemaFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
        } catch (IOException ex) {
            LOGGER.error("DatasourceParser - error processing the data sources schema : ", ex); //$NON-NLS-1$
        }
        schemaFile.deleteOnExit();

        // create parser
        final SAXParserFactory factory = SAXParserFactory.newInstance();
        factory.setNamespaceAware( true );
        factory.setValidating( true );

        try {
            this.parser = factory.newSAXParser();
            this.parser.setProperty( "http://java.sun.com/xml/jaxp/properties/schemaLanguage", "http://www.w3.org/2001/XMLSchema" ); //$NON-NLS-1$ //$NON-NLS-2$
            this.parser.setProperty( "http://java.sun.com/xml/jaxp/properties/schemaSource", schemaFile ); //$NON-NLS-1$
        } catch (Exception ex) {
            throw RelationalModelFactory.handleError(ex);
        }
    }

    /**
     * @return the error messages output from the last parse operation 
     *   or <code>null</code> if parse has not been called
     */
    public List<String> getErrors() {
        if (this.handler == null) {
            return null;
        }

        return this.handler.getErrors();
    }

    /**
     * @return the fatal error messages output from the last parse operation 
     *   or <code>null</code> if parse has not been called
     */
    public List<String> getFatalErrors() {
        if (this.handler == null) {
            return null;
        }

        return this.handler.getFatalErrors();
    }

    /**
     * @return the information messages output from the last parse operation 
     *   or <code>null</code> if parse has not been called
     */
    public List<String> getInfos() {
        if (this.handler == null) {
            return null;
        }

        return this.handler.getInfos();
    }

    /**
     * @return the warning messages output from the last parse operation 
     *   or <code>null</code> if parse has not been called
     */
    public List<String> getWarnings() {
        if (this.handler == null) {
            return null;
        }

        return this.handler.getWarnings();
    }

    /**
     * @param transaction the transaction (cannot be <code>null</code>)
     * @param parent the prospective parent of the data sources
     * @param datasourceStream the data sources input stream (cannot be <code>null</code>)
     * @param options the options to determine whether to create datasources in the parent's repository
     *                   see {@link ParserOption}s for details of options
     * @return the Data sources (never <code>null</code>)
     * @throws KException if the definition file is <code>null</code> or if there is a problem parsing the file
     */
    public Datasource[] parse( final UnitOfWork transaction, final KomodoObject parent,
                                                     final InputStream datasourceStream, EnumSet<ParserOption> options )
                                                     throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( datasourceStream, "datasourceStream" ); //$NON-NLS-1$
        ArgCheck.isNotNull( parent, "parent" ); //$NON-NLS-1$

        if (options == null)
            options = EnumSet.allOf(ParserOption.class);

        this.handler = new Handler(transaction, parent, options);
        try {
            this.parser.parse(datasourceStream, handler);
            return this.handler.getDatasources();
        } catch (Exception ex) {
            throw RelationalModelFactory.handleError(ex);
        }
    }

    /**
     * @param transaction the transaction (cannot be <code>null</code>)
     * @param parent the prospective parent of the data sources
     * @param datasourceFile the data sources file (cannot be <code>null</code>)
     * @param options the options to determine whether to create datasources in the parent's repository
     *                   see {@link ParserOption}s for details of options
     * @return the Data sources (never <code>null</code>)
     * @throws KException if the definition file is <code>null</code> or if there is a problem parsing the file
     */
    public Datasource[] parse( final UnitOfWork transaction, final KomodoObject parent,
                                                     final File datasourceFile, EnumSet<ParserOption> options )
                                                     throws KException {
        ArgCheck.isNotNull( datasourceFile, "datasourceFile" ); //$NON-NLS-1$

        InputStream fileStream = null;
        try {
            fileStream = new FileInputStream(datasourceFile);
            return parse(transaction, parent, fileStream, options);
        } catch (Exception ex) {
            throw RelationalModelFactory.handleError(ex);
        } finally {
            if (fileStream != null)
                try {
                    fileStream.close();
                } catch (IOException e) {
                    // Nothing to do
                }
        }
    }

    /**
     * @param transaction the transaction (cannot be <code>null</code>)
     * @param parent the prospective parent of the data sources
     * @param datasourceFile the data sources file (cannot be <code>null</code>)
     * @return the Data sources (never <code>null</code>)
     * @throws KException if the definition file is <code>null</code> or if there is a problem parsing the file
     */
    public Datasource[] parse( final UnitOfWork transaction, final KomodoObject parent,
                                                     final File datasourceFile ) throws KException {
        return parse(transaction, parent, datasourceFile, null);
    }

    /**
     * @param datasourceStream the data sources stream (cannot be <code>null</code>)
     * @return the Data sources (never <code>null</code>)
     * @throws KException if the definition file is <code>null</code> or if there is a problem parsing the file
     */
    public String[] validate( final InputStream datasourceStream ) throws KException {
        ArgCheck.isNotNull( datasourceStream, "datasourceStream" ); //$NON-NLS-1$

        this.handler = new Handler();
        try {
            this.parser.parse(datasourceStream, handler);
            return this.handler.getDatasourceNames();
        } catch (Exception ex) {
            throw RelationalModelFactory.handleError(ex);
        }
    }

    /**
     * @param datasourceFile the data sources file (cannot be <code>null</code>)
     * @return the Data sources (never <code>null</code>)
     * @throws KException if the definition file is <code>null</code> or if there is a problem parsing the file
     */
    public String[] validate( final File datasourceFile ) throws KException {
        ArgCheck.isNotNull( datasourceFile, "datasourceFile" ); //$NON-NLS-1$

        InputStream fileStream = null;
        try {
            fileStream = new FileInputStream(datasourceFile);
            return validate(fileStream);
        } catch (Exception ex) {
            throw RelationalModelFactory.handleError(ex);
        } finally {
            if (fileStream != null)
                try {
                    fileStream.close();
                } catch (IOException e) {
                    // Nothing to do
                }
        }
    }

    /**
     * The handler used by the parser. Each instance should be only used to parse one file.
     */
    class Handler extends DefaultHandler {

        private final List<String> fatals;
        private final List<String> errors;
        private final List<String> infos;
        private final List<String> warnings;
        
        private final Collection<Datasource> dataSources;
        private final Collection<String> dataSourceNames;
        private final Map<String,String> propertyMap;

        private StringBuilder propertyValue = new StringBuilder();
        
        private final Stack<String> elements;
        private String datasourceName;
        private String propertyName;
        private String jdbc;
        private UnitOfWork uow;
        private KomodoObject parent;
        private final EnumSet<ParserOption> options;

        public Handler( final UnitOfWork uow, KomodoObject parent, EnumSet<ParserOption> options) {
            this.elements = new Stack<String>();
            this.fatals = new ArrayList<String>();
            this.errors = new ArrayList<String>();
            this.infos = new ArrayList<String>();
            this.warnings = new ArrayList<String>();
            this.dataSources = new ArrayList<Datasource>();
            this.propertyMap = new HashMap<String, String>();
            this.dataSourceNames = new ArrayList<String>();
            this.uow = uow;
            this.parent = parent;
            this.options = options;
        }

        public Handler() {
            this(null, null, EnumSet.noneOf(ParserOption.class));
        }

        /**
         * {@inheritDoc}
         * 
         * @see org.xml.sax.helpers.DefaultHandler#characters(char[], int, int)
         */
        @Override
        public void characters( char[] ch,
                                int start,
                                int length ) throws SAXException {
            String value = new String(ch, start, length);

            if (Datasource.XML_ELEM_PROPERTY.equals(getCurrentElement())) {
                this.propertyValue.append(value);
            }

            super.characters(ch, start, length);
        }

        /**
         * {@inheritDoc}
         * 
         * @see org.xml.sax.helpers.DefaultHandler#endElement(java.lang.String, java.lang.String, java.lang.String)
         */
        @Override
        public void endElement( String uri,
                                String localName,
                                String qName ) throws SAXException {

            if(Datasource.XML_ELEM_DATASOURCE.equals(localName)) {
                if(options.contains(ParserOption.CREATE_REPO_SOURCES)) {
                    // Determine if source with same name already exists
                    KomodoObject[] existingSource = null;
                    try {
                        ArgCheck.isNotNull(parent, "parent");
                        existingSource = parent.getChildrenOfType(uow, KomodoLexicon.DataSource.NODE_TYPE, datasourceName);
                    } catch (KException ex1) {
                        error(new SAXParseException(ex1.getMessage(), null));
                        LOGGER.error("DatasourceParser - error fetching Datasource : ", ex1); //$NON-NLS-1$
                    }

                    Datasource theDatasource = null;
                    if(existingSource!=null && existingSource.length>0) {
                        if(options.contains(ParserOption.REPLACE_REPO_SOURCE)) {
                            if(deleteDatasource(existingSource[0])) {
                                theDatasource = createDatasource(parent);
                                if(theDatasource!=null) dataSources.add(theDatasource);
                            }
                        }
                    } else {
                        theDatasource = createDatasource(parent);
                        if(theDatasource!=null) dataSources.add(theDatasource);
                    }
                }
                
                if(datasourceName!=null) dataSourceNames.add(datasourceName);
                this.datasourceName = null;
                this.jdbc = null;
                this.propertyMap.clear();
            } else if(Datasource.XML_ELEM_PROPERTY.equals(localName)) {
                this.propertyMap.put(this.propertyName, this.propertyValue.toString());
                this.propertyName = null;
                this.propertyValue.setLength(0);
            }

            this.elements.pop();

            super.endElement(uri, localName, qName);
        }

        private boolean deleteDatasource(KomodoObject source) {
            try {
                WorkspaceManager.getInstance( source.getRepository(), uow ).delete(uow, source);
            } catch (KException ex) {
                error(new SAXParseException(ex.getMessage(), null));
                LOGGER.error("DatasourceParser - error deleting existing Datasource : ", ex); //$NON-NLS-1$
                return false;
            }
            return true;
        }
        
        private Datasource createDatasource(KomodoObject parent) {
            Datasource theDatasource = null;
            try {
                String path = parent.getAbsolutePath();
                Repository repo = parent.getRepository();
                theDatasource = RelationalModelFactory.createDatasource(uow, repo, path, datasourceName);
                
                // Set jdbc from attribute
                boolean isJdbc = true;
                if( this.jdbc != null ) {
                    isJdbc = Boolean.parseBoolean(this.jdbc);
                }
                theDatasource.setJdbc(uow, isJdbc);
                
                // Set remaining properties
                for(String propName : this.propertyMap.keySet()) {
                    String propValue = this.propertyMap.get(propName);
                    theDatasource.setProperty(uow, propName, propValue);
                }
            } catch (KException ex) {
                error(new SAXParseException(ex.getMessage(), null));
                LOGGER.error("DatasourceParser - error creating Datasource : ", ex); //$NON-NLS-1$
            }
            return theDatasource;
        }
        
        /**
         * {@inheritDoc}
         * 
         * @see org.xml.sax.helpers.DefaultHandler#error(org.xml.sax.SAXParseException)
         */
        @Override
        public void error( SAXParseException e ) {
            this.errors.add(e.getLocalizedMessage());
        }

        /**
         * {@inheritDoc}
         * 
         * @see org.xml.sax.helpers.DefaultHandler#fatalError(org.xml.sax.SAXParseException)
         */
        @Override
        public void fatalError( SAXParseException e ) {
            this.fatals.add(e.getLocalizedMessage());
        }
        
        /**
         * {@inheritDoc}
         * 
         * @see org.xml.sax.helpers.DefaultHandler#warning(org.xml.sax.SAXParseException)
         */
        @Override
        public void warning( SAXParseException e ) {
            this.warnings.add(e.getLocalizedMessage());
        }

        /**
         * @return the element currently being parsed
         */
        private String getCurrentElement() {
            if (this.elements.empty()) {
                return null;
            }

            return this.elements.peek();
        }

        /**
         * @return the error messages output from the last parse operation (never <code>null</code> but can be empty)
         */
        List<String> getErrors() {
            return this.errors;
        }

        /**
         * @return the fatal error messages output from the last parse operation (never <code>null</code> but can be empty)
         */
        List<String> getFatalErrors() {
            return this.fatals;
        }

        /**
         * @return the information messages output from the last parse operation (never <code>null</code> but can be empty)
         */
        List<String> getInfos() {
            return this.infos;
        }

        /**
         * @return the warning messages output from the last parse operation (never <code>null</code> but can be empty)
         */
        List<String> getWarnings() {
            return this.warnings;
        }

        /**
         * @return the Datasources (never <code>null</code> but can be empty)
         */
        Datasource[] getDatasources() {
            return dataSources.toArray( new Datasource[ dataSources.size() ] );
        }

        /**
         * @return the Datasource Names (never <code>null</code> but can be empty)
         */
        String[] getDatasourceNames() {
            return dataSourceNames.toArray( new String[ dataSourceNames.size() ] );
        }
        
        /**
         * {@inheritDoc}
         * 
         * @see org.xml.sax.helpers.DefaultHandler#startElement(java.lang.String, java.lang.String, java.lang.String,
         *      org.xml.sax.Attributes)
         */
        @Override
        public void startElement( String uri,
                                  String localName,
                                  String qName,
                                  Attributes attributes ) throws SAXException {

            this.elements.push(localName);

            if(Datasource.XML_ELEM_DATASOURCE_SET.equals(localName)) {
                this.dataSources.clear();
            } else if(Datasource.XML_ELEM_DATASOURCE.equals(localName)) {
                this.datasourceName = attributes.getValue(Datasource.XML_ATTR_NAME);
                this.jdbc = attributes.getValue(Datasource.XML_ATTR_JDBC);
                this.propertyMap.clear();
            } else if(Datasource.XML_ELEM_PROPERTY.equals(localName)) {
                this.propertyName = attributes.getValue(Datasource.XML_ATTR_NAME);
            }
                        
            super.startElement(uri, localName, qName, attributes);
        }

        /**
         * {@inheritDoc}
         * 
         * @see org.xml.sax.helpers.DefaultHandler#skippedEntity(java.lang.String)
         */
        @Override
        public void skippedEntity( String name ) {
            this.infos.add( Messages.getString( Messages.DatasourceParser.SCHEMA_FILE_NOT_FOUND, name ) );
        }

        /**
         * {@inheritDoc}
         * 
         * @see org.xml.sax.helpers.DefaultHandler#unparsedEntityDecl(java.lang.String, java.lang.String, java.lang.String,
         *      java.lang.String)
         */
        @Override
        public void unparsedEntityDecl( String name,
                                        String publicId,
                                        String systemId,
                                        String notationName ) {
            this.infos.add( Messages.getString( Messages.DatasourceParser.SCHEMA_FILE_NOT_FOUND, name ) );
        }

    }

}
