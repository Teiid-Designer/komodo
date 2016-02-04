/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.datasource.internal;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Stack;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import org.komodo.relational.Messages;
import org.komodo.relational.datasource.Datasource;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * The <code>DatasourceValidationParser</code> parses a data source definition file.  
 * This parser validates a file and returns the names of data sources in the file.
 */
public class DatasourceValidationParser {

    private final String DATASOURCE_SCHEMA_FILE = "datasource.xsd"; //$NON-NLS-1$

    private static final KLog LOGGER = KLog.getLogger();

    private Handler handler;
    /**
     * The parser of the data sources file
     */
    private SAXParser parser;

    /**
     * Constructs a parser.
     * @throws Exception if there were problems with the data sources schema file
     */
    public DatasourceValidationParser( ) throws Exception {
        initParser( );
    }
    
    private void initParser( ) throws Exception {
        InputStream schemaStream = getClass().getClassLoader().getResourceAsStream(DATASOURCE_SCHEMA_FILE);
        File schemaFile = null;
        try {
            schemaFile = File.createTempFile("datasourceSchemaFile", ".xsd"); //$NON-NLS-1$  //$NON-NLS-2$
            Files.copy(schemaStream, schemaFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
        } catch (IOException ex) {
            LOGGER.error("DatasourceValidationParser - error processing the data sources schema : ", ex); //$NON-NLS-1$
        }
        schemaFile.deleteOnExit();

        // create parser
        final SAXParserFactory factory = SAXParserFactory.newInstance();
        factory.setNamespaceAware( true );
        factory.setValidating( true );

        this.parser = factory.newSAXParser();
        this.parser.setProperty( "http://java.sun.com/xml/jaxp/properties/schemaLanguage", "http://www.w3.org/2001/XMLSchema" ); //$NON-NLS-1$ //$NON-NLS-2$
        this.parser.setProperty( "http://java.sun.com/xml/jaxp/properties/schemaSource", schemaFile ); //$NON-NLS-1$
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
     * @param datasourceFile the data sources file (cannot be <code>null</code>)
     * @return the Data source names (never <code>null</code>)
     * @throws Exception if the definition file is <code>null</code> or if there is a problem parsing the file
     */
    public String[] parse( final UnitOfWork transaction, final File datasourceFile ) throws Exception {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( datasourceFile, "datasourceFile" ); //$NON-NLS-1$
        
        this.handler = new Handler(transaction);
        this.parser.parse(datasourceFile, handler);
        return this.handler.getDatasourceNames();
    }

    /**
     * The handler used by the parser. Each instance should be only used to parse one file.
     */
    class Handler extends DefaultHandler {

        private final List<String> fatals;
        private final List<String> errors;
        private final List<String> infos;
        private final List<String> warnings;
        
        private final Collection<String> dataSourceNames;
        
        private final Stack<String> elements;
        private String datasourceName;

        public Handler( final UnitOfWork uow ) {
            this.elements = new Stack<String>();
            this.fatals = new ArrayList<String>();
            this.errors = new ArrayList<String>();
            this.infos = new ArrayList<String>();
            this.warnings = new ArrayList<String>();
            this.dataSourceNames = new ArrayList<String>();
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
                if(datasourceName!=null) dataSourceNames.add(datasourceName);
                
                this.datasourceName = null;
            }

            this.elements.pop();

            super.endElement(uri, localName, qName);
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
                this.dataSourceNames.clear();
            } else if(Datasource.XML_ELEM_DATASOURCE.equals(localName)) {
                this.datasourceName = attributes.getValue(Datasource.XML_ATTR_NAME);
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
