/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.repository.test;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.komodo.repository.ObjectImpl;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

@SuppressWarnings( {"javadoc", "nls"} )
public final class KomodoValidationTest {

    class Handler extends DefaultHandler {

        private final List< String > errors = new ArrayList<>();
        private final List< String > fatals = new ArrayList<>();

        /**
         * {@inheritDoc}
         *
         * @see org.xml.sax.helpers.DefaultHandler#error(org.xml.sax.SAXParseException)
         */
        @Override
        public void error( final SAXParseException e ) {
            this.errors.add( e.getLocalizedMessage() );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.xml.sax.helpers.DefaultHandler#fatalError(org.xml.sax.SAXParseException)
         */
        @Override
        public void fatalError( final SAXParseException e ) {
            this.fatals.add( e.getLocalizedMessage() );
        }

        /**
         * @return the error messages output from the last parse operation (never <code>null</code> but can be empty)
         */
        List< String > getErrors() {
            return this.errors;
        }

        /**
         * @return the fatal error messages output from the last parse operation (never <code>null</code> but can be empty)
         */
        List< String > getFatalErrors() {
            return this.fatals;
        }

    }

    private static SAXParser _parser;
    private static final String XSD_PATH = "komodoValidation.xsd";

    @BeforeClass
    public static void oneTimeSetup() throws Exception {
        final SAXParserFactory factory = SAXParserFactory.newInstance();
        factory.setNamespaceAware( true );
        factory.setValidating( true );

        _parser = factory.newSAXParser();
        _parser.setProperty( "http://java.sun.com/xml/jaxp/properties/schemaLanguage", "http://www.w3.org/2001/XMLSchema" ); //$NON-NLS-1$ //$NON-NLS-2$

        final URL schemaUrl = ObjectImpl.class.getResource( XSD_PATH );

        if (schemaUrl == null) {
            throw new IllegalStateException( "Validation schema not found" );
        }

        final File schemaFile = new File( schemaUrl.getFile() );

        if (!schemaFile.exists()) {
            throw new IllegalStateException( "Validation schema file does not exist" );
        }

        _parser.setProperty( "http://java.sun.com/xml/jaxp/properties/schemaSource", schemaFile ); //$NON-NLS-1$
    }

    private Handler handler;

    @Before
    public void init() {
        this.handler = new Handler();
    }

    @Test
    public void shouldParseValidationFileWithNoErrors() throws Exception {
        final File testFile = new File( "resources/validationAllConstructs.xml" );
        _parser.parse( testFile, this.handler ); // use null as handler to see errors in console
//        _parser.parse( testFile, (DefaultHandler)null ); // uncomment to see errors in console
        assertThat( this.handler.getErrors().size(), is( 0 ) );
        assertThat( this.handler.getFatalErrors().isEmpty(), is( true ) );
    }

    @Test
    public void shouldVerifyValidationUniquenessChecks() throws Exception {
        final int numErrors = 7;
        final File testFile = new File( "resources/validationVerifyUniquenessChecks.xml" );
        _parser.parse( testFile, this.handler ); // use null as handler to see errors in console
//        _parser.parse( testFile, (DefaultHandler)null ); // uncomment to see errors in console
        assertThat( this.handler.getErrors().size(), is( numErrors ) );
        assertThat( this.handler.getFatalErrors().isEmpty(), is( true ) );

        // verify we got the errors we expect and the order we expect
        final List< String > errors = this.handler.getErrors();
        assertThat( errors.get( 0 ).contains( "Duplicate unique value [en]" ), is( true ) );
        assertThat( errors.get( 1 ).contains( "Duplicate unique value [es]" ), is( true ) );
        assertThat( errors.get( 2 ).contains( "Duplicate unique value [nz]" ), is( true ) );
        assertThat( errors.get( 3 ).contains( "Duplicate unique value [vdb:connectionType]" ), is( true ) );
        assertThat( errors.get( 4 ).contains( "Duplicate unique value [ca]" ), is( true ) );
        assertThat( errors.get( 5 ).contains( "Duplicate unique value [ddl:statementOption]" ), is( true ) );
        assertThat( errors.get( 6 ).contains( "Duplicate unique value [vdb:dataRole]" ), is( true ) );
    }

}
