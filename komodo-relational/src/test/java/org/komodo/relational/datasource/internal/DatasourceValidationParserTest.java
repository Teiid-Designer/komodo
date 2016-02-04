/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.datasource.internal;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import java.io.File;
import java.util.List;
import org.junit.BeforeClass;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;

@SuppressWarnings( { "javadoc", "nls" } )
public final class DatasourceValidationParserTest extends RelationalModelTest {

    private static DatasourceValidationParser datasourceValidationParser;

    @BeforeClass
    public static void oneTimeSetup() throws Exception {
        datasourceValidationParser = new DatasourceValidationParser( );
    }
    
    @Test( expected = IllegalArgumentException.class )
    public void shouldFailWhenNullRulesFile() throws Exception {
        datasourceValidationParser.parse(getTransaction(), null);
    }

    @Test
    public void shouldFindErrorsInXmlWithErrors() throws Exception {
        String testFilePath = getClass().getClassLoader().getResource("datasource-with-errors.xml").getFile();
        final File testFile = new File(testFilePath);

        final int numErrors = 6;
        final int numSources = 4;
        final String[] dataSourceNames = datasourceValidationParser.parse(getTransaction(), testFile);
        final List<String> errors = datasourceValidationParser.getErrors();
        
        assertThat( errors.size(), is( numErrors ) );
        assertThat( dataSourceNames.length, is( numSources ) );

        // verify we got the errors we expect and the order we expect
        assertThat( errors.get( 0 ).contains( "Duplicate unique value [mySource] declared for identity constraint \"nameKey\" of element \"dataSourceSet\"" ), is( true ) );
        
        assertThat( errors.get( 1 ).contains( "Value '' with length = '0' is not facet-valid with respect to minLength '1' for type 'nonEmptyStringType'" ), is( true ) );
        assertThat( errors.get( 2 ).contains( "Element 'property' must have no element [children], and the value must be valid." ), is( true ) );
        assertThat( errors.get( 3 ).contains( "Attribute 'jdbc' must appear on element 'dataSource'." ), is( true ) );
        assertThat( errors.get( 4 ).contains( "'dunno' is not a valid value for 'boolean'" ), is( true ) );
        assertThat( errors.get( 5 ).contains( "The value 'dunno' of attribute 'jdbc' on element 'dataSource' is not valid with respect to its type, 'boolean" ), is( true ) );
    }

    @Test
    public void shouldParseSingleDatasourceXmlWithNoErrors() throws Exception {
        String testFilePath = getClass().getClassLoader().getResource("datasource-valid.xml").getFile();
        final File testFile = new File(testFilePath);

        final String[] dataSourceNames = datasourceValidationParser.parse(getTransaction(), testFile);
        final List<String> errors = datasourceValidationParser.getErrors();
        assertThat( errors.size(), is( 0 ) );
        assertThat( dataSourceNames.length, is( 1 ) );
        assertThat( dataSourceNames[0], is( "mySource" ) );
    }
    
    @Test
    public void shouldParseMultipleDatasourceXmlWithNoErrors() throws Exception {
        String testFilePath = getClass().getClassLoader().getResource("datasource-multipleValid.xml").getFile();
        final File testFile = new File(testFilePath);

        final String[] dataSourceNames = datasourceValidationParser.parse(getTransaction(), testFile);
        final List<String> errors = datasourceValidationParser.getErrors();
        assertThat( errors.size(), is( 0 ) );
        assertThat( dataSourceNames.length, is( 3 ) );
        
        assertThat( dataSourceNames[0], is( "mySource1" ) );
        assertThat( dataSourceNames[1], is( "mySource2" ) );
        assertThat( dataSourceNames[2], is( "mySource3" ) );
    }
    
    @Test
    public void shouldParseDashboardDSXmlWithNoErrors() throws Exception {
        String testFilePath = getClass().getClassLoader().getResource("datasource-validDashboardDS.xml").getFile();
        final File testFile = new File(testFilePath);

        final String[] dataSourceNames = datasourceValidationParser.parse(getTransaction(), testFile);
        final List<String> errors = datasourceValidationParser.getErrors();
        assertThat( errors.size(), is( 0 ) );
        assertThat( dataSourceNames.length, is( 1 ) );
        
        assertThat( dataSourceNames[0], is( "DashboardDS" ) );
    }
    
}
