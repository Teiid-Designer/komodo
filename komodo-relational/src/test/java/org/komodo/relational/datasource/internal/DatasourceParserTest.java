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

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import java.io.File;
import java.util.List;
import org.junit.BeforeClass;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.datasource.Datasource;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;

@SuppressWarnings( { "javadoc", "nls" } )
public final class DatasourceParserTest extends RelationalModelTest {

    private static DatasourceParser datasourceParser;

    @BeforeClass
    public static void setup() throws Exception {
        datasourceParser = new DatasourceParser();
    }

    private KomodoObject getWorkspace() throws KException {
        return _repo.komodoWorkspace(getTransaction());
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailWhenNullRulesFile() throws Exception {
        datasourceParser.parse(getTransaction(), getWorkspace(), null);
    }

    @Test
    public void shouldFindErrorsInXmlWithErrors() throws Exception {
        String testFilePath = getClass().getClassLoader().getResource("datasource-with-errors.xml").getFile();
        final File testFile = new File(testFilePath);

        final int numErrors = 6;
        final int numSources = 4;
        final Datasource[] dataSources = datasourceParser.parse(getTransaction(), getWorkspace(), testFile);
        final List<String> errors = datasourceParser.getErrors();
        
        assertThat( errors.size(), is( numErrors ) );
        assertThat( dataSources.length, is( numSources ) );

        // verify we got the errors we expect and the order we expect
        assertThat( errors.get( 0 ).contains( "Duplicate unique value [mySource] declared for identity constraint \"nameKey\" of element \"dataSourceSet\"" ), is( true ) );
        
        assertThat( errors.get( 1 ).contains( "Value '' with length = '0' is not facet-valid with respect to minLength '1' for type 'nonEmptyStringType'" ), is( true ) );
        assertThat( errors.get( 2 ).contains( "Element 'property' must have no element [children], and the value must be valid." ), is( true ) );
        assertThat( errors.get( 3 ).contains( "Attribute 'jdbc' must appear on element 'dataSource'." ), is( true ) );
        assertThat( errors.get( 4 ).contains( "'dunno' is not a valid value for 'boolean'" ), is( true ) );
        assertThat( errors.get( 5 ).contains( "The value 'dunno' of attribute 'jdbc' on element 'dataSource' is not valid with respect to its type, 'boolean" ), is( true ) );
    }

    @Test
    public void shouldValidateSingleDatasourceXmlWithNoErrors() throws Exception {
        String testFilePath = getClass().getClassLoader().getResource("datasource-valid.xml").getFile();
        final File testFile = new File(testFilePath);

        final String[] dataSourceNames = datasourceParser.validate(testFile);
        final List<String> errors = datasourceParser.getErrors();
        assertThat( errors.size(), is( 0 ) );
        assertThat( dataSourceNames.length, is( 1 ) );
        assertThat( dataSourceNames[0], is( "mySource" ) );
    }
    
    @Test
    public void shouldValidateMultipleDatasourceXmlWithNoErrors() throws Exception {
        String testFilePath = getClass().getClassLoader().getResource("datasource-multipleValid.xml").getFile();
        final File testFile = new File(testFilePath);

        final String[] dataSourceNames = datasourceParser.validate(testFile);
        final List<String> errors = datasourceParser.getErrors();
        assertThat( errors.size(), is( 0 ) );
        assertThat( dataSourceNames.length, is( 3 ) );
        
        assertThat( dataSourceNames[0], is( "mySource1" ) );
        assertThat( dataSourceNames[1], is( "mySource2" ) );
        assertThat( dataSourceNames[2], is( "mySource3" ) );
    }
    
    @Test
    public void shouldValidateDashboardDSXmlWithNoErrors() throws Exception {
        String testFilePath = getClass().getClassLoader().getResource("datasource-validDashboardDS.xml").getFile();
        final File testFile = new File(testFilePath);

        final String[] dataSourceNames = datasourceParser.validate(testFile);
        final List<String> errors = datasourceParser.getErrors();
        assertThat( errors.size(), is( 0 ) );
        assertThat( dataSourceNames.length, is( 1 ) );
        
        assertThat( dataSourceNames[0], is( "DashboardDS" ) );
    }
    
    @Test
    public void shouldParseSingleDatasourceXmlWithNoErrors() throws Exception {
        String testFilePath = getClass().getClassLoader().getResource("datasource-valid.xml").getFile();
        final File testFile = new File(testFilePath);

        final Datasource[] dataSources = datasourceParser.parse(getTransaction(), getWorkspace(), testFile);
        final List<String> errors = datasourceParser.getErrors();
        assertThat( errors.size(), is( 0 ) );
        assertThat( dataSources.length, is( 1 ) );
        assertThat( dataSources[0].getName(getTransaction()), is( "mySource" ) );
        assertThat( dataSources[0].isJdbc(getTransaction()), is( false ) );
        assertThat( dataSources[0].isPreview(getTransaction()), is( true ) );
        assertThat( dataSources[0].getProfileName(getTransaction()), is("dsProfileName") );
        assertThat( dataSources[0].getJndiName(getTransaction()), is("java:/jndiName") );
        assertThat( dataSources[0].getDriverName(getTransaction()), is("dsDriver") );
        assertThat( dataSources[0].getClassName(getTransaction()), is("dsClassname") );
        assertThat( dataSources[0].getProperty(getTransaction(), "prop1").getStringValue(getTransaction()), is("prop1Value") );
        assertThat( dataSources[0].getProperty(getTransaction(), "prop2").getStringValue(getTransaction()), is("prop2Value") );
    }
    
    @Test
    public void shouldParseMultipleDatasourceXmlWithNoErrors() throws Exception {
        String testFilePath = getClass().getClassLoader().getResource("datasource-multipleValid.xml").getFile();
        final File testFile = new File(testFilePath);

        final Datasource[] dataSources = datasourceParser.parse(getTransaction(), getWorkspace(), testFile);
        final List<String> errors = datasourceParser.getErrors();
        assertThat( errors.size(), is( 0 ) );
        assertThat( dataSources.length, is( 3 ) );
        
        // Check source 1
        assertThat( dataSources[0].getName(getTransaction()), is( "mySource1" ) );
        assertThat( dataSources[0].isJdbc(getTransaction()), is( true ) );
        assertThat( dataSources[0].isPreview(getTransaction()), is( false ) );
        assertThat( dataSources[0].getProfileName(getTransaction()), is( "mySource1Profile" ) );
        assertThat( dataSources[0].getJndiName(getTransaction()), is( "java:/jndiName1" ) );
        assertThat( dataSources[0].getDriverName(getTransaction()), is( "dsDriver1" ) );
        assertThat( dataSources[0].getClassName(getTransaction())==null, is( true ) );
        assertThat( dataSources[0].getProperty(getTransaction(), "prop1").getStringValue(getTransaction()), is( "prop1Value" ) );
        assertThat( dataSources[0].getProperty(getTransaction(), "prop2").getStringValue(getTransaction()), is( "prop2Value" ) );
        
        // Check source 2
        assertThat( dataSources[1].getName(getTransaction()), is( "mySource2" ) );
        assertThat( dataSources[1].isJdbc(getTransaction()), is( false ) );
        assertThat( dataSources[1].isPreview(getTransaction()), is( true ) );
        assertThat( dataSources[1].getProfileName(getTransaction()), is( "mySource2Profile" ) );
        assertThat( dataSources[1].getJndiName(getTransaction()), is( "java:/jndiName2" ) );
        assertThat( dataSources[1].getDriverName(getTransaction()), is( "dsDriver2" ) );
        assertThat( dataSources[1].getClassName(getTransaction()), is( "dsClassname2" ) );
        assertThat( dataSources[1].getProperty(getTransaction(), "prop1").getStringValue(getTransaction()), is( "prop1Value" ) );
        assertThat( dataSources[1].getProperty(getTransaction(), "prop2").getStringValue(getTransaction()), is( "prop2Value" ) );
        
        // Check source 3
        assertThat( dataSources[2].getName(getTransaction()), is( "mySource3" ) );
        assertThat( dataSources[2].isJdbc(getTransaction()), is( true ) );
        assertThat( dataSources[2].isPreview(getTransaction()), is( false ) );
        assertThat( dataSources[2].getProfileName(getTransaction()), is( "mySource3Profile" ) );
        assertThat( dataSources[2].getJndiName(getTransaction()), is( "java:/jndiName3" ) );
        assertThat( dataSources[2].getDriverName(getTransaction()), is( "dsDriver3" ) );
        assertThat( dataSources[2].getClassName(getTransaction())==null, is( true ) );
        assertThat( dataSources[2].getProperty(getTransaction(), "prop1").getStringValue(getTransaction()), is( "prop1Value" ) );
        assertThat( dataSources[2].getProperty(getTransaction(), "prop2").getStringValue(getTransaction()), is( "prop2Value" ) );
    }
    
    @Test
    public void shouldParseDashboardDSXmlWithNoErrors() throws Exception {
        String testFilePath = getClass().getClassLoader().getResource("datasource-validDashboardDS.xml").getFile();
        final File testFile = new File(testFilePath);

        final Datasource[] dataSources = datasourceParser.parse(getTransaction(), getWorkspace(), testFile);
        final List<String> errors = datasourceParser.getErrors();
        assertThat( errors.size(), is( 0 ) );
        assertThat( dataSources.length, is( 1 ) );
        
        assertThat( dataSources[0].getName(getTransaction()), is( "DashboardDS" ) );
        assertThat( dataSources[0].isJdbc(getTransaction()), is( true ) );
        assertThat( dataSources[0].isPreview(getTransaction()), is( false ) );
        assertThat( dataSources[0].getProfileName(getTransaction())==null, is( true ) );
        assertThat( dataSources[0].getJndiName(getTransaction()), is("java:jboss/datasources/DashboardDS") );
        assertThat( dataSources[0].getDriverName(getTransaction()), is("h2") );
        assertThat( dataSources[0].getClassName(getTransaction())==null, is( true ) );
        assertThat( dataSources[0].getProperty(getTransaction(), "username").getStringValue(getTransaction()), is("sa") );
        assertThat( dataSources[0].getProperty(getTransaction(), "password").getStringValue(getTransaction()), is("sa") );
        assertThat( dataSources[0].getProperty(getTransaction(), "connection-url").getStringValue(getTransaction()), is("jdbc:h2:file:${jboss.server.data.dir}/teiid-dashboard/teiid-dashboard-ds;DB_CLOSE_ON_EXIT=FALSE;DB_CLOSE_DELAY=-1") );
        assertThat( dataSources[0].getProperty(getTransaction(), "set-tx-query-timeout").getStringValue(getTransaction()), is("false") );
        assertThat( dataSources[0].getProperty(getTransaction(), "share-prepared-statements").getStringValue(getTransaction()), is("false") );
        assertThat( dataSources[0].getProperty(getTransaction(), "spy").getStringValue(getTransaction()), is("false") );
        assertThat( dataSources[0].getProperty(getTransaction(), "statistics-enabled").getStringValue(getTransaction()), is("false") );
        assertThat( dataSources[0].getProperty(getTransaction(), "track-statements").getStringValue(getTransaction()), is("NOWARN") );
        assertThat( dataSources[0].getProperty(getTransaction(), "use-fast-fail").getStringValue(getTransaction()), is("false") );
        assertThat( dataSources[0].getProperty(getTransaction(), "validate-on-match").getStringValue(getTransaction()), is("false") );
        assertThat( dataSources[0].getProperty(getTransaction(), "allow-multiple-users").getStringValue(getTransaction()), is("false") );
        assertThat( dataSources[0].getProperty(getTransaction(), "connectable").getStringValue(getTransaction()), is("false") );
    }
    
}
