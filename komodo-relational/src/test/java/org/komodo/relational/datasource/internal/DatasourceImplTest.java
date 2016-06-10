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
import java.util.Arrays;
import java.util.Properties;
import org.junit.Before;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.datasource.Datasource;
import org.komodo.spi.constants.ExportConstants;
import org.komodo.spi.repository.KomodoType;

@SuppressWarnings( { "javadoc", "nls" } )
public final class DatasourceImplTest extends RelationalModelTest {

    private static final String DS_NAME = "mySource";

    protected Datasource datasource;

    @Before
    public void init() throws Exception {
        this.datasource = createDatasource( DS_NAME );
    }

    @Test
    public void shouldSetJndiName() throws Exception {
        String newValue = "java:/jndiName";
        this.datasource.setJndiName( getTransaction(), newValue );
        assertThat( this.datasource.getJndiName( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetDriverName() throws Exception {
        String newValue = "oracle";
        this.datasource.setDriverName( getTransaction(), newValue );
        assertThat( this.datasource.getDriverName( getTransaction() ), is( newValue ) );
    }
        
    @Test
    public void shouldSetProfileName() throws Exception {
        String newValue = "myConnectionProfile";
        this.datasource.setProfileName( getTransaction(), newValue );
        assertThat( this.datasource.getProfileName( getTransaction() ), is( newValue ) );
    }
        
    @Test
    public void shouldSetJdbc() throws Exception {
        boolean isJdbc = false;
        this.datasource.setJdbc( getTransaction(), isJdbc );
        assertThat( this.datasource.isJdbc( getTransaction() ), is( isJdbc ) );
    }

    @Test
    public void shouldSetPreview() throws Exception {
        boolean isPreview = true;
        this.datasource.setPreview( getTransaction(), isPreview );
        assertThat( this.datasource.isPreview( getTransaction() ), is( isPreview ) );
    }

    @Test
    public void shouldSetClassname() throws Exception {
        String className = "myClass.name";
        this.datasource.setJdbc(getTransaction(), false);
        this.datasource.setClassName( getTransaction(), className );
        assertThat( this.datasource.getClassName( getTransaction() ), is( className ) );
    }
        
    @Test
    public void shouldHaveDefaultJdbc() throws Exception {
        assertThat( this.datasource.isJdbc( getTransaction() ), is( Datasource.DEFAULT_JDBC ) );
    }

    @Test
    public void shouldHaveDefaultPreview() throws Exception {
        assertThat( this.datasource.isPreview( getTransaction() ), is( Datasource.DEFAULT_PREVIEW ) );
    }

    @Test
    public void shouldHaveId() throws Exception {
        assertThat( this.datasource.getName( getTransaction() ), is( DS_NAME ) );
    }

    @Test
    public void shouldHaveJdbcProperty() throws Exception {
        String[] propNames = this.datasource.getPropertyNames( getTransaction() );
        assertThat( Arrays.asList(propNames).contains(KomodoLexicon.DataSource.JDBC), is( true ) );
    }
    
    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.datasource.getPropertyNames( getTransaction() );
        final String[] rawProps = this.datasource.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.datasource.getPropertyNames( getTransaction() );
        final Filter[] filters = this.datasource.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }
    
    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.datasource.getPrimaryType( getTransaction() ).getName(), is( KomodoLexicon.DataSource.NODE_TYPE ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.datasource.getTypeIdentifier( getTransaction() ), is(KomodoType.DATASOURCE));
    }
    
    @Test
    public void shouldExport() throws Exception {
        this.datasource.setJdbc(getTransaction(), false);
        this.datasource.setPreview(getTransaction(), true);
        this.datasource.setProfileName( getTransaction(), "dsProfileName" );
        this.datasource.setJndiName(getTransaction(), "java:/jndiName");
        this.datasource.setDriverName(getTransaction(), "dsDriver");
        this.datasource.setClassName(getTransaction(), "dsClassname");
        this.datasource.setProperty(getTransaction(), "prop1", "prop1Value");
        this.datasource.setProperty(getTransaction(), "prop2", "prop2Value");

        byte[] xml = this.datasource.export(getTransaction(), new Properties());
        String xmlString = new String(xml);
        
        assertThat( xmlString.contains(DS_NAME), is( true ) );
        assertThat( xmlString.contains("\t"), is( false ) );
    }

    @Test
    public void shouldExportTabbed() throws Exception {
        this.datasource.setJdbc(getTransaction(), false);
        this.datasource.setPreview(getTransaction(), true);
        this.datasource.setProfileName( getTransaction(), "dsProfileName" );
        this.datasource.setJndiName(getTransaction(), "java:/jndiName");
        this.datasource.setDriverName(getTransaction(), "dsDriver");
        this.datasource.setClassName(getTransaction(), "dsClassname");
        this.datasource.setProperty(getTransaction(), "prop1", "prop1Value");
        this.datasource.setProperty(getTransaction(), "prop2", "prop2Value");

        Properties exportProps = new Properties();
        exportProps.put( ExportConstants.USE_TABS_PROP_KEY, true );
        byte[] xml = this.datasource.export(getTransaction(), exportProps);
        String xmlString = new String(xml);

        assertThat( xmlString.contains(DS_NAME), is( true ) );
        assertThat( xmlString.contains("\t"), is( true ) );
    }

}
