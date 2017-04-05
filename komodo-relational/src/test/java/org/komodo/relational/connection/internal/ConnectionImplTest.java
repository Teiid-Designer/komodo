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

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

import java.util.Properties;

import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.connection.Connection;
import org.komodo.spi.constants.ExportConstants;
import org.komodo.spi.repository.KomodoType;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ConnectionImplTest extends RelationalModelTest {

    private static final String DS_NAME = "mySource";

    protected Connection connection;

    @Before
    public void init() throws Exception {
        this.connection = createConnection( DS_NAME );
    }

    @Test
    public void shouldSetDescription() throws Exception {
        String newValue = "description";
        this.connection.setDescription( getTransaction(), newValue );
        assertThat( this.connection.getDescription( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetExternalLocation() throws Exception {
        String newValue = "/Users/elvis/blah";
        this.connection.setExternalLocation( getTransaction(), newValue );
        assertThat( this.connection.getExternalLocation( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetJndiName() throws Exception {
        String newValue = "java:/jndiName";
        this.connection.setJndiName( getTransaction(), newValue );
        assertThat( this.connection.getJndiName( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetDriverName() throws Exception {
        String newValue = "oracle";
        this.connection.setDriverName( getTransaction(), newValue );
        assertThat( this.connection.getDriverName( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetJdbc() throws Exception {
        boolean isJdbc = false;
        this.connection.setJdbc( getTransaction(), isJdbc );
        assertThat( this.connection.isJdbc( getTransaction() ), is( isJdbc ) );
    }

    @Test
    public void shouldSetClassname() throws Exception {
        String className = "myClass.name";
        this.connection.setJdbc(getTransaction(), false);
        this.connection.setClassName( getTransaction(), className );
        assertThat( this.connection.getClassName( getTransaction() ), is( className ) );
    }

    @Test
    public void shouldHaveDefaultJdbc() throws Exception {
        assertThat( this.connection.isJdbc( getTransaction() ), is( Connection.DEFAULT_JDBC ) );
    }

    @Test
    public void shouldHaveId() throws Exception {
        assertThat( this.connection.getName( getTransaction() ), is( DS_NAME ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.connection.getPropertyNames( getTransaction() );
        final String[] rawProps = this.connection.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.connection.getPropertyNames( getTransaction() );
        final Filter[] filters = this.connection.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.connection.getPrimaryType( getTransaction() ).getName(), is( DataVirtLexicon.Connection.NODE_TYPE ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.connection.getTypeIdentifier( getTransaction() ), is(KomodoType.CONNECTION));
    }

    @Test
    public void shouldExport() throws Exception {
        final String description = "new-description";
        this.connection.setDescription(getTransaction(), description);

        final String extLoc = "new-external-location";
        this.connection.setExternalLocation(getTransaction(), extLoc );

        this.connection.setJdbc(getTransaction(), false);
        this.connection.setJndiName(getTransaction(), "java:/jndiName");
        this.connection.setDriverName(getTransaction(), "dsDriver");
        this.connection.setClassName(getTransaction(), "dsClassname");
        this.connection.setProperty(getTransaction(), "prop1", "prop1Value");
        this.connection.setProperty(getTransaction(), "prop2", "prop2Value");

        byte[] xml = this.connection.export(getTransaction(), new Properties());
        String xmlString = new String(xml);

        assertThat( xmlString.contains(DS_NAME), is( true ) );
        assertThat( xmlString.contains("\t"), is( false ) );
        assertThat( xmlString.contains( description ), is( true ) );
        assertThat( xmlString.contains( extLoc ), is( true ) );
    }

    @Test
    public void shouldExportTabbed() throws Exception {
        this.connection.setJdbc(getTransaction(), false);
        this.connection.setJndiName(getTransaction(), "java:/jndiName");
        this.connection.setDriverName(getTransaction(), "dsDriver");
        this.connection.setClassName(getTransaction(), "dsClassname");
        this.connection.setProperty(getTransaction(), "prop1", "prop1Value");
        this.connection.setProperty(getTransaction(), "prop2", "prop2Value");

        Properties exportProps = new Properties();
        exportProps.put( ExportConstants.USE_TABS_PROP_KEY, true );

        byte[] xml = this.connection.export(getTransaction(), exportProps);
        String xmlString = new String(xml);

        assertThat( xmlString.contains(DS_NAME), is( true ) );
        assertThat( xmlString.contains("\t"), is( true ) );
    }

}
