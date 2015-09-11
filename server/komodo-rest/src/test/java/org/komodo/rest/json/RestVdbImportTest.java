/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNot.not;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;

@SuppressWarnings( { "javadoc", "nls" } )
public final class RestVdbImportTest {

    private static final String NAME = "MyVdbImport";
    private static final int VERSION = 2;
    private static final boolean IMPORT_DATA_POLICIES = true;

    private RestVdbImport vdbImport;

    @Before
    public void init() {
        this.vdbImport = new RestVdbImport( NAME, VERSION );
        this.vdbImport.setImportDataPolicies( IMPORT_DATA_POLICIES );
    }

    @Test
    public void shouldBeEqual() {
        final RestVdbImport thatImport = new RestVdbImport( this.vdbImport.getName(), this.vdbImport.getVersion() );
        thatImport.setImportDataPolicies( this.vdbImport.isImportDataPolicies() );
        thatImport.setLinks( this.vdbImport.getLinks() );
        thatImport.setProperties( this.vdbImport.getProperties() );

        assertThat( this.vdbImport, is( thatImport ) );
    }

    @Test
    public void shouldBeEqualWhenComparingEmptyEntries() {
        final RestVdbImport empty1 = new RestVdbImport();
        final RestVdbImport empty2 = new RestVdbImport();
        assertThat( empty1, is( empty2 ) );
    }

    @Test
    public void shouldConstructEmptyImport() {
        final RestVdbImport empty = new RestVdbImport();
        assertThat( empty.getName(), is( nullValue() ) );
        assertThat( empty.getVersion(), is( Vdb.DEFAULT_VERSION ) );
        assertThat( empty.isImportDataPolicies(), is( VdbImport.DEFAULT_IMPORT_DATA_POLICIES ) );
        assertThat( empty.getProperties().isEmpty(), is( true ) );
        assertThat( empty.getLinks().length, is( 0 ) );
    }

    @Test
    public void shouldHaveSameHashCode() {
        final RestVdbImport thatImport = new RestVdbImport( this.vdbImport.getName(), this.vdbImport.getVersion() );
        thatImport.setImportDataPolicies( this.vdbImport.isImportDataPolicies() );
        thatImport.setLinks( this.vdbImport.getLinks() );
        thatImport.setProperties( this.vdbImport.getProperties() );

        assertThat( this.vdbImport.hashCode(), is( thatImport.hashCode() ) );
    }

    @Test
    public void shouldNotBeEqualWhenImportDataPoliciesIsDifferent() {
        final RestVdbImport thatImport = new RestVdbImport( this.vdbImport.getName(), this.vdbImport.getVersion() );
        thatImport.setImportDataPolicies( !this.vdbImport.isImportDataPolicies() );
        thatImport.setLinks( this.vdbImport.getLinks() );
        thatImport.setProperties( this.vdbImport.getProperties() );

        assertThat( this.vdbImport.isImportDataPolicies(), is( not( thatImport.isImportDataPolicies() ) ) );
        assertThat( this.vdbImport, is( not( thatImport ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenNameIsDifferent() {
        final RestVdbImport thatImport = new RestVdbImport( this.vdbImport.getName() + "blah", this.vdbImport.getVersion() );
        thatImport.setImportDataPolicies( this.vdbImport.isImportDataPolicies() );
        thatImport.setLinks( this.vdbImport.getLinks() );
        thatImport.setProperties( this.vdbImport.getProperties() );

        assertThat( this.vdbImport.getName(), is( not( thatImport.getName() ) ) );
        assertThat( this.vdbImport, is( not( thatImport ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenVersionIsDifferent() {
        final RestVdbImport thatImport = new RestVdbImport( this.vdbImport.getName(), this.vdbImport.getVersion() + 1 );
        thatImport.setImportDataPolicies( this.vdbImport.isImportDataPolicies() );
        thatImport.setLinks( this.vdbImport.getLinks() );
        thatImport.setProperties( this.vdbImport.getProperties() );

        assertThat( this.vdbImport.getVersion(), is( not( thatImport.getVersion() ) ) );
        assertThat( this.vdbImport, is( not( thatImport ) ) );
    }

    @Test
    public void shouldSetImportDataPolicies() {
        final boolean newImportDataPolicies = !this.vdbImport.isImportDataPolicies();
        this.vdbImport.setImportDataPolicies( newImportDataPolicies );
        assertThat( this.vdbImport.isImportDataPolicies(), is( newImportDataPolicies ) );
    }

    @Test
    public void shouldSetName() {
        final String newName = "blah";
        this.vdbImport.setName( newName );
        assertThat( this.vdbImport.getName(), is( newName ) );
    }

    @Test
    public void shouldSetVersion() {
        final int newVersion = ( this.vdbImport.getVersion() + 1 );
        this.vdbImport.setVersion( newVersion );
        assertThat( this.vdbImport.getVersion(), is( newVersion ) );
    }

}
