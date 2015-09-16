/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational.json;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.rest.relational.RestVdbImport;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;

@SuppressWarnings( { "javadoc", "nls" } )
public final class VdbImportSerializerTest {

    private static final boolean IMPORT_DATA_POLICIES = true;
    private static final String JSON = "{\"id\":\"MyImport\",\"version\":2,\"importDataPolicies\":true}";
    private static final String NAME = "MyImport";
    private static final int VERSION = 2;

    private RestVdbImport vdbImport;

    @Before
    public void init() {
        this.vdbImport = new RestVdbImport( NAME, VERSION );
        this.vdbImport.setImportDataPolicies( IMPORT_DATA_POLICIES );
    }

    @Test
    public void shouldExport() {
        assertThat( KomodoJsonMarshaller.marshall( this.vdbImport ), is( JSON ) );
    }

    @Test
    public void shouldImport() {
        final RestVdbImport vdbImport = KomodoJsonMarshaller.unmarshall( JSON, RestVdbImport.class );
        assertThat( vdbImport.isImportDataPolicies(), is( IMPORT_DATA_POLICIES ) );
        assertThat( vdbImport.getName(), is( NAME ) );
        assertThat( vdbImport.getVersion(), is( VERSION ) );
        assertThat( vdbImport.getLinks().length, is( 0 ) );
        assertThat( vdbImport.getProperties().isEmpty(), is( true ) );
    }

    @Test
    public void shouldImportWhenImportDataPoliciesIsMissing() {
        final String malformed = "{\"id\":\"MyImport\",\"version\":2}";
        final RestVdbImport vdbImport = KomodoJsonMarshaller.unmarshall( malformed, RestVdbImport.class );
        assertThat( vdbImport.isImportDataPolicies(), is( VdbImport.DEFAULT_IMPORT_DATA_POLICIES ) );
        assertThat( vdbImport.getName(), is( NAME ) );
        assertThat( vdbImport.getVersion(), is( VERSION ) );
        assertThat( vdbImport.getLinks().length, is( 0 ) );
        assertThat( vdbImport.getProperties().isEmpty(), is( true ) );
    }

    @Test
    public void shouldImportWhenVersionIsMissing() {
        final String malformed = "{\"id\":\"MyImport\",\"importDataPolicies\":true}";
        final RestVdbImport vdbImport = KomodoJsonMarshaller.unmarshall( malformed, RestVdbImport.class );
        assertThat( vdbImport.isImportDataPolicies(), is( IMPORT_DATA_POLICIES ) );
        assertThat( vdbImport.getName(), is( NAME ) );
        assertThat( vdbImport.getVersion(), is( Vdb.DEFAULT_VERSION ) );
        assertThat( vdbImport.getLinks().length, is( 0 ) );
        assertThat( vdbImport.getProperties().isEmpty(), is( true ) );
    }

    @Test( expected = Exception.class )
    public void shouldNotExportWhenNameIsMissing() {
        final RestVdbImport incomplete = new RestVdbImport();
        incomplete.setImportDataPolicies( IMPORT_DATA_POLICIES );
        incomplete.setVersion( VERSION );
        KomodoJsonMarshaller.marshall( incomplete );
    }

    @Test( expected = Exception.class )
    public void shouldNotImportWhenIdIsMissing() {
        final String malformed = "{\"version\":5,\"importDataPolicies\":false}";
        KomodoJsonMarshaller.unmarshall( malformed, RestVdbImport.class );
    }

}
