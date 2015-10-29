/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational.json;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import java.net.URI;
import javax.ws.rs.core.UriBuilder;
import org.junit.Before;
import org.junit.Test;
import org.komodo.rest.json.JsonConstants;
import org.komodo.rest.relational.RestVdbImport;
import org.komodo.spi.repository.KomodoType;

@SuppressWarnings( { "javadoc", "nls" } )
public final class VdbImportSerializerTest implements JsonConstants {

    private static final URI BASE_URI = UriBuilder.fromUri("http://localhost:8081/v1/workspace/").build();
    private static final String IMP_DATA_PATH = "/workspace/MyVdb/vdbImports/MyImport";
    private static final String VDB_NAME = "MyVdb";
    private static final boolean IMPORT_DATA_POLICIES = true;
    private static final String NAME = "MyImport";
    private static final int VERSION = 2;
    private static final String JSON = EMPTY_STRING +
        OPEN_BRACE + NEW_LINE +
        "  \"" + ID + "\": \"" + NAME + "\"," + NEW_LINE +
        "  \"" + DATA_PATH + "\": \"" + IMP_DATA_PATH + "\"," + NEW_LINE +
        "  \"" + KTYPE + "\": \"" + KomodoType.VDB_IMPORT.getType() + "\"," + NEW_LINE +
        "  \"" + HAS_CHILDREN + "\": false," + NEW_LINE +
        "  \"vdb__importVdb\": \"" + NAME + "\"," + NEW_LINE +
        "  \"vdb__version\": 2," + NEW_LINE +
        "  \"vdb__importDataPolicies\": true," + NEW_LINE +
        "  \"" + LINKS + "\": " + OPEN_SQUARE_BRACKET + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"self\"," + NEW_LINE +
        "      \"href\": \"http://localhost:8081/v1/workspace/workspace/vdbs/MyVdb/VdbImports/MyImport\"" + NEW_LINE +
        "    " + CLOSE_BRACE + COMMA + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"parent\"," + NEW_LINE +
        "      \"href\": \"http://localhost:8081/v1/workspace/workspace/vdbs/MyVdb\"" + NEW_LINE +
        "    " + CLOSE_BRACE + NEW_LINE +
        "  " + CLOSE_SQUARE_BRACKET + NEW_LINE +
      CLOSE_BRACE;



    private RestVdbImport vdbImport;

    @Before
    public void init() {
        this.vdbImport = new RestVdbImport(BASE_URI, NAME,
                                                                   IMP_DATA_PATH, KomodoType.VDB_IMPORT, false, VDB_NAME);
        this.vdbImport.setName(NAME);
        this.vdbImport.setVersion(VERSION);
        this.vdbImport.setImportDataPolicies( IMPORT_DATA_POLICIES );
    }

    @Test
    public void shouldExport() {
        String importValue = KomodoJsonMarshaller.marshall( this.vdbImport );
        System.out.println(importValue);
        assertEquals(JSON, importValue);
    }

    @Test
    public void shouldImport() {
        final RestVdbImport vdbImport = KomodoJsonMarshaller.unmarshall( JSON, RestVdbImport.class );
        assertThat( vdbImport.isImportDataPolicies(), is( IMPORT_DATA_POLICIES ) );
        assertThat( vdbImport.getName(), is( NAME ) );
        assertThat( vdbImport.getVersion(), is( VERSION ) );
        assertThat( vdbImport.getLinks().size(), is( 2 ) );
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
        final String malformed = "{\"vdb__version\":5,\"vdb__importDataPolicies\":false}";
        KomodoJsonMarshaller.unmarshall( malformed, RestVdbImport.class );
    }

}
