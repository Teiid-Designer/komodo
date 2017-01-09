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
package org.komodo.rest.relational.json;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import org.jboss.resteasy.util.Encode;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.rest.relational.response.RestVdbImport;
import org.komodo.spi.repository.KomodoType;
import org.mockito.Mockito;

@SuppressWarnings( { "javadoc", "nls" } )
public final class VdbImportSerializerTest extends AbstractSerializerTest {

    private static final String IMP_DATA_PATH = VDB_DATA_PATH + "/vdbImports/MyImport";
    private static final boolean IMPORT_DATA_POLICIES = true;
    private static final String NAME = "MyImport";
    private static final int VERSION = 2;
    private static final String JSON = EMPTY_STRING +
        OPEN_BRACE + NEW_LINE +
        "  \"" + BASE_URI + "\": \"" + MY_BASE_URI + "\"," + NEW_LINE +
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
        "      \"href\": \"" + BASE_URI_PREFIX + VDB_DATA_PATH + "/VdbImports/MyImport\"" + NEW_LINE +
        "    " + CLOSE_BRACE + COMMA + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"parent\"," + NEW_LINE +
        "      \"href\": \"" + BASE_URI_PREFIX + VDB_DATA_PATH + "\"" + NEW_LINE +
        "    " + CLOSE_BRACE + COMMA + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"children\"," + NEW_LINE +
        "      \"href\": \"" + BASE_URI_PREFIX + SEARCH + "parent\\u003d" + Encode.encodeQueryParam(IMP_DATA_PATH)  + "\"" + NEW_LINE +
        "    " + CLOSE_BRACE + NEW_LINE +
        "  " + CLOSE_SQUARE_BRACKET + NEW_LINE +
      CLOSE_BRACE;



    private RestVdbImport vdbImport;

    @Before
    public void init() throws Exception {
        Vdb theVdb = mockObject(Vdb.class, VDB_NAME, VDB_DATA_PATH, KomodoType.VDB, true);
        VdbImport theImport = mockObject(VdbImport.class, NAME, IMP_DATA_PATH, KomodoType.VDB_IMPORT, false);
        Mockito.when(theImport.getParent(transaction)).thenReturn(theVdb);


        this.vdbImport = new RestVdbImport(MY_BASE_URI, theImport, transaction);
        this.vdbImport.setName(NAME);
        this.vdbImport.setVersion(VERSION);
        this.vdbImport.setImportDataPolicies( IMPORT_DATA_POLICIES );
    }

    @Test
    public void shouldExport() {
        String importValue = KomodoJsonMarshaller.marshall( this.vdbImport );
        assertEquals(JSON, importValue);
    }

    @Test
    public void shouldImport() {
        final RestVdbImport vdbImport = KomodoJsonMarshaller.unmarshall( JSON, RestVdbImport.class );
        assertThat( vdbImport.isImportDataPolicies(), is( IMPORT_DATA_POLICIES ) );
        assertThat( vdbImport.getName(), is( NAME ) );
        assertThat( vdbImport.getVersion(), is( VERSION ) );
        assertThat( vdbImport.getLinks().size(), is( 3 ) );
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
