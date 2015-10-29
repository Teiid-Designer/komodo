/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational.json;

import static org.junit.Assert.assertEquals;
import java.net.URI;
import javax.ws.rs.core.UriBuilder;
import org.junit.Before;
import org.junit.Test;
import org.komodo.rest.relational.RestVdb;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoType;

@SuppressWarnings( { "javadoc", "nls" } )
public final class VdbSerializerTest implements StringConstants {

    private static final URI BASE_URI = UriBuilder.fromUri("http://localhost:8081/v1/workspace/").build();
    private static final String VDB_NAME = "MyVdb";
    private static final String DATA_PATH = "/data/path";
    private static final String DESCRIPTION = "my description";
    private static final String ORIGINAL_FILE = "/Users/ElvisIsKing/MyVdb.xml";
    private static final KomodoType kType = KomodoType.VDB;
    private static final String CONNECTION_TYPE = "BY_VERSION";
    private static final int VERSION = 1;

    private static final String JSON = OPEN_BRACE + NEW_LINE +
        "  \"keng__id\": \"MyVdb\"," + NEW_LINE +
        "  \"keng__dataPath\": \"/data/path\"," + NEW_LINE +
        "  \"keng__kType\": \"Vdb\"," + NEW_LINE +
        "  \"keng__hasChildren\": true," + NEW_LINE +
        "  \"vdb__name\": \"MyVdb\"," + NEW_LINE +
        "  \"vdb__description\": \"my description\"," + NEW_LINE +
        "  \"vdb__originalFile\": \"/Users/ElvisIsKing/MyVdb.xml\"," + NEW_LINE +
        "  \"vdb__preview\": false," + NEW_LINE +
        "  \"vdb__connectionType\": \"BY_VERSION\"," + NEW_LINE +
        "  \"vdb__version\": 1," + NEW_LINE +
        "  \"keng___links\": [" + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"self\"," + NEW_LINE +
        "      \"href\": \"http://localhost:8081/v1/workspace/workspace/vdbs/MyVdb\"" + NEW_LINE +
        "    " + CLOSE_BRACE + COMMA + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"parent\"," + NEW_LINE +
        "      \"href\": \"http://localhost:8081/v1/workspace/workspace/vdbs\"" + NEW_LINE +
        "    " + CLOSE_BRACE + COMMA + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"imports\"," + NEW_LINE +
        "      \"href\": \"http://localhost:8081/v1/workspace/workspace/vdbs/MyVdb/VdbImports\"" + NEW_LINE +
        "    " + CLOSE_BRACE + COMMA + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"models\"," + NEW_LINE +
        "      \"href\": \"http://localhost:8081/v1/workspace/workspace/vdbs/MyVdb/Models\"" + NEW_LINE +
        "    " + CLOSE_BRACE + COMMA + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"translators\"," + NEW_LINE +
        "      \"href\": \"http://localhost:8081/v1/workspace/workspace/vdbs/MyVdb/VdbTranslators\"" + NEW_LINE +
        "    " + CLOSE_BRACE + COMMA + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"dataRoles\"," + NEW_LINE +
        "      \"href\": \"http://localhost:8081/v1/workspace/workspace/vdbs/MyVdb/VdbDataRoles\"" + NEW_LINE +
        "    " + CLOSE_BRACE + NEW_LINE +
        "  " + CLOSE_SQUARE_BRACKET + NEW_LINE +
        CLOSE_BRACE;


    private RestVdb vdb;

    @Before
    public void init() {
        this.vdb = new RestVdb(BASE_URI, VDB_NAME, DATA_PATH, kType, true);
        this.vdb.setName(VDB_NAME);
        this.vdb.setDescription(DESCRIPTION);
        this.vdb.setOriginalFilePath(ORIGINAL_FILE);
        this.vdb.setConnectionType(CONNECTION_TYPE);
        this.vdb.setPreview(false);
        this.vdb.setVersion(VERSION);
    }

    @Test
    public void shouldExportJson() {
        String json = KomodoJsonMarshaller.marshall( this.vdb );
        System.out.println(json);
        assertEquals(json, JSON);
    }

    @Test
    public void shouldImportJson() {
        final RestVdb descriptor = KomodoJsonMarshaller.unmarshall( JSON, RestVdb.class );
        assertEquals(descriptor.getName(), VDB_NAME);
        assertEquals(descriptor.getDescription(), DESCRIPTION);
        assertEquals(descriptor.getOriginalFilePath(), ORIGINAL_FILE);
        assertEquals(descriptor.getLinks().size(), 6);
        assertEquals(descriptor.getProperties().isEmpty(), true);
    }

    @Test( expected = Exception.class )
    public void shouldNotExportJsonWhenNameIsMissing() {
        final RestVdb descriptor = new RestVdb();
        KomodoJsonMarshaller.marshall( descriptor );
    }

    @Test( expected = Exception.class )
    public void shouldNotImportJsonWhenIdIsMissing() {
        final String malformed = "{\"description\":\"my description\",\"links\":[{\"rel\":\"self\",\"href\":\"http://localhost:8080/v1/workspace/vdbs/MyVdb\",\"method\":\"GET\"},{\"rel\":\"parent\",\"href\":\"http://localhost:8080/v1/workspace/vdbs\",\"method\":\"GET\"},{\"rel\":\"manifest\",\"href\":\"http://localhost:8080/v1/workspace/vdbs/MyVdb/manifest\",\"method\":\"GET\"}]}";
        KomodoJsonMarshaller.unmarshall( malformed, RestVdb.class );
    }

}
