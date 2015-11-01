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
import java.util.ArrayList;
import java.util.List;
import javax.ws.rs.core.UriBuilder;
import org.junit.Before;
import org.junit.Test;
import org.komodo.rest.json.JsonConstants;
import org.komodo.rest.relational.RestVdbCondition;
import org.komodo.rest.relational.RestVdbMask;
import org.komodo.rest.relational.RestVdbPermission;
import org.komodo.spi.repository.KomodoType;

@SuppressWarnings( { "javadoc", "nls" } )
public final class VdbPermissionSerializerTest implements JsonConstants {

    private static final URI BASE_URI = UriBuilder.fromUri("http://localhost:8081/v1/workspace/").build();
    private static final String PARENT_VDB = "vdb1";
    private static final String PARENT_DATA_ROLE = "MyDataRole";
    private static final String NAME = "MyPermission";
    private static final String PERM_DATA_PATH = "/workspace/vdb1/vdbDataRoles/MyDataRole/permissions/" + NAME;
    private static final boolean ALLOW_ALTER = true;
    private static final boolean ALLOW_CREATE = false;
    private static final boolean ALLOW_DELETE = true;
    private static final boolean ALLOW_EXECUTE = false;
    private static final boolean ALLOW_LANGUAGE = true;
    private static final boolean ALLOW_READ = false;
    private static final boolean ALLOW_UPDATE = true;

    private static final List<RestVdbCondition> CONDITIONS = new ArrayList<>();
    private static final String CONDITION1 = "condition1";
    private static final String CONDITION2 = "condition2";
    private static final String CONDITION3 = "condition3";
    private static final String CONDITION1_DATA_PATH = PERM_DATA_PATH + "/conditions/" + CONDITION1;
    private static final String CONDITION2_DATA_PATH = PERM_DATA_PATH + "/conditions/" + CONDITION2;
    private static final String CONDITION3_DATA_PATH = PERM_DATA_PATH + "/conditions/" + CONDITION3;

    private static final List<RestVdbMask> MASKS = new ArrayList<>();
    private static final String MASK1 = "mask1";
    private static final String MASK2 = "mask2";
    private static final String MASK3 = "mask3";
    private static final String MASK1_DATA_PATH = PERM_DATA_PATH + "/masks/" + MASK1;
    private static final String MASK2_DATA_PATH = PERM_DATA_PATH + "/masks/" + MASK2;
    private static final String MASK3_DATA_PATH = PERM_DATA_PATH + "/masks/" + MASK3;

    static {
        RestVdbCondition condition1 = new RestVdbCondition(BASE_URI, CONDITION1,
                                                           CONDITION1_DATA_PATH, KomodoType.VDB_CONDITION,
                                                           false, NAME, PARENT_DATA_ROLE, PARENT_VDB);
        condition1.setName(CONDITION1);
        condition1.setConstraint(true);

        RestVdbCondition condition2 = new RestVdbCondition(BASE_URI, CONDITION2,
                                                           CONDITION2_DATA_PATH, KomodoType.VDB_CONDITION,
                                                           false, NAME, PARENT_DATA_ROLE, PARENT_VDB);
        condition2.setName(CONDITION2);
        condition2.setConstraint(false);

        RestVdbCondition condition3 = new RestVdbCondition(BASE_URI, CONDITION3,
                                                           CONDITION3_DATA_PATH, KomodoType.VDB_CONDITION,
                                                           false, NAME, PARENT_DATA_ROLE, PARENT_VDB);
        condition3.setName(CONDITION3);
        condition3.setConstraint(true);

        CONDITIONS.add(condition1);
        CONDITIONS.add(condition2);
        CONDITIONS.add(condition3);

        RestVdbMask mask1 = new RestVdbMask(BASE_URI, MASK1, MASK1_DATA_PATH,
                                                                          KomodoType.VDB_MASK, false, NAME, PARENT_DATA_ROLE,
                                                                          PARENT_VDB);
        mask1.setName(MASK1);
        mask1.setOrder("that");

        RestVdbMask mask2 = new RestVdbMask(BASE_URI, MASK2, MASK2_DATA_PATH,
                                                                        KomodoType.VDB_MASK, false, NAME, PARENT_DATA_ROLE,
                                                                        PARENT_VDB);
        mask2.setName(MASK2);
        mask2.setOrder("or");

        RestVdbMask mask3 = new RestVdbMask(BASE_URI, MASK3, MASK3_DATA_PATH,
                                                                        KomodoType.VDB_MASK, false, NAME, PARENT_DATA_ROLE,
                                                                        PARENT_VDB);
        mask3.setName(MASK3);
        mask3.setOrder("halfdozenofanother");

        MASKS.add(mask1);
        MASKS.add(mask2);
        MASKS.add(mask3);
    }

    private static final String JSON = EMPTY_STRING +
    OPEN_BRACE + NEW_LINE +
    "  \"" + ID + "\": \"" + NAME + "\"," + NEW_LINE +
    "  \"" + DATA_PATH + "\": \"" + PERM_DATA_PATH + "\"," + NEW_LINE +
    "  \"" + KTYPE + "\": \"" + KomodoType.VDB_PERMISSION.getType() + "\"," + NEW_LINE +
    "  \"" + HAS_CHILDREN + "\": true," + NEW_LINE +
    "  \"vdb__permission\": \"" + NAME + "\"," + NEW_LINE +
    "  \"vdb__allowAlter\": true," + NEW_LINE +
    "  \"vdb__allowCreate\": false," + NEW_LINE +
    "  \"vdb__allowDelete\": true," + NEW_LINE +
    "  \"vdb__allowExecute\": false," + NEW_LINE +
    "  \"vdb__allowLanguage\": true," + NEW_LINE +
    "  \"vdb__allowRead\": false," + NEW_LINE +
    "  \"vdb__allowUpdate\": true," + NEW_LINE +
    "  \"" + LINKS + "\": " + OPEN_SQUARE_BRACKET + NEW_LINE +
    "    " + OPEN_BRACE + NEW_LINE +
    "      \"rel\": \"self\"," + NEW_LINE +
    "      \"href\": \"http://localhost:8081/v1/workspace/workspace/vdbs/vdb1/VdbDataRoles/MyDataRole/VdbPermissions/MyPermission\"" + NEW_LINE +
    "    " + CLOSE_BRACE + COMMA + NEW_LINE +
    "    " + OPEN_BRACE + NEW_LINE +
    "      \"rel\": \"parent\"," + NEW_LINE +
    "      \"href\": \"http://localhost:8081/v1/workspace/workspace/vdbs/vdb1/VdbDataRoles/MyDataRole\"" + NEW_LINE +
    "    " + CLOSE_BRACE + COMMA + NEW_LINE +
    "    " + OPEN_BRACE + NEW_LINE +
    "      \"rel\": \"conditions\"," + NEW_LINE +
    "      \"href\": \"http://localhost:8081/v1/workspace/workspace/vdbs/vdb1/VdbDataRoles/MyDataRole/VdbPermissions/MyPermission/VdbConditions\"" + NEW_LINE +
    "    " + CLOSE_BRACE + COMMA + NEW_LINE +
    "    " + OPEN_BRACE + NEW_LINE +
    "      \"rel\": \"masks\"," + NEW_LINE +
    "      \"href\": \"http://localhost:8081/v1/workspace/workspace/vdbs/vdb1/VdbDataRoles/MyDataRole/VdbPermissions/MyPermission/VdbMasks\"" + NEW_LINE +
    "    " + CLOSE_BRACE + NEW_LINE +
    "  " + CLOSE_SQUARE_BRACKET + NEW_LINE +
    CLOSE_BRACE;

    private RestVdbPermission permission;

    @Before
    public void init() {
        this.permission = new RestVdbPermission(BASE_URI, NAME, PERM_DATA_PATH,
                                                                            KomodoType.VDB_PERMISSION, true,
                                                                            PARENT_DATA_ROLE, PARENT_VDB);
        this.permission.setName(NAME);
        this.permission.setAllowAlter( ALLOW_ALTER );
        this.permission.setAllowCreate( ALLOW_CREATE );
        this.permission.setAllowDelete( ALLOW_DELETE );
        this.permission.setAllowExecute( ALLOW_EXECUTE );
        this.permission.setAllowLanguage( ALLOW_LANGUAGE );
        this.permission.setAllowRead( ALLOW_READ );
        this.permission.setAllowUpdate( ALLOW_UPDATE );
    }

    @Test
    public void shouldExportJson() {
        String json = KomodoJsonMarshaller.marshall( this.permission );
        assertEquals(JSON, json);
    }

    @Test
    public void shouldImportJson() {
        final RestVdbPermission permission = KomodoJsonMarshaller.unmarshall( JSON, RestVdbPermission.class );
        assertThat( permission.getName(), is( NAME ) );
        assertThat( permission.isAllowAlter(), is( ALLOW_ALTER ) );
        assertThat( permission.isAllowCreate(), is( ALLOW_CREATE ) );
        assertThat( permission.isAllowDelete(), is( ALLOW_DELETE ) );
        assertThat( permission.isAllowExecute(), is( ALLOW_EXECUTE ) );
        assertThat( permission.isAllowLanguage(), is( ALLOW_LANGUAGE ) );
        assertThat( permission.isAllowRead(), is( ALLOW_READ ) );
        assertThat( permission.isAllowUpdate(), is( ALLOW_UPDATE ) );
        assertThat( permission.getLinks().size(), is( 4 ) );
        assertThat( permission.getProperties().isEmpty(), is( true ) );
    }

    @Test( expected = Exception.class )
    public void shouldNotExportWhenNameIsMissing() {
        final RestVdbPermission incomplete = new RestVdbPermission();
        KomodoJsonMarshaller.marshall( incomplete );
    }

    @Test( expected = Exception.class )
    public void shouldNotImportJsonWhenIdIsMissing() {
        final String malformed = "{\"allowAlter\":true,\"allowCreate\":false,\"allowDelete\":true,\"allowExecute\":false,\"allowLanguage\":true,\"allowRead\":false,\"allowUpdate\":true,\"conditions\":{\"over\":true,\"the\":false,\"rainbow\":true},\"masks\":{\"this\":\"that\",\"either\":\"or\",\"sixofone\":\"halfdozenofanother\"}}";
        KomodoJsonMarshaller.unmarshall( malformed, RestVdbPermission.class );
    }

}
