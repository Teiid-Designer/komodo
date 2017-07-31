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
import java.util.Arrays;
import java.util.List;
import org.jboss.resteasy.util.Encode;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.relational.response.RestVdbDataRole;
import org.komodo.rest.relational.response.RestVdbPermission;
import org.komodo.spi.repository.KomodoType;
import org.mockito.Mockito;

@SuppressWarnings( {"javadoc", "nls"} )
public final class VdbDataRoleSerializerTest extends AbstractSerializerTest {

    private static final String NAME = "MyDataRole";
    private static final String DESCRIPTION = "my description";
    private static final String ROLE_DATA_PATH = VDB_DATA_PATH + "/VdbDataRoles/MyDataRole";
    private static final String PERMISSION_DATA_PATH = ROLE_DATA_PATH + "/VdbPermissions/MyPermission";
    private static final boolean ALLOW_CREATE_TEMP_TABLES = true;
    private static final boolean ANY_AUTHENTICATED = false;
    private static final boolean GRANT_ALL = false;
    private static final List<String> MAPPED_ROLES = Arrays.asList("larry", "curly", "moe");
    
    private static final String PERMISSION_NAME = "MyPermission";
    private static final boolean ALLOW_ALTER = true;
    private static final boolean ALLOW_CREATE = false;
    private static final boolean ALLOW_DELETE = true;
    private static final boolean ALLOW_EXECUTE = false;
    private static final boolean ALLOW_LANGUAGE = true;
    private static final boolean ALLOW_READ = false;
    private static final boolean ALLOW_UPDATE = true;

    /**
     * JSON without a permission.
     */
    private static final String JSON = EMPTY_STRING +
        OPEN_BRACE + NEW_LINE +
        "  \"" + BASE_URI + "\": \"" + MY_BASE_URI + "\"," + NEW_LINE +
        "  \"" + ID + "\": \"" + NAME + "\"," + NEW_LINE +
        "  \"" + DATA_PATH + "\": \"" + ROLE_DATA_PATH + "\"," + NEW_LINE +
        "  \"" + KTYPE + "\": \"" + KomodoType.VDB_DATA_ROLE.getType() + "\"," + NEW_LINE +
        "  \"" + HAS_CHILDREN + "\": true," + NEW_LINE +
        "  \"vdb__dataRole\": \"" + NAME + "\"," + NEW_LINE +
        "  \"vdb__description\": \"" + DESCRIPTION + "\"," + NEW_LINE +
        "  \"vdb__allowCreateTemporaryTables\": true," + NEW_LINE +
        "  \"vdb__anyAuthenticated\": false," + NEW_LINE +
        "  \"vdb__grantAll\": false," + NEW_LINE +
        "  \"vdb__mappedRoleNames\": [" + NEW_LINE +
        "    \"larry\"," + NEW_LINE +
        "    \"curly\"," + NEW_LINE +
        "    \"moe\"" + NEW_LINE +
        "  ]," + NEW_LINE +
        "  \"" + LINKS + "\": " + OPEN_SQUARE_BRACKET + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"self\"," + NEW_LINE +
        "      \"href\": \"" + BASE_URI_PREFIX + ROLE_DATA_PATH + '\"' + NEW_LINE +
        "    " + CLOSE_BRACE + COMMA + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"parent\"," + NEW_LINE +
        "      \"href\": \"" + BASE_URI_PREFIX + VDB_DATA_PATH + "\"" + NEW_LINE +
        "    " + CLOSE_BRACE + COMMA + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"children\"," + NEW_LINE +
        "      \"href\": \"" + BASE_URI_PREFIX + SEARCH + "parent\\u003d" + Encode.encodeQueryParam(ROLE_DATA_PATH) + "\"" + NEW_LINE +
        "    " + CLOSE_BRACE + COMMA + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"permissions\"," + NEW_LINE +
        "      \"href\": \"" + BASE_URI_PREFIX + ROLE_DATA_PATH + "/VdbPermissions\"" + NEW_LINE +
        "    " + CLOSE_BRACE + NEW_LINE +
        "  " + CLOSE_SQUARE_BRACKET + NEW_LINE +
        CLOSE_BRACE;

    /**
     * JSON with a permission
     */
    private static final String JSON2 = EMPTY_STRING +
        OPEN_BRACE + NEW_LINE +
        "  \"" + BASE_URI + "\": \"" + MY_BASE_URI + "\"," + NEW_LINE +
        "  \"" + ID + "\": \"" + NAME + "\"," + NEW_LINE +
        "  \"" + DATA_PATH + "\": \"" + ROLE_DATA_PATH + "\"," + NEW_LINE +
        "  \"" + KTYPE + "\": \"" + KomodoType.VDB_DATA_ROLE.getType() + "\"," + NEW_LINE +
        "  \"" + HAS_CHILDREN + "\": true," + NEW_LINE +
        "  \"vdb__dataRole\": \"" + NAME + "\"," + NEW_LINE +
        "  \"vdb__description\": \"" + DESCRIPTION + "\"," + NEW_LINE +
        "  \"vdb__allowCreateTemporaryTables\": true," + NEW_LINE +
        "  \"vdb__anyAuthenticated\": false," + NEW_LINE +
        "  \"vdb__grantAll\": false," + NEW_LINE +
        "  \"vdb__mappedRoleNames\": [" + NEW_LINE +
        "    \"larry\"," + NEW_LINE +
        "    \"curly\"," + NEW_LINE +
        "    \"moe\"" + NEW_LINE +
        "  ]," + NEW_LINE +
        
        "  \"vdb__permissions\": [\n" +
        "    {\n" +
        "      \"" + BASE_URI + "\": \"" + MY_BASE_URI + "\"," + NEW_LINE +
        "      \"" + ID + "\": \"" + PERMISSION_NAME + "\"," + NEW_LINE +
        "      \"" + DATA_PATH + "\": \"" + PERMISSION_DATA_PATH + "\"," + NEW_LINE +
        "      \"" + KTYPE + "\": \"" + KomodoType.VDB_PERMISSION.getType() + "\"," + NEW_LINE +
        "      \"" + HAS_CHILDREN + "\": true," + NEW_LINE +
        "      \"vdb__permission\": \"" + PERMISSION_NAME + "\",\n" +
        "      \"vdb__allowAlter\" : " + ALLOW_ALTER + ",\n" +
        "      \"vdb__allowCreate\" : " + ALLOW_CREATE + ",\n" +
        "      \"vdb__allowDelete\" : " + ALLOW_DELETE + ",\n" +
        "      \"vdb__allowExecute\" : " + ALLOW_EXECUTE + ",\n" +
        "      \"vdb__allowLanguage\" : " + ALLOW_LANGUAGE + ",\n" +
        "      \"vdb__allowRead\" : " + ALLOW_READ + ",\n" +
        "      \"vdb__allowUpdate\" : " + ALLOW_UPDATE + ",\n" +
        "      \"" + LINKS + "\": [\n" +
        "        {\n" +
        "          \"rel\": \"self\"," + NEW_LINE +
        "          \"href\": \"" + BASE_URI_PREFIX + PERMISSION_DATA_PATH + '\"' + NEW_LINE +
        "        },\n" +
        "        {\n" +
        "          \"rel\": \"parent\"," + NEW_LINE +
        "          \"href\": \"" + BASE_URI_PREFIX + VDB_DATA_PATH + "/VdbDataRoles/MyDataRole\"" + NEW_LINE +
        "        },\n" +
        "        {\n" +
        "          \"rel\": \"children\"," + NEW_LINE +
        "          \"href\": \"" + BASE_URI_PREFIX + SEARCH + "parent\\u003d" + Encode.encodeQueryParam( PERMISSION_DATA_PATH ) + "\"" + NEW_LINE +
        "        },\n" +
        "        {\n" +
        "          \"rel\": \"conditions\"," + NEW_LINE +
        "          \"href\": \""+ BASE_URI_PREFIX + PERMISSION_DATA_PATH + "/VdbConditions\"" + NEW_LINE +
        "        },\n" +
        "        {\n" +
        "          \"rel\": \"masks\"," + NEW_LINE +
        "          \"href\": \"" + BASE_URI_PREFIX + PERMISSION_DATA_PATH + "/VdbMasks\"" + NEW_LINE +
        "        }\n" +
        "      ]\n" +
        "    }\n" +
        "  ],\n" +
        "  \"" + LINKS + "\": " + OPEN_SQUARE_BRACKET + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"self\"," + NEW_LINE +
        "      \"href\": \"" + BASE_URI_PREFIX + ROLE_DATA_PATH + '\"' + NEW_LINE +
        "    " + CLOSE_BRACE + COMMA + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"parent\"," + NEW_LINE +
        "      \"href\": \"" + BASE_URI_PREFIX + VDB_DATA_PATH + "\"" + NEW_LINE +
        "    " + CLOSE_BRACE + COMMA + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"children\"," + NEW_LINE +
        "      \"href\": \"" + BASE_URI_PREFIX + SEARCH + "parent\\u003d" + Encode.encodeQueryParam(ROLE_DATA_PATH) + "\"" + NEW_LINE +
        "    " + CLOSE_BRACE + COMMA + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"permissions\"," + NEW_LINE +
        "      \"href\": \"" + BASE_URI_PREFIX + ROLE_DATA_PATH + "/VdbPermissions\"" + NEW_LINE +
        "    " + CLOSE_BRACE + NEW_LINE +
        "  " + CLOSE_SQUARE_BRACKET + NEW_LINE +
        CLOSE_BRACE;

    private RestVdbDataRole dataRole;
    private DataRole modelDataRole;

    @Before
    public void init() throws Exception {
        Vdb theVdb = mockObject(Vdb.class, VDB_NAME, VDB_DATA_PATH, KomodoType.VDB, true);
        this.modelDataRole = mockObject(DataRole.class, NAME, ROLE_DATA_PATH, KomodoType.VDB_DATA_ROLE, true);
        Mockito.when(this.modelDataRole.getParent(transaction)).thenReturn(theVdb);
        Mockito.when( this.modelDataRole.getPermissions( transaction ) ).thenReturn( Permission.NO_PERMISSIONS );

        this.dataRole = new RestVdbDataRole(MY_BASE_URI, this.modelDataRole, transaction);
        this.dataRole.setName(NAME);
        this.dataRole.setDescription(DESCRIPTION);
        this.dataRole.setAllowCreateTempTables(ALLOW_CREATE_TEMP_TABLES);
        this.dataRole.setAnyAuthenticated(ANY_AUTHENTICATED);
        this.dataRole.setGrantAll(GRANT_ALL);
        this.dataRole.setMappedRoles(MAPPED_ROLES.toArray(new String[MAPPED_ROLES.size()]));
    }

    @Test
    public void shouldExportJson() {
        String json = KomodoJsonMarshaller.marshall(this.dataRole);
        assertEquals(JSON, json);
    }

    @Test
    public void shouldExportJsonWithPermissions() throws Exception {
        // create model object
        final Permission permission = mockObject( Permission.class,
                                                  PERMISSION_NAME,
                                                  PERMISSION_DATA_PATH,
                                                  KomodoType.VDB_PERMISSION,
                                                  true );
        Mockito.when( permission.getParent( this.transaction ) ).thenReturn( this.modelDataRole );

        // add a permission
        final RestVdbPermission restPermission = new RestVdbPermission( MY_BASE_URI, permission, transaction );
        restPermission.setAllowAlter( ALLOW_ALTER );
        restPermission.setAllowCreate( ALLOW_CREATE );
        restPermission.setAllowDelete( ALLOW_DELETE );
        restPermission.setAllowExecute( ALLOW_EXECUTE );
        restPermission.setAllowLanguage( ALLOW_LANGUAGE );
        restPermission.setAllowRead( ALLOW_READ );
        restPermission.setAllowUpdate( ALLOW_UPDATE );
        restPermission.setName( PERMISSION_NAME );
        restPermission.setId( PERMISSION_NAME );
        restPermission.setDataPath( PERMISSION_DATA_PATH );
        restPermission.setkType( KomodoType.VDB_PERMISSION );
        this.dataRole.setPermissions( new RestVdbPermission[] { restPermission } );

        // verify permission is included in exported json
        final String json = KomodoJsonMarshaller.marshall( this.dataRole );
        assertThat( json.contains( RestVdbDataRole.PERMISSIONS_LABEL ), is( true ) );
        assertThat( json.contains( PERMISSION_NAME ), is( true ) );
        assertThat( json.contains( RestVdbPermission.ALLOW_ALTER_LABEL ), is( true ) );
        assertThat( json.contains( RestVdbPermission.ALLOW_CREATE_LABEL ), is( true ) );
        assertThat( json.contains( RestVdbPermission.ALLOW_DELETE_LABEL ), is( true ) );
        assertThat( json.contains( RestVdbPermission.ALLOW_EXECUTE_LABEL ), is( true ) );
        assertThat( json.contains( RestVdbPermission.ALLOW_LANGUAGE_LABEL ), is( true ) );
        assertThat( json.contains( RestVdbPermission.ALLOW_READ_LABEL ), is( true ) );
        assertThat( json.contains( RestVdbPermission.ALLOW_UPDATE_LABEL ), is( true ) );
    }

    @Test
    public void shouldImportJson() {
        final RestVdbDataRole dataRole = KomodoJsonMarshaller.unmarshall(JSON, RestVdbDataRole.class);
        assertThat(dataRole.getDescription(), is(DESCRIPTION));
        assertThat(dataRole.getName(), is(NAME));
        assertThat(dataRole.isAllowCreateTempTables(), is(ALLOW_CREATE_TEMP_TABLES));
        assertThat(dataRole.isAnyAuthenticated(), is(ANY_AUTHENTICATED));
        assertThat(dataRole.isGrantAll(), is(GRANT_ALL));
        assertThat(dataRole.getLinks().size(), is(4));
        assertThat(dataRole.getProperties().isEmpty(), is(true));
        assertThat(dataRole.getMappedRoles().length, is(MAPPED_ROLES.size()));
        assertThat(dataRole.getPermissions().length, is( 0 ) );
    }

    @Test
    public void shouldImportJsonWithPermissions() {
        final RestVdbDataRole dataRole = KomodoJsonMarshaller.unmarshall( JSON2, RestVdbDataRole.class );
        final RestVdbPermission[] permissions = dataRole.getPermissions();
        assertThat( permissions.length, is( 1 ) );
        
        final RestVdbPermission permission = permissions[ 0 ];
        assertThat( permission.getName(), is( PERMISSION_NAME ) );
        assertThat( permission.isAllowAlter(), is( ALLOW_ALTER ) );
        assertThat( permission.isAllowCreate(), is( ALLOW_CREATE ) );
        assertThat( permission.isAllowDelete(), is( ALLOW_DELETE ) );
        assertThat( permission.isAllowExecute(), is( ALLOW_EXECUTE ) );
        assertThat( permission.isAllowLanguage(), is( ALLOW_LANGUAGE ) );
        assertThat( permission.isAllowRead(), is( ALLOW_READ ) );
        assertThat( permission.isAllowUpdate(), is( ALLOW_UPDATE ) );        
        assertThat( permission.getLinks().size(), is( 5 ) );
        assertThat( permission.getProperties().isEmpty(), is( true ) );
    }

    @Test( expected = Exception.class )
    public void shouldNotExportWhenNameIsMissing() {
        final RestBasicEntity incomplete = new RestVdbDataRole();
        KomodoJsonMarshaller.marshall(incomplete);
    }

    @Test( expected = Exception.class )
    public void shouldNotImportJsonWhenIdIsMissing() {
        final String malformed = "{\"description\":\"my description\"anyAuthenticated,\"allowCreateTempTables\":true,\"grantAll\":false,\"mappedRoles\":[\"larry\",\"curly\",\"moe\"]}";
        KomodoJsonMarshaller.unmarshall(malformed, RestVdbDataRole.class);
    }

}
