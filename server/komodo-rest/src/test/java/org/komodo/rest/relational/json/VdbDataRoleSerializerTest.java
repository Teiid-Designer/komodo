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
import java.util.Arrays;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import org.komodo.rest.relational.RestVdbDataRole;
import org.komodo.rest.relational.RestVdbPermission;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;

@SuppressWarnings( { "javadoc", "nls" } )
public final class VdbDataRoleSerializerTest {

    private static final String DESCRIPTION = "my description";
    private static final String JSON = "{\"id\":\"MyDataRole\",\"description\":\"my description\",\"allowCreateTempTables\":true,\"anyAuthenticated\":false,\"grantAll\":false,\"mappedRoles\":[\"larry\",\"curly\",\"moe\"],\"permissions\":[{\"id\":\"firstPermission\",\"allowAlter\":false,\"allowCreate\":false,\"allowDelete\":false,\"allowExecute\":false,\"allowLanguage\":false,\"allowRead\":false,\"allowUpdate\":false,\"conditions\":{},\"masks\":{}},{\"id\":\"secondPermission\",\"allowAlter\":false,\"allowCreate\":false,\"allowDelete\":false,\"allowExecute\":false,\"allowLanguage\":false,\"allowRead\":false,\"allowUpdate\":false,\"conditions\":{},\"masks\":{}}]}";
    private static final String NAME = "MyDataRole";
    private static final boolean ALLOW_CREATE_TEMP_TABLES = true;
    private static final boolean ANY_AUTHENTICATED = false;
    private static final boolean GRANT_ALL = false;
    private static final List< String > MAPPED_ROLES = Arrays.asList( "larry", "curly", "moe" );

    private RestVdbDataRole dataRole;
    private RestVdbPermission[] permissions = new RestVdbPermission[ 2 ];

    @Before
    public void init() {
        this.dataRole = new RestVdbDataRole( NAME );
        this.dataRole.setDescription( DESCRIPTION );
        this.dataRole.setAllowCreateTempTables( ALLOW_CREATE_TEMP_TABLES );
        this.dataRole.setAnyAuthenticated( ANY_AUTHENTICATED );
        this.dataRole.setGrantAll( GRANT_ALL );
        this.dataRole.setMappedRoles( MAPPED_ROLES.toArray( new String[ MAPPED_ROLES.size() ] ) );

        this.permissions[ 0 ] = new RestVdbPermission( "firstPermission" );
        this.permissions[ 1 ] = new RestVdbPermission( "secondPermission" );
        this.dataRole.setPermissions( this.permissions );
    }

    @Test
    public void shouldExportEmptyMappedRolesAndPermissions() {
        this.dataRole.setMappedRoles( null );
        this.dataRole.setPermissions( null );
        final String expected = "{\"id\":\"MyDataRole\",\"description\":\"my description\",\"allowCreateTempTables\":true,\"anyAuthenticated\":false,\"grantAll\":false,\"mappedRoles\":[],\"permissions\":[]}";
        assertThat( KomodoJsonMarshaller.marshall( this.dataRole ), is( expected ) );

    }

    @Test
    public void shouldExportJson() {
        assertThat( KomodoJsonMarshaller.marshall( this.dataRole ), is( JSON ) );
    }

    @Test
    public void shouldImportJson() {
        final RestVdbDataRole dataRole = KomodoJsonMarshaller.unmarshall( JSON, RestVdbDataRole.class );
        assertThat( dataRole.getDescription(), is( DESCRIPTION ) );
        assertThat( dataRole.getName(), is( NAME ) );
        assertThat( dataRole.isAllowCreateTempTables(), is( ALLOW_CREATE_TEMP_TABLES ) );
        assertThat( dataRole.isAnyAuthenticated(), is( ANY_AUTHENTICATED ) );
        assertThat( dataRole.isGrantAll(), is( GRANT_ALL ) );
        assertThat( dataRole.getLinks().length, is( 0 ) );
        assertThat( dataRole.getProperties().isEmpty(), is( true ) );
        assertThat( dataRole.getMappedRoles().length, is( MAPPED_ROLES.size() ) );
    }

    @Test( expected = Exception.class )
    public void shouldNotExportWhenNameIsMissing() {
        final RestVdbDataRole incomplete = new RestVdbDataRole();
        KomodoJsonMarshaller.marshall( incomplete );
    }

    @Test( expected = Exception.class )
    public void shouldNotImportJsonWhenIdIsMissing() {
        final String malformed = "{\"description\":\"my description\"anyAuthenticated,\"allowCreateTempTables\":true,\"grantAll\":false,\"mappedRoles\":[\"larry\",\"curly\",\"moe\"]}";
        KomodoJsonMarshaller.unmarshall( malformed, RestVdbDataRole.class );
    }

}
