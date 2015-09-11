/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json.serialize;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import org.komodo.rest.json.RestVdbPermission;

@SuppressWarnings( { "javadoc", "nls" } )
public final class VdbPermissionSerializerTest {

    private static final String JSON = "{\"id\":\"MyPermission\",\"allowAlter\":true,\"allowCreate\":false,\"allowDelete\":true,\"allowExecute\":false,\"allowLanguage\":true,\"allowRead\":false,\"allowUpdate\":true,\"conditions\":{\"over\":true,\"the\":false,\"rainbow\":true},\"masks\":{\"this\":\"that\",\"either\":\"or\",\"sixofone\":\"halfdozenofanother\"}}";
    private static final String NAME = "MyPermission";
    private static final boolean ALLOW_ALTER = true;
    private static final boolean ALLOW_CREATE = false;
    private static final boolean ALLOW_DELETE = true;
    private static final boolean ALLOW_EXECUTE = false;
    private static final boolean ALLOW_LANGUAGE = true;
    private static final boolean ALLOW_READ = false;
    private static final boolean ALLOW_UPDATE = true;

    private static final Map< String, Boolean > CONDITIONS = Collections.unmodifiableMap( new HashMap< String, Boolean >() {

        private static final long serialVersionUID = 1L;

        {
            put( "over", true );
            put( "the", false );
            put( "rainbow", true );
        }
    } );

    private static final Map< String, String > MASKS = Collections.unmodifiableMap( new HashMap< String, String >() {

        private static final long serialVersionUID = 1L;

        {
            put( "this", "that" );
            put( "either", "or" );
            put( "sixofone", "halfdozenofanother" );
        }
    } );

    private RestVdbPermission permission;

    @Before
    public void init() {
        this.permission = new RestVdbPermission( NAME );
        this.permission.setAllowAlter( ALLOW_ALTER );
        this.permission.setAllowCreate( ALLOW_CREATE );
        this.permission.setAllowDelete( ALLOW_DELETE );
        this.permission.setAllowExecute( ALLOW_EXECUTE );
        this.permission.setAllowLanguage( ALLOW_LANGUAGE );
        this.permission.setAllowRead( ALLOW_READ );
        this.permission.setAllowUpdate( ALLOW_UPDATE );
        this.permission.setConditions( CONDITIONS );
        this.permission.setMasks( MASKS );
    }

    @Test
    public void shouldExportEmptyConditionsAndMasks() {
        this.permission.setConditions( null );
        this.permission.setMasks( null );
        final String expected = "{\"id\":\"MyPermission\",\"allowAlter\":true,\"allowCreate\":false,\"allowDelete\":true,\"allowExecute\":false,\"allowLanguage\":true,\"allowRead\":false,\"allowUpdate\":true,\"conditions\":{},\"masks\":{}}";
        assertThat( KomodoJsonMarshaller.marshall( this.permission ), is( expected ) );
    }

    @Test
    public void shouldExportJson() {
        assertThat( KomodoJsonMarshaller.marshall( this.permission ), is( JSON ) );
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
        assertThat( permission.getConditions(), is( CONDITIONS ) );
        assertThat( permission.getMasks(), is( MASKS ) );
        assertThat( permission.getLinks().length, is( 0 ) );
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
