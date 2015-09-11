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
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;

@SuppressWarnings( { "javadoc", "nls" } )
public final class RestVdbPermissionTest {

    private static final String NAME = "MyPermission";
    private static final boolean ALLOW_ALTER = true;
    private static final boolean ALLOW_CREATE = true;
    private static final boolean ALLOW_DELETE = true;
    private static final boolean ALLOW_EXECUTE = true;
    private static final boolean ALLOW_LANGUAGE = true;
    private static final boolean ALLOW_READ = true;
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

    private RestVdbPermission copy() {
        final RestVdbPermission copy = new RestVdbPermission( this.permission.getName() );
        copy.setAllowAlter( this.permission.isAllowAlter() );
        copy.setAllowCreate( this.permission.isAllowCreate() );
        copy.setAllowDelete( this.permission.isAllowDelete() );
        copy.setAllowExecute( this.permission.isAllowExecute() );
        copy.setAllowLanguage( this.permission.isAllowLanguage() );
        copy.setAllowRead( this.permission.isAllowRead() );
        copy.setAllowUpdate( this.permission.isAllowUpdate() );
        copy.setConditions( this.permission.getConditions() );
        copy.setMasks( this.permission.getMasks() );
        copy.setLinks( this.permission.getLinks() );
        copy.setProperties( this.permission.getProperties() );

        return copy;
    }

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
    public void shouldBeEqual() {
        final RestVdbPermission thatPermission = copy();
        assertThat( this.permission, is( thatPermission ) );
    }

    @Test
    public void shouldBeEqualWhenComparingEmptyEntries() {
        final RestVdbPermission empty1 = new RestVdbPermission();
        final RestVdbPermission empty2 = new RestVdbPermission();
        assertThat( empty1, is( empty2 ) );
    }

    @Test
    public void shouldConstructEmptyPermission() {
        final RestVdbPermission empty = new RestVdbPermission();
        assertThat( empty.getName(), is( nullValue() ) );
        assertThat( empty.getConditions().isEmpty(), is( true ) );
        assertThat( empty.getMasks().isEmpty(), is( true ) );
        assertThat( empty.getProperties().isEmpty(), is( true ) );
        assertThat( empty.getLinks().length, is( 0 ) );
    }

    @Test
    public void shouldHaveSameHashCode() {
        final RestVdbPermission thatPermission = copy();
        assertThat( this.permission.hashCode(), is( thatPermission.hashCode() ) );
    }

    @Test
    public void shouldNotBeEqualWhenAllowAlterIsDifferent() {
        final RestVdbPermission thatPermission = copy();
        thatPermission.setAllowAlter( !this.permission.isAllowAlter() );
        assertThat( this.permission, is( not( thatPermission ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenAllowCreateIsDifferent() {
        final RestVdbPermission thatPermission = copy();
        thatPermission.setAllowCreate( !this.permission.isAllowCreate() );
        assertThat( this.permission, is( not( thatPermission ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenAllowDeleteIsDifferent() {
        final RestVdbPermission thatPermission = copy();
        thatPermission.setAllowDelete( !this.permission.isAllowDelete() );
        assertThat( this.permission, is( not( thatPermission ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenAllowExecuteIsDifferent() {
        final RestVdbPermission thatPermission = copy();
        thatPermission.setAllowExecute( !this.permission.isAllowExecute() );
        assertThat( this.permission, is( not( thatPermission ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenAllowLanguageIsDifferent() {
        final RestVdbPermission thatPermission = copy();
        thatPermission.setAllowLanguage( !this.permission.isAllowLanguage() );
        assertThat( this.permission, is( not( thatPermission ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenAllowReadIsDifferent() {
        final RestVdbPermission thatPermission = copy();
        thatPermission.setAllowRead( !this.permission.isAllowRead() );
        assertThat( this.permission, is( not( thatPermission ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenAllowUpdateIsDifferent() {
        final RestVdbPermission thatPermission = copy();
        thatPermission.setAllowUpdate( !this.permission.isAllowUpdate() );
        assertThat( this.permission, is( not( thatPermission ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenConditionsAreDifferent() {
        final RestVdbPermission thatPermission = copy();
        thatPermission.setConditions( Collections.singletonMap( "blah", false ) );
        assertThat( this.permission, is( not( thatPermission ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenMasksAreDifferent() {
        final RestVdbPermission thatPermission = copy();
        thatPermission.setMasks( Collections.singletonMap( "blah", "blah" ) );
        assertThat( this.permission, is( not( thatPermission ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenNameIsDifferent() {
        final RestVdbPermission thatPermission = copy();
        thatPermission.setName( this.permission.getName() + "blah" );
        assertThat( this.permission, is( not( thatPermission ) ) );
    }

    @Test
    public void shouldSetAllowAlter() {
        final boolean newValue = !this.permission.isAllowAlter();
        this.permission.setAllowAlter( newValue );
        assertThat( this.permission.isAllowAlter(), is( newValue ) );
    }

    @Test
    public void shouldSetAllowCreate() {
        final boolean newValue = !this.permission.isAllowCreate();
        this.permission.setAllowCreate( newValue );
        assertThat( this.permission.isAllowCreate(), is( newValue ) );
    }

    @Test
    public void shouldSetAllowDelete() {
        final boolean newValue = !this.permission.isAllowDelete();
        this.permission.setAllowDelete( newValue );
        assertThat( this.permission.isAllowDelete(), is( newValue ) );
    }

    @Test
    public void shouldSetAllowExecute() {
        final boolean newValue = !this.permission.isAllowExecute();
        this.permission.setAllowExecute( newValue );
        assertThat( this.permission.isAllowExecute(), is( newValue ) );
    }

    @Test
    public void shouldSetAllowLanguage() {
        final boolean newValue = !this.permission.isAllowLanguage();
        this.permission.setAllowLanguage( newValue );
        assertThat( this.permission.isAllowLanguage(), is( newValue ) );
    }

    @Test
    public void shouldSetAllowRead() {
        final boolean newValue = !this.permission.isAllowRead();
        this.permission.setAllowRead( newValue );
        assertThat( this.permission.isAllowRead(), is( newValue ) );
    }

    @Test
    public void shouldSetAllowUpdate() {
        final boolean newValue = !this.permission.isAllowUpdate();
        this.permission.setAllowUpdate( newValue );
        assertThat( this.permission.isAllowUpdate(), is( newValue ) );
    }

    @Test
    public void shouldSetConditions() {
        final Map< String, Boolean > newConditions = Collections.singletonMap( "blah", true );
        this.permission.setConditions( newConditions );
        assertThat( this.permission.getConditions(), is( newConditions ) );
    }

    @Test
    public void shouldSetMasks() {
        final Map< String, String > newMasks = Collections.singletonMap( "blah", "blah" );
        this.permission.setMasks( newMasks );
        assertThat( this.permission.getMasks(), is( newMasks ) );
    }

    @Test
    public void shouldSetName() {
        final String newName = "blah";
        this.permission.setName( newName );
        assertThat( this.permission.getName(), is( newName ) );
    }

}
