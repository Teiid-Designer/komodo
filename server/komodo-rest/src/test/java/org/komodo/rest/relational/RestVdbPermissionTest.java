/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNot.not;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import java.util.ArrayList;
import java.util.List;
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

    private static final List<RestVdbCondition> CONDITIONS = new ArrayList<>();

    private static final List<RestVdbMask> MASKS = new ArrayList<>();

    static {
        RestVdbCondition condition1 = new RestVdbCondition();
        condition1.setName("over");
        condition1.setConstraint(true);

        RestVdbCondition condition2 = new RestVdbCondition();
        condition1.setName("the");
        condition1.setConstraint(false);

        RestVdbCondition condition3 = new RestVdbCondition();
        condition1.setName("rainbow");
        condition1.setConstraint(true);

        CONDITIONS.add(condition1);
        CONDITIONS.add(condition2);
        CONDITIONS.add(condition3);

        RestVdbMask mask1 = new RestVdbMask();
        mask1.setName("this");
        mask1.setOrder("that");

        RestVdbMask mask2 = new RestVdbMask();
        mask1.setName("either");
        mask1.setOrder("or");

        RestVdbMask mask3 = new RestVdbMask();
        mask1.setName("sixofone");
        mask1.setOrder("halfdozenofanother");

        MASKS.add(mask1);
        MASKS.add(mask2);
        MASKS.add(mask3);
    }

    private RestVdbPermission permission;

    private RestVdbPermission copy() {
        final RestVdbPermission copy = new RestVdbPermission();
        copy.setName(this.permission.getName() );
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
        this.permission = new RestVdbPermission();
        this.permission.setName(NAME );
        this.permission.setAllowAlter( ALLOW_ALTER );
        this.permission.setAllowCreate( ALLOW_CREATE );
        this.permission.setAllowDelete( ALLOW_DELETE );
        this.permission.setAllowExecute( ALLOW_EXECUTE );
        this.permission.setAllowLanguage( ALLOW_LANGUAGE );
        this.permission.setAllowRead( ALLOW_READ );
        this.permission.setAllowUpdate( ALLOW_UPDATE );
        this.permission.setConditions( CONDITIONS.toArray(new RestVdbCondition[0]) );
        this.permission.setMasks( MASKS.toArray(new RestVdbMask[0]) );
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
        assertThat( empty.getConditions().length, is( 0 ) );
        assertThat( empty.getMasks().length, is( 0 ) );
        assertThat( empty.getProperties().isEmpty(), is( true ) );
        assertThat( empty.getLinks().size(), is( 0 ) );
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

        RestVdbCondition condition = new RestVdbCondition();
        condition.setName("blah");
        condition.setConstraint(false);

        thatPermission.setConditions(new RestVdbCondition[] { condition });
        assertThat( this.permission, is( not( thatPermission ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenMasksAreDifferent() {
        final RestVdbPermission thatPermission = copy();

        RestVdbMask mask = new RestVdbMask();
        mask.setName("blah");
        mask.setOrder("blah");

        thatPermission.setMasks(new RestVdbMask[] { mask });
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
        RestVdbCondition condition = new RestVdbCondition();
        condition.setName("blah");
        condition.setConstraint(false);

        RestVdbCondition[] newConditions = new RestVdbCondition[] { condition };
        this.permission.setConditions( newConditions );
        assertThat( this.permission.getConditions(), is( newConditions ) );
    }

    @Test
    public void shouldSetMasks() {
        RestVdbMask mask = new RestVdbMask();
        mask.setName("blah");
        mask.setOrder("blah");

        RestVdbMask[] newMasks = new RestVdbMask[] { mask };
        this.permission.setMasks(newMasks);

        assertThat( this.permission.getMasks(), is( newMasks ) );
    }

    @Test
    public void shouldSetName() {
        final String newName = "blah";
        this.permission.setName( newName );
        assertThat( this.permission.getName(), is( newName ) );
    }

}
