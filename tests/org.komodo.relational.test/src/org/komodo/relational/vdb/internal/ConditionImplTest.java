/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.vdb.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ConditionImplTest extends RelationalModelTest {

    private Condition condition;

    @Before
    public void init() throws Exception {
        final Vdb vdb = createVdb();
        final DataRole dataRole = vdb.addDataRole( this.uow, "dataRole" );
        final Permission permission = dataRole.addPermission( this.uow, "permission" );
        this.condition = permission.addCondition( this.uow, "condition" );
        commit();
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.condition.isChildRestricted(), is( true ) );
    }

    @Test
    public void shouldFailConstructionIfNotCondition() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new ConditionImpl( this.uow, _repo, this.condition.getParent( this.uow ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveConstraintDefaultValueAfterConstruction() throws Exception {
        assertThat( this.condition.isConstraint( this.uow ), is( Condition.DEFAULT_CONSTRAINT ) );
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.condition.getPrimaryType( this.uow ).getName(), is( VdbLexicon.DataRole.Permission.Condition.CONDITION ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.condition.getTypeIdentifier( this.uow ), is(KomodoType.VDB_CONDITION));
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.condition.getPropertyNames( this.uow );
        final String[] rawProps = this.condition.getRawPropertyNames( this.uow );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveParentPermission() throws Exception {
        assertThat( this.condition.getParent( this.uow ), is( instanceOf( Permission.class ) ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.condition.addChild( this.uow, "blah", null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.condition.getPropertyNames( this.uow );
        final Filter[] filters = this.condition.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldSetConstraintValue() throws Exception {
        final boolean newValue = !Condition.DEFAULT_CONSTRAINT;
        this.condition.setConstraint( this.uow, newValue );
        assertThat( this.condition.isConstraint( this.uow ), is( newValue ) );
    }

    /*
     * ********************************************************************
     * *****                  Resolver Tests                          *****
     * ********************************************************************
     */

    @Test
    public void shouldCreateUsingResolver() throws Exception {
        final String name = "blah";
        final KomodoObject kobject = ConditionImpl.RESOLVER.create( this.uow,
                                                                    _repo,
                                                                    this.condition.getParent( this.uow ),
                                                                    name,
                                                                    null );
        assertThat( kobject, is( notNullValue() ) );
        assertThat( kobject, is( instanceOf( Condition.class ) ) );
        assertThat( kobject.getName( this.uow ), is( name ) );
    }

    @Test( expected = KException.class )
    public void shouldFailCreateUsingResolverWithInvalidParent() throws Exception {
        final KomodoObject bogusParent = _repo.add( this.uow, null, "bogus", null );
        ConditionImpl.RESOLVER.create( this.uow, _repo, bogusParent, "blah", null );
    }

}
