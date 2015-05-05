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
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.RelationalObject.Filter;
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ConditionImplTest extends RelationalModelTest {

    private Condition condition;
    private DataRole dataRole;
    private Permission permission;
    private Vdb vdb;

    @Before
    public void init() throws Exception {
        this.vdb = RelationalModelFactory.createVdb( this.uow, _repo, null, "vdb", "/Users/sledge/hammer/MyVdb.vdb" );
        this.dataRole = RelationalModelFactory.createDataRole( this.uow, _repo, this.vdb, "dataRole" );
        this.permission = RelationalModelFactory.createPermission( this.uow, _repo, this.dataRole, "permission" );
        this.condition = RelationalModelFactory.createCondition( this.uow, _repo, this.permission, "condition" );
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
                new ConditionImpl( this.uow, _repo, this.vdb.getAbsolutePath() );
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

}
