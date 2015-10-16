/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

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
import org.komodo.relational.model.AccessPattern;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TableConstraint;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class AccessPatternImplTest extends RelationalModelTest {

    private static final String NAME = "accesspattern";

    private AccessPattern accessPattern;
    private Table table;

    @Before
    public void init() throws Exception {
        this.table = createTable();
        this.accessPattern = this.table.addAccessPattern( this.uow, NAME );
        commit();
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.accessPattern.isChildRestricted(), is( true ) );
    }

    @Test
    public void shouldFailConstructionIfNotAccessPattern() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new AccessPatternImpl( this.uow, _repo, this.table.getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectConstraintType() throws Exception {
        assertThat( this.accessPattern.getConstraintType(), is( TableConstraint.ConstraintType.ACCESS_PATTERN ) );
        assertThat( this.accessPattern.getRawProperty( this.uow, TeiidDdlLexicon.Constraint.TYPE ).getStringValue( this.uow ),
                    is( TableConstraint.ConstraintType.ACCESS_PATTERN.toValue() ) );
    }

    @Test
    public void shouldHaveCorrectDescriptor() throws Exception {
        assertThat( this.accessPattern.hasDescriptor( this.uow, TeiidDdlLexicon.Constraint.TABLE_ELEMENT ), is( true ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.accessPattern.getTypeIdentifier( this.uow ), is(KomodoType.ACCESS_PATTERN));
    }

    @Test
    public void shouldHaveCorrectName() throws Exception {
        assertThat( this.accessPattern.getName( this.uow ), is( NAME ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.accessPattern.getPropertyNames( this.uow );
        final String[] rawProps = this.accessPattern.getRawPropertyNames( this.uow );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveParentTable() throws Exception {
        assertThat( this.accessPattern.getParent( this.uow ), is( instanceOf( Table.class ) ) );
    }

    @Test
    public void shouldHaveParentTableAfterConstruction() throws Exception {
        assertThat( this.accessPattern.getTable( this.uow ), is( this.table ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.accessPattern.addChild( this.uow, "blah", null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.accessPattern.getPropertyNames( this.uow );
        final Filter[] filters = this.accessPattern.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    /*
     * ********************************************************************
     * *****                  Resolver Tests                          *****
     * ********************************************************************
     */

    @Test
    public void shouldCreateUsingResolver() throws Exception {
        final String name = "blah";
        final KomodoObject kobject = AccessPattern.RESOLVER.create( this.uow, _repo, this.table, name, null );
        assertThat( kobject, is( notNullValue() ) );
        assertThat( kobject, is( instanceOf( AccessPattern.class ) ) );
        assertThat( kobject.getName( this.uow ), is( name ) );
    }

    @Test( expected = KException.class )
    public void shouldFailCreateUsingResolverWithInvalidParent() throws Exception {
        final KomodoObject bogusParent = _repo.add( this.uow, null, "bogus", null );
        AccessPattern.RESOLVER.create( this.uow, _repo, bogusParent, "blah", null );
    }

}
