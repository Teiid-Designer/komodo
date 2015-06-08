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
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TableConstraint;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class UniqueConstraintImplTest extends RelationalModelTest {

    private static final String NAME = "uniqueconstraint";

    private Table table;
    private UniqueConstraint uniqueConstraint;

    @Before
    public void init() throws Exception {
        this.table = RelationalModelFactory.createTable( this.uow, _repo, mock( Model.class ), "table" );
        this.uniqueConstraint = RelationalModelFactory.createUniqueConstraint( this.uow, _repo, this.table, NAME );
        commit();
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.uniqueConstraint.isChildRestricted(), is( true ) );
    }

    @Test
    public void shouldFailConstructionIfNotUniqueConstraint() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new UniqueConstraintImpl( this.uow, _repo, this.table.getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectConstraintType() throws Exception {
        assertThat( this.uniqueConstraint.getConstraintType(), is( TableConstraint.ConstraintType.UNIQUE ) );
        assertThat( this.uniqueConstraint.getProperty( this.uow, TeiidDdlLexicon.Constraint.TYPE ).getStringValue( this.uow ),
                    is( TableConstraint.ConstraintType.UNIQUE.toValue() ) );
    }

    @Test
    public void shouldHaveCorrectDescriptor() throws Exception {
        assertThat( this.uniqueConstraint.hasDescriptor( this.uow, TeiidDdlLexicon.Constraint.TABLE_ELEMENT ), is( true ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.uniqueConstraint.getTypeIdentifier( this.uow ), is(KomodoType.UNIQUE_CONSTRAINT));
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.uniqueConstraint.getPropertyNames( this.uow );
        final String[] rawProps = this.uniqueConstraint.getRawPropertyNames( this.uow );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveParentTableAfterConstruction() throws Exception {
        assertThat( this.uniqueConstraint.getParent( this.uow ), is( instanceOf( Table.class ) ) );
        assertThat( this.uniqueConstraint.getTable( this.uow ), is( this.table ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.uniqueConstraint.addChild( this.uow, "blah", null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.uniqueConstraint.getPropertyNames( this.uow );
        final Filter[] filters = this.uniqueConstraint.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

}
