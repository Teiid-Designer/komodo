/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsCollectionContaining.hasItems;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import java.util.Arrays;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Index;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TableConstraint;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class IndexImplTest extends RelationalModelTest {

    private static final String NAME = "index";

    private Index index;
    private Table table;

    @Before
    public void init() throws Exception {
        this.table = createTable();
        this.index = this.table.addIndex( this.uow, NAME );
        commit();
    }

    @Test
    public void shouldAddColumns() throws Exception {
        final Column columnA = RelationalModelFactory.createColumn( this.uow, _repo, mock( Table.class ), "columnA" );
        this.index.addColumn( this.uow, columnA );

        final Column columnB = RelationalModelFactory.createColumn( this.uow, _repo, mock( Table.class ), "columnB" );
        this.index.addColumn( this.uow, columnB );

        commit(); // must commit so that query used in next method will work

        assertThat( this.index.getColumns( this.uow ).length, is( 2 ) );
        assertThat( Arrays.asList( this.index.getColumns( this.uow ) ), hasItems( columnA, columnB ) );
    }

    @Test
    public void shouldAllowEmptyExpression() throws Exception {
        this.index.setExpression( this.uow, EMPTY_STRING );
        assertThat( this.index.getExpression( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullExpression() throws Exception {
        this.index.setExpression( this.uow, null );
        assertThat( this.index.getExpression( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.index.isChildRestricted(), is( true ) );
    }

    @Test
    public void shouldFailConstructionIfNotIndex() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new IndexImpl( this.uow, _repo, this.table.getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectConstraintType() throws Exception {
        assertThat( this.index.getConstraintType(), is( TableConstraint.ConstraintType.INDEX ) );
        assertThat( this.index.getRawProperty( this.uow, TeiidDdlLexicon.Constraint.TYPE ).getStringValue( this.uow ),
                    is( TableConstraint.ConstraintType.INDEX.toValue() ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.index.getTypeIdentifier( this.uow ), is(KomodoType.INDEX));
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.index.getPropertyNames( this.uow );
        final String[] rawProps = this.index.getRawPropertyNames( this.uow );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveParentTableAfterConstruction() throws Exception {
        assertThat( this.index.getParent( this.uow ), is( instanceOf( Table.class ) ) );
        assertThat( this.index.getTable( this.uow ), is( this.table ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.index.addChild( this.uow, "blah", null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.index.getPropertyNames( this.uow );
        final Filter[] filters = this.index.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotHaveExpressionAfterConstruction() throws Exception {
        assertThat( this.index.getExpression( this.uow ), is( nullValue() ) );
        assertThat( this.index.hasProperty( this.uow, TeiidDdlLexicon.Constraint.EXPRESSION ), is( false ) );
    }

    @Test
    public void shouldRemoveExpressionWithEmptyString() throws Exception {
        this.index.setExpression( this.uow, "expression" );
        this.index.setExpression( this.uow, StringConstants.EMPTY_STRING );
        assertThat( this.index.getExpression( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveExpressionWithNull() throws Exception {
        this.index.setExpression( this.uow, "expression" );
        this.index.setExpression( this.uow, null );
        assertThat( this.index.getExpression( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldSetExpression() throws Exception {
        final String value = "expression";
        this.index.setExpression( this.uow, value );
        assertThat( this.index.getExpression( this.uow ), is( value ) );
    }

    /*
     * ********************************************************************
     * *****                  Resolver Tests                          *****
     * ********************************************************************
     */

    @Test
    public void shouldCreateUsingResolver() throws Exception {
        final String name = "blah";
        final KomodoObject kobject = Index.RESOLVER.create( this.uow, _repo, this.table, name, null );
        assertThat( kobject, is( notNullValue() ) );
        assertThat( kobject, is( instanceOf( Index.class ) ) );
        assertThat( kobject.getName( this.uow ), is( name ) );
    }

    @Test( expected = KException.class )
    public void shouldFailCreateUsingResolverWithInvalidParent() throws Exception {
        final KomodoObject bogusParent = _repo.add( this.uow, null, "bogus", null );
        Index.RESOLVER.create( this.uow, _repo, bogusParent, "blah", null );
    }

}
