/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TableConstraint;
import org.komodo.spi.KException;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class TableConstraintTest extends RelationalModelTest {

    private static final String NAME = "tableConstraint";

    private TableConstraint constraint;
    private Table table;

    @Before
    public void init() throws Exception {
        this.table = createTable();
        this.constraint = this.table.addAccessPattern( this.uow, NAME );
        commit();
    }

    @Test
    public void shouldAddColumn() throws Exception {
        final Column column = RelationalModelFactory.createColumn( this.uow, _repo, mock( Table.class ), "column" );
        this.constraint.addColumn( this.uow, column );

        assertThat( this.constraint.hasProperty( this.uow, TeiidDdlLexicon.Constraint.REFERENCES ), is( true ) );
        assertThat( this.constraint.getProperty( this.uow, TeiidDdlLexicon.Constraint.REFERENCES ).getValues( this.uow ).length,
                    is( 1 ) );

        commit(); // must commit so that query used in next method will work
        assertThat( this.constraint.getColumns( this.uow ).length, is( 1 ) );
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.constraint.isChildRestricted(), is( true ) );
    }

    @Test( expected = KException.class )
    public void shouldFailWhenRemovingColumnThatWasNeverAdded() throws Exception {
        final Column column = RelationalModelFactory.createColumn( this.uow, _repo, mock( Table.class ), "column" );
        this.constraint.removeColumn( this.uow, column );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.constraint.getPropertyNames( this.uow );
        final String[] rawProps = this.constraint.getRawPropertyNames( this.uow );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveTableAfterConstruction() throws Exception {
        assertThat( this.constraint.getTable( this.uow ), is( this.table ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.constraint.addChild( this.uow, "blah", null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddNullColumn() throws Exception {
        this.constraint.addColumn( this.uow, null );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotBeAbleToSetConstraintType() throws Exception {
        this.constraint.setProperty( this.uow, TeiidDdlLexicon.Constraint.TYPE, "INDEX" );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.constraint.getPropertyNames( this.uow );
        final Filter[] filters = this.constraint.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotHaveColumnsAfterConstruction() throws Exception {
        assertThat( this.constraint.getColumns( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveColumn() throws Exception {
        final Column column = RelationalModelFactory.createColumn( this.uow, _repo, mock( Table.class ), "column" );
        this.constraint.addColumn( this.uow, column );
        commit(); // must commit so that query used in next method will work

        this.constraint.removeColumn( this.uow, column );
        assertThat( this.constraint.hasProperty( this.uow, TeiidDdlLexicon.Constraint.REFERENCES ), is( false ) );
    }

}
