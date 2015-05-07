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
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import java.util.Arrays;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TableConstraint;
import org.komodo.spi.KException;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.Constraint;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ForeignKeyImplTest extends RelationalModelTest {

    private static final String NAME = "foreignKey";

    private ForeignKey foreignKey;
    private Table parentTable;
    private Table refTable;

    @Before
    public void init() throws Exception {
        this.parentTable = RelationalModelFactory.createTable( this.uow, _repo, mock( Model.class ), "parentTable" );
        this.refTable = RelationalModelFactory.createTable( this.uow, _repo, mock( Model.class ), "refTable" );
        this.foreignKey = RelationalModelFactory.createForeignKey( this.uow, _repo, this.parentTable, NAME, this.refTable );
        commit();
    }

    @Test
    public void shouldAddReferencesColumns() throws Exception {
        final Column columnA = RelationalModelFactory.createColumn( this.uow, _repo, this.refTable, "columnRefA" );
        this.foreignKey.addReferencesColumn( this.uow, columnA );

        final Column columnB = RelationalModelFactory.createColumn( this.uow, _repo, this.refTable, "columnRefB" );
        this.foreignKey.addReferencesColumn( this.uow, columnB );

        commit(); // must commit so that query used in getReferencesColumns will work

        assertThat( this.foreignKey.getReferencesColumns( this.uow ).length, is( 2 ) );
        assertThat( Arrays.asList( this.foreignKey.getReferencesColumns( this.uow ) ), hasItems( columnA, columnB ) );
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.foreignKey.isChildRestricted(), is( true ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullColumn() throws Exception {
        this.foreignKey.addColumn( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullReferencesColumn() throws Exception {
        this.foreignKey.addReferencesColumn( this.uow, null );
    }

    @Test
    public void shouldFailConstructionIfNotForeignKey() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new ForeignKeyImpl( this.uow, _repo, this.parentTable.getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectConstraintType() throws Exception {
        assertThat( this.foreignKey.getConstraintType(), is( TableConstraint.ConstraintType.FOREIGN_KEY ) );
        assertThat( this.foreignKey.getProperty( this.uow, TeiidDdlLexicon.Constraint.TYPE ).getStringValue( this.uow ),
                    is( TableConstraint.ConstraintType.FOREIGN_KEY.toValue() ) );
    }

    @Test
    public void shouldHaveCorrectDescriptor() throws Exception {
        assertThat( this.foreignKey.hasDescriptor( this.uow, Constraint.FOREIGN_KEY_CONSTRAINT ), is( true ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.foreignKey.getPropertyNames( this.uow );
        final String[] rawProps = this.foreignKey.getRawPropertyNames( this.uow );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveParentTableAfterConstruction() throws Exception {
        assertThat( this.foreignKey.getParent( this.uow ), is( instanceOf( Table.class ) ) );
        assertThat( this.foreignKey.getTable( this.uow ), is( this.parentTable ) );
    }

    @Test
    public void shouldHaveReferencesTableAfterConstruction() throws Exception {
        assertThat( this.foreignKey.getReferencesTable( this.uow ), is( this.refTable ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.foreignKey.addChild( this.uow, "blah", null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotAllowNullTableReference() throws Exception {
        this.foreignKey.setReferencesTable( this.uow, null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.foreignKey.getPropertyNames( this.uow );
        final Filter[] filters = this.foreignKey.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotHaveColumnReferencesAfterConstruction() throws Exception {
        assertThat( this.foreignKey.getReferencesColumns( this.uow ), is( notNullValue() ) );
        assertThat( this.foreignKey.getReferencesColumns( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveColumnsAfterConstruction() throws Exception {
        assertThat( this.foreignKey.getColumns( this.uow ), is( notNullValue() ) );
        assertThat( this.foreignKey.getColumns( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveReferencesColumn() throws Exception {
        final Column columnA = RelationalModelFactory.createColumn( this.uow, _repo, this.refTable, "removeRefColumnA" );
        this.foreignKey.addReferencesColumn( this.uow, columnA );
        commit(); // must commit so that query used in next method will work

        this.foreignKey.removeReferencesColumn( this.uow, columnA );
        assertThat( this.foreignKey.getReferencesColumns( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldSetTableReference() throws Exception {
        final Table newTable = RelationalModelFactory.createTable( this.uow, _repo, mock( Model.class ), "newTable" );
        this.foreignKey.setReferencesTable( this.uow, newTable );
        commit(); // must commit so that query used in next method will work

        assertThat( this.foreignKey.getReferencesTable( this.uow ), is( newTable ) );
    }

}
