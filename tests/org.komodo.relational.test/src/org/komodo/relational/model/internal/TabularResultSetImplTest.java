/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsCollectionContaining.hasItem;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import java.util.Arrays;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.ResultSetColumn;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;

@SuppressWarnings( { "javadoc", "nls" } )
public final class TabularResultSetImplTest extends RelationalModelTest {

    private StoredProcedure procedure;
    private TabularResultSet resultSet;

    @Before
    public void init() throws Exception {
        final Model model = createModel();
        this.procedure = model.addStoredProcedure( this.uow, "procedure" );
        this.resultSet = this.procedure.setResultSet( this.uow, TabularResultSet.class );
        commit();
    }

    @Test
    public void shouldAddColumn() throws Exception {
        final String resultSetColumnName = "resultSetColumn";
        final ResultSetColumn resultSetColumn = this.resultSet.addColumn( this.uow, resultSetColumnName );
        assertThat( this.resultSet.getColumns( this.uow ).length, is( 1 ) );
        assertThat( resultSetColumn.getName( this.uow ), is( resultSetColumnName ) );
        assertThat( this.resultSet.getChildren( this.uow )[0], is( instanceOf( ResultSetColumn.class ) ) );
    }

    @Test
    public void shouldFailConstructionIfNotTabularResultSet() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new TabularResultSetImpl( this.uow, _repo, this.procedure.getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectChildTypes() {
        assertThat( Arrays.asList( this.resultSet.getChildTypes() ), hasItem( ResultSetColumn.IDENTIFIER ) );
        assertThat( this.resultSet.getChildTypes().length, is( 1 ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.resultSet.getTypeIdentifier( this.uow ), is(KomodoType.TABULAR_RESULT_SET));
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.resultSet.getPropertyNames( this.uow );
        final String[] rawProps = this.resultSet.getRawPropertyNames( this.uow );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowRename() throws Exception {
        this.resultSet.rename( this.uow, "newName" );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.resultSet.getPropertyNames( this.uow );
        final Filter[] filters = this.resultSet.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldObtainStrongTypedChildren() throws Exception {
        this.resultSet.addColumn( this.uow, "resultSetColumn" );
        assertThat( this.resultSet.getChildren( this.uow ).length, is( 1 ) );
        assertThat( this.resultSet.getChildren( this.uow )[0], is( instanceOf( ResultSetColumn.class ) ) );
    }

    /*
     * ********************************************************************
     * *****                  Resolver Tests                          *****
     * ********************************************************************
     */

    @Test
    public void shouldCreateUsingResolver() throws Exception {
        final KomodoObject kobject = TabularResultSetImpl.RESOLVER.create( this.uow, _repo, this.procedure, "blah", null );
        assertThat( kobject, is( notNullValue() ) );
        assertThat( kobject, is( instanceOf( TabularResultSet.class ) ) );
        assertThat( kobject.getName( this.uow ), is( CreateProcedure.RESULT_SET ) );
    }

    @Test( expected = KException.class )
    public void shouldFailCreateUsingResolverWithInvalidParent() throws Exception {
        final KomodoObject bogusParent = _repo.add( this.uow, null, "bogus", null );
        TabularResultSetImpl.RESOLVER.create( this.uow, _repo, bogusParent, "blah", null );
    }

}
