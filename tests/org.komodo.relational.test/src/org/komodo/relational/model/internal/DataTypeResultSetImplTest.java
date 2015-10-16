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
import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.relational.model.DataTypeResultSet.Type;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;

@SuppressWarnings( { "javadoc", "nls" } )
public final class DataTypeResultSetImplTest extends RelationalModelTest {

    private StoredProcedure procedure;
    private DataTypeResultSet resultSet;

    @Before
    public void init() throws Exception {
        final Model model = createModel();
        this.procedure = model.addStoredProcedure( this.uow, "procedure" );
        this.resultSet = this.procedure.setResultSet( this.uow, DataTypeResultSet.class );
        commit();
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.resultSet.isChildRestricted(), is( true ) );
    }

    @Test
    public void shouldFailConstructionIfNotDataTypeResultSet() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new DataTypeResultSetImpl( this.uow, _repo, this.procedure.getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectDisplayString() throws Exception {
        // STRING
        assertThat( this.resultSet.getDisplayString( this.uow ), is( "STRING" ) );

        // STRING(50)
        this.resultSet.setLength( this.uow, 50 );
        assertThat( this.resultSet.getDisplayString( this.uow ), is( "STRING(50)" ) );

        // STRING(50)[]
        this.resultSet.setArray( this.uow, true );
        assertThat( this.resultSet.getDisplayString( this.uow ), is( "STRING(50)[]" ) );
    }

    @Test
    public void shouldHaveCorrectName() throws Exception {
        assertThat( this.resultSet.getName( this.uow ), is( CreateProcedure.RESULT_SET ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.resultSet.getTypeIdentifier( this.uow ), is(KomodoType.DATA_TYPE_RESULT_SET));
    }

    @Test
    public void shouldHaveDatatypeLengthPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.resultSet.getLength( this.uow ), is( RelationalConstants.DEFAULT_LENGTH ) );
        assertThat( this.resultSet.hasProperty( this.uow, StandardDdlLexicon.DATATYPE_LENGTH ), is( false ) );
    }

    @Test
    public void shouldHaveDefaultTypeAfterConstruction() throws Exception {
        assertThat( this.resultSet.getType( this.uow ), is( Type.DEFAULT_VALUE ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.resultSet.getPropertyNames( this.uow );
        final String[] rawProps = this.resultSet.getRawPropertyNames( this.uow );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.resultSet.addChild( this.uow, "blah", null );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowRename() throws Exception {
        this.resultSet.rename( this.uow, "newName" );
    }

    @Test
    public void shouldNotBeAnArrayAfterConstruction() throws Exception {
        assertThat( this.resultSet.isArray( this.uow ), is( false ) );
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
    public void shouldSetArray() throws Exception {
        this.resultSet.setArray( this.uow, true );
        assertThat( this.resultSet.isArray( this.uow ), is( true ) );
    }

    @Test
    public void shouldSetDatatypeLengthProperty() throws Exception {
        final long value = ( RelationalConstants.DEFAULT_LENGTH + 10 );
        this.resultSet.setLength( this.uow, value );
        assertThat( this.resultSet.getLength( this.uow ), is( value ) );
        assertThat( this.resultSet.getProperty( this.uow, StandardDdlLexicon.DATATYPE_LENGTH ).getLongValue( this.uow ),
                    is( value ) );
    }

    @Test
    public void shouldSetType() throws Exception {
        final Type value = Type.BIGDECIMAL;
        this.resultSet.setType( this.uow, value );
        assertThat( this.resultSet.getType( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetTypeToDefaultWhenNull() throws Exception {
        this.resultSet.setType( this.uow, Type.BIGDECIMAL );
        this.resultSet.setType( this.uow, null );
        assertThat( this.resultSet.getType( this.uow ), is( Type.DEFAULT_VALUE ) );
    }

    /*
     * ********************************************************************
     * *****                  Resolver Tests                          *****
     * ********************************************************************
     */

    @Test
    public void shouldCreateUsingResolver() throws Exception {
        final KomodoObject kobject = DataTypeResultSet.RESOLVER.create( this.uow, _repo, this.procedure, "blah", null );
        assertThat( kobject, is( notNullValue() ) );
        assertThat( kobject, is( instanceOf( DataTypeResultSet.class ) ) );
        assertThat( kobject.getName( this.uow ), is( CreateProcedure.RESULT_SET ) );
    }

    @Test( expected = KException.class )
    public void shouldFailCreateUsingResolverWithInvalidParent() throws Exception {
        final KomodoObject bogusParent = _repo.add( this.uow, null, "bogus", null );
        DataTypeResultSet.RESOLVER.create( this.uow, _repo, bogusParent, "blah", null );
    }

}
