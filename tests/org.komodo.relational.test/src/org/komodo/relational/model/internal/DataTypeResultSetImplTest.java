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
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.relational.model.DataTypeResultSet.Type;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
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
        this.procedure = model.addStoredProcedure( getTransaction(), "procedure" );
        this.resultSet = this.procedure.setResultSet( getTransaction(), DataTypeResultSet.class );
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
                new DataTypeResultSetImpl( getTransaction(), _repo, this.procedure.getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectDisplayString() throws Exception {
        // STRING
        assertThat( this.resultSet.getDisplayString( getTransaction() ), is( "STRING" ) );

        // STRING(50)
        this.resultSet.setLength( getTransaction(), 50 );
        assertThat( this.resultSet.getDisplayString( getTransaction() ), is( "STRING(50)" ) );

        // STRING(50)[]
        this.resultSet.setArray( getTransaction(), true );
        assertThat( this.resultSet.getDisplayString( getTransaction() ), is( "STRING(50)[]" ) );
    }

    @Test
    public void shouldHaveCorrectName() throws Exception {
        assertThat( this.resultSet.getName( getTransaction() ), is( CreateProcedure.RESULT_SET ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.resultSet.getTypeIdentifier( getTransaction() ), is(KomodoType.DATA_TYPE_RESULT_SET));
    }

    @Test
    public void shouldHaveDatatypeLengthPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.resultSet.getLength( getTransaction() ), is( RelationalConstants.DEFAULT_LENGTH ) );
        assertThat( this.resultSet.hasProperty( getTransaction(), StandardDdlLexicon.DATATYPE_LENGTH ), is( false ) );
    }

    @Test
    public void shouldHaveDefaultTypeAfterConstruction() throws Exception {
        assertThat( this.resultSet.getType( getTransaction() ), is( Type.DEFAULT_VALUE ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.resultSet.getPropertyNames( getTransaction() );
        final String[] rawProps = this.resultSet.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveNullablePropertyAfterConstruction() throws Exception {
        assertThat( this.resultSet.hasProperty( getTransaction(), StandardDdlLexicon.NULLABLE ), is( true ) );
        assertThat( this.resultSet.getNullable( getTransaction() ), is( RelationalConstants.Nullable.DEFAULT_VALUE ) );
        assertThat( this.resultSet.getProperty( getTransaction(), StandardDdlLexicon.NULLABLE )
                                  .getStringValue( getTransaction() ),
                    is( RelationalConstants.Nullable.DEFAULT_VALUE.toValue() ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.resultSet.addChild( getTransaction(), "blah", null );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowRename() throws Exception {
        this.resultSet.rename( getTransaction(), "newName" );
    }

    @Test
    public void shouldNotBeAnArrayAfterConstruction() throws Exception {
        assertThat( this.resultSet.isArray( getTransaction() ), is( false ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.resultSet.getPropertyNames( getTransaction() );
        final Filter[] filters = this.resultSet.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldSetArray() throws Exception {
        this.resultSet.setArray( getTransaction(), true );
        assertThat( this.resultSet.isArray( getTransaction() ), is( true ) );
    }

    @Test
    public void shouldSetDatatypeLengthProperty() throws Exception {
        final long value = ( RelationalConstants.DEFAULT_LENGTH + 10 );
        this.resultSet.setLength( getTransaction(), value );
        assertThat( this.resultSet.getLength( getTransaction() ), is( value ) );
        assertThat( this.resultSet.getProperty( getTransaction(), StandardDdlLexicon.DATATYPE_LENGTH ).getLongValue( getTransaction() ),
                    is( value ) );
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String expected = "new description";
        this.resultSet.setDescription( getTransaction(), expected );
        assertThat( this.resultSet.getDescription( getTransaction() ), is( expected ) );
    }

    @Test
    public void shouldSetNameInSource() throws Exception {
        final String expected = "newNameInSource";
        this.resultSet.setNameInSource( getTransaction(), expected );
        assertThat( this.resultSet.getNameInSource( getTransaction() ), is( expected ) );
    }

    @Test
    public void shouldSetNullableProperty() throws Exception {
        final Nullable expected = Nullable.NO_NULLS;
        this.resultSet.setNullable( getTransaction(), expected );
        assertThat( this.resultSet.getNullable( getTransaction() ), is( expected ) );
        assertThat( this.resultSet.getProperty( getTransaction(), StandardDdlLexicon.NULLABLE )
                                  .getStringValue( getTransaction() ),
                    is( expected.toValue() ) );
    }

    @Test
    public void shouldSetType() throws Exception {
        final Type value = Type.BIGDECIMAL;
        this.resultSet.setType( getTransaction(), value );
        assertThat( this.resultSet.getType( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetTypeToDefaultWhenNull() throws Exception {
        this.resultSet.setType( getTransaction(), Type.BIGDECIMAL );
        this.resultSet.setType( getTransaction(), null );
        assertThat( this.resultSet.getType( getTransaction() ), is( Type.DEFAULT_VALUE ) );
    }

    @Test
    public void shouldUnsetNameInSourceWhenNullValue() throws Exception {
        this.resultSet.setNameInSource( getTransaction(), "newNameInSource" );
        this.resultSet.setNameInSource( getTransaction(), null );
        assertThat( this.resultSet.getNameInSource( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldSetDatatypePrecisionProperty() throws Exception {
        final long expected = 10;
        this.resultSet.setPrecision( getTransaction(), expected );
        assertThat( this.resultSet.getPrecision( getTransaction() ), is( expected ) );
        assertThat( this.resultSet.getProperty( getTransaction(), StandardDdlLexicon.DATATYPE_PRECISION )
                                  .getLongValue( getTransaction() ),
                    is( expected ) );
    }

    @Test
    public void shouldSetDatatypeScaleProperty() throws Exception {
        final long expected = 10;
        this.resultSet.setScale( getTransaction(), expected );
        assertThat( this.resultSet.getScale( getTransaction() ), is( expected ) );
        assertThat( this.resultSet.getProperty( getTransaction(), StandardDdlLexicon.DATATYPE_SCALE )
                                  .getLongValue( getTransaction() ),
                    is( expected ) );
    }

    @Test
    public void shouldUnsetDescriptionWhenNullValue() throws Exception {
        this.resultSet.setDescription( getTransaction(), "new description" );
        this.resultSet.setDescription( getTransaction(), null );
        assertThat( this.resultSet.getDescription( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldUnsetDescriptionWhenEmptyValue() throws Exception {
        this.resultSet.setDescription( getTransaction(), "new description" );
        this.resultSet.setDescription( getTransaction(), StringConstants.EMPTY_STRING );
        assertThat( this.resultSet.getDescription( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldUnsetNameInSourceWhenEmptyValue() throws Exception {
        this.resultSet.setNameInSource( getTransaction(), "newNameInSource" );
        this.resultSet.setNameInSource( getTransaction(), StringConstants.EMPTY_STRING );
        assertThat( this.resultSet.getNameInSource( getTransaction() ), is( nullValue() ) );
    }

    /*
     * ********************************************************************
     * *****                  Resolver Tests                          *****
     * ********************************************************************
     */

    @Test
    public void shouldCreateUsingResolver() throws Exception {
        final KomodoObject kobject = DataTypeResultSet.RESOLVER.create( getTransaction(), _repo, this.procedure, "blah", null );
        assertThat( kobject, is( notNullValue() ) );
        assertThat( kobject, is( instanceOf( DataTypeResultSet.class ) ) );
        assertThat( kobject.getName( getTransaction() ), is( CreateProcedure.RESULT_SET ) );
    }

    @Test( expected = KException.class )
    public void shouldFailCreateUsingResolverWithInvalidParent() throws Exception {
        final KomodoObject bogusParent = _repo.add( getTransaction(), null, "bogus", null );
        DataTypeResultSet.RESOLVER.create( getTransaction(), _repo, bogusParent, "blah", null );
    }

}
