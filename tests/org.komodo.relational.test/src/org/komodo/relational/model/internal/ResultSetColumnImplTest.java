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
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.RelationalObject.Filter;
import org.komodo.relational.model.ResultSetColumn;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ResultSetColumnImplTest extends RelationalModelTest {

    private static final String NAME = "column";

    private ResultSetColumn column;
    private TabularResultSet resultSet;

    @Before
    public void init() throws Exception {
        final Vdb vdb = RelationalModelFactory.createVdb( null, _repo, null, "vdb", "path" );
        final Model model = RelationalModelFactory.createModel( null, _repo, vdb, "model" );
        final StoredProcedure procedure = RelationalModelFactory.createStoredProcedure( null, _repo, model, "procedure" );
        this.resultSet = RelationalModelFactory.createTabularResultSet( null, _repo, procedure );
        this.column = RelationalModelFactory.createResultSetColumn( null, _repo, this.resultSet, NAME );
    }

    @Test
    public void shouldAllowEmptyDescription() throws Exception {
        this.column.setDescription( null, "blah" );
        this.column.setDescription( null, StringConstants.EMPTY_STRING );
        assertThat( this.column.getDescription( null ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowEmptyNameInSource() throws Exception {
        this.column.setNameInSource( null, "blah" );
        this.column.setNameInSource( null, StringConstants.EMPTY_STRING );
        assertThat( this.column.getNameInSource( null ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullDescription() throws Exception {
        this.column.setDescription( null, "blah" );
        this.column.setDescription( null, null );
        assertThat( this.column.getDescription( null ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullNameInSource() throws Exception {
        this.column.setNameInSource( null, "blah" );
        this.column.setNameInSource( null, null );
        assertThat( this.column.getNameInSource( null ), is( nullValue() ) );
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.column.isChildRestricted(), is( true ) );
    }

    @Test
    public void shouldFailConstructionIfNotResultSetColumn() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new ColumnImpl( null, _repo, _repo.komodoLibrary( null ).getAbsolutePath() );
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyUuidWhenNeverAdded() throws Exception {
        this.column.setUuid( null, StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullUuidWhenNeverAdded() throws Exception {
        this.column.setUuid( null, null );
    }

    @Test
    public void shouldHaveCorrectName() throws Exception {
        assertThat( this.column.getName( null ), is( NAME ) );
    }

    @Test
    public void shouldHaveDatatypeLengthPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getLength( null ), is( RelationalConstants.DEFAULT_LENGTH ) );
        assertThat( this.column.hasProperty( null, StandardDdlLexicon.DATATYPE_LENGTH ), is( false ) );
    }

    @Test
    public void shouldHaveDatatypeNamePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getDatatypeName( null ), is( RelationalConstants.DEFAULT_DATATYPE_NAME ) );
        assertThat( this.column.hasProperty( null, StandardDdlLexicon.DATATYPE_NAME ), is( false ) );
    }

    @Test
    public void shouldHaveDatatypePrecisionPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getPrecision( null ), is( RelationalConstants.DEFAULT_PRECISION ) );
        assertThat( this.column.hasProperty( null, StandardDdlLexicon.DATATYPE_PRECISION ), is( false ) );
    }

    @Test
    public void shouldHaveDatatypeScalePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getScale( null ), is( RelationalConstants.DEFAULT_SCALE ) );
        assertThat( this.column.hasProperty( null, StandardDdlLexicon.DATATYPE_SCALE ), is( false ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.column.getPropertyNames( null );
        final String[] rawProps = this.column.getRawPropertyNames( null );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveNullablePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.hasProperty( null, StandardDdlLexicon.NULLABLE ), is( true ) );
        assertThat( this.column.getNullable( null ), is( RelationalConstants.Nullable.DEFAULT_VALUE ) );
        assertThat( this.column.getProperty( null, StandardDdlLexicon.NULLABLE ).getStringValue( null ),
                    is( RelationalConstants.Nullable.DEFAULT_VALUE.toValue() ) );
    }

    @Test
    public void shouldHaveParentResultSet() throws Exception {
        assertThat( this.column.getParent( null ), is( instanceOf( TabularResultSet.class ) ) );
        assertThat( this.column.getParent( null ), is( ( KomodoObject )this.resultSet ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.column.addChild( null, "blah", null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.column.getPropertyNames( null );
        final Filter[] filters = this.column.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotCountStatementOptionsAsChildren() throws Exception {
        this.column.setUuid( null, "elvis" );
        this.column.setStatementOption( null, "sledge", "hammer" );
        assertThat( this.column.getChildren( null ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveDefaultValueAfterConstruction() throws Exception {
        assertThat( this.column.getDefaultValue( null ), is( nullValue() ) );
        assertThat( this.column.hasProperty( null, StandardDdlLexicon.DEFAULT_VALUE ), is( false ) );
    }

    @Test
    public void shouldRemoveDefaultValueWithEmptyString() throws Exception {
        this.column.setDefaultValue( null, "defaultValue" );
        this.column.setDefaultValue( null, StringConstants.EMPTY_STRING );
        assertThat( this.column.getDefaultValue( null ), is( nullValue() ) );
        assertThat( this.column.hasProperty( null, StandardDdlLexicon.DEFAULT_VALUE ), is( false ) );
    }

    @Test
    public void shouldRemoveDefaultValueWithNull() throws Exception {
        this.column.setDefaultValue( null, "defaultValue" );
        this.column.setDefaultValue( null, null );
        assertThat( this.column.getDefaultValue( null ), is( nullValue() ) );
        assertThat( this.column.hasProperty( null, StandardDdlLexicon.DEFAULT_VALUE ), is( false ) );
    }

    @Test
    public void shouldSetDatatypeLengthProperty() throws Exception {
        final long value = ( RelationalConstants.DEFAULT_LENGTH + 10 );
        this.column.setLength( null, value );
        assertThat( this.column.getLength( null ), is( value ) );
        assertThat( this.column.getProperty( null, StandardDdlLexicon.DATATYPE_LENGTH ).getLongValue( null ), is( value ) );
    }

    @Test
    public void shouldSetDatatypeNameProperty() throws Exception {
        final String value = "datatypename";
        this.column.setDatatypeName( null, value );
        assertThat( this.column.getDatatypeName( null ), is( value ) );
        assertThat( this.column.getProperty( null, StandardDdlLexicon.DATATYPE_NAME ).getStringValue( null ), is( value ) );
    }

    @Test
    public void shouldSetDatatypePrecisionProperty() throws Exception {
        final int value = 10;
        this.column.setPrecision( null, value );
        assertThat( this.column.getPrecision( null ), is( value ) );
        assertThat( this.column.getProperty( null, StandardDdlLexicon.DATATYPE_PRECISION ).getLongValue( null ),
                    is( ( long )value ) );
    }

    @Test
    public void shouldSetDatatypeScaleProperty() throws Exception {
        final int value = 10;
        this.column.setScale( null, value );
        assertThat( this.column.getScale( null ), is( value ) );
        assertThat( this.column.getProperty( null, StandardDdlLexicon.DATATYPE_SCALE ).getLongValue( null ), is( ( long )value ) );
    }

    @Test
    public void shouldSetDefaultValueProperty() throws Exception {
        final String value = "defaultvalue";
        this.column.setDefaultValue( null, value );
        assertThat( this.column.getDefaultValue( null ), is( value ) );
        assertThat( this.column.getProperty( null, StandardDdlLexicon.DEFAULT_VALUE ).getStringValue( null ), is( value ) );
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String value = "description";
        this.column.setDescription( null, value );
        assertThat( this.column.getDescription( null ), is( value ) );
    }

    @Test
    public void shouldSetNameInSource() throws Exception {
        final String value = "nameInSource";
        this.column.setNameInSource( null, value );
        assertThat( this.column.getNameInSource( null ), is( value ) );
    }

    @Test
    public void shouldSetNullableProperty() throws Exception {
        final Nullable value = Nullable.NO_NULLS;
        this.column.setNullable( null, value );
        assertThat( this.column.getNullable( null ), is( value ) );
        assertThat( this.column.getProperty( null, StandardDdlLexicon.NULLABLE ).getStringValue( null ), is( value.toValue() ) );
    }

    @Test
    public void shouldSetUuid() throws Exception {
        final String value = "uuid";
        this.column.setUuid( null, value );
        assertThat( this.column.getUuid( null ), is( value ) );
    }

}
