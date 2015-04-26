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
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.relational.model.DataTypeResultSet.Type;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.spi.KException;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;

@SuppressWarnings( { "javadoc", "nls" } )
public class DataTypeResultSetImplTest extends RelationalModelTest {

    private StoredProcedure procedure;
    private DataTypeResultSet resultSet;

    @Before
    public void init() throws Exception {
        this.procedure = RelationalModelFactory.createStoredProcedure( null, _repo, mock( Model.class ), "procedure" );
        this.resultSet = RelationalModelFactory.createDataTypeResultSet( null, _repo, this.procedure );
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.resultSet.isChildRestricted(), is( true ) );
    }

    @Test
    public void shouldFailConstructionIfNotDataTypeResultSet() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new DataTypeResultSetImpl( null, _repo, this.procedure.getAbsolutePath() );
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectDisplayString() throws Exception {
        // STRING
        assertThat( this.resultSet.getDisplayString( null ), is( "STRING" ) );

        // STRING(50)
        this.resultSet.setLength( null, 50 );
        assertThat( this.resultSet.getDisplayString( null ), is( "STRING(50)" ) );

        // STRING(50)[]
        this.resultSet.setArray( null, true );
        assertThat( this.resultSet.getDisplayString( null ), is( "STRING(50)[]" ) );
    }

    @Test
    public void shouldHaveCorrectName() throws Exception {
        assertThat( this.resultSet.getName( null ), is( CreateProcedure.RESULT_SET ) );
    }

    @Test
    public void shouldHaveDatatypeLengthPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat( this.resultSet.getLength( null ), is( RelationalConstants.DEFAULT_LENGTH ) );
        assertThat( this.resultSet.hasProperty( null, StandardDdlLexicon.DATATYPE_LENGTH ), is( false ) );
    }

    @Test
    public void shouldHaveDefaultTypeAfterConstruction() throws Exception {
        assertThat( this.resultSet.getType( null ), is( Type.DEFAULT_VALUE ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.resultSet.addChild( null, "blah", null );
    }

    @Test
    public void shouldNotBeAnArrayAfterConstruction() throws Exception {
        assertThat( this.resultSet.isArray( null ), is( false ) );
    }

    @Test
    public void shouldSetArray() throws Exception {
        this.resultSet.setArray( null, true );
        assertThat( this.resultSet.isArray( null ), is( true ) );
    }

    @Test
    public void shouldSetDatatypeLengthProperty() throws Exception {
        final long value = ( RelationalConstants.DEFAULT_LENGTH + 10 );
        this.resultSet.setLength( null, value );
        assertThat( this.resultSet.getLength( null ), is( value ) );
        assertThat( this.resultSet.getProperty( null, StandardDdlLexicon.DATATYPE_LENGTH ).getLongValue( null ), is( value ) );
    }

    @Test
    public void shouldSetType() throws Exception {
        final Type value = Type.BIGDECIMAL;
        this.resultSet.setType( null, value );
        assertThat( this.resultSet.getType( null ), is( value ) );
    }

    @Test
    public void shouldSetTypeToDefaultWhenNull() throws Exception {
        this.resultSet.setType( null, Type.BIGDECIMAL );
        this.resultSet.setType( null, null );
        assertThat( this.resultSet.getType( null ), is( Type.DEFAULT_VALUE ) );
    }

}
