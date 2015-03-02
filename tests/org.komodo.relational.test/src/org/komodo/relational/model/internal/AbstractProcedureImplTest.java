/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertThat;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.model.AbstractProcedure;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.SchemaElement.SchemaElementType;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;

@SuppressWarnings( {"javadoc", "nls"} )
public final class AbstractProcedureImplTest extends RelationalModelTest {

    private AbstractProcedure procedure;

    @Before
    public void init() throws Exception {
        final Vdb vdb = RelationalModelFactory.createVdb( null, _repo, null, "vdb", "externalFilePath" );
        final Model model = RelationalModelFactory.createModel( null, _repo, vdb, "model" );
        this.procedure = RelationalModelFactory.createVirtualProcedure( null, _repo, model, "procedure" );
    }

    @Test
    public void shouldAddParameter() throws Exception {
        final String name = "param";
        final Parameter param = this.procedure.addParameter( null, name );
        assertThat( param, is( notNullValue() ) );
        assertThat( param.getName( null ), is( name ) );
    }

    @Test
    public void shouldAddStatementOption() throws Exception {
        final String name = "statementoption";
        final String value = "statementvalue";
        final StatementOption statementOption = this.procedure.setStatementOption( null, name, value );
        assertThat( statementOption, is( notNullValue() ) );
        assertThat( statementOption.getName( null ), is( name ) );
        assertThat( statementOption.getOption( null ), is( value ) );
    }

    @Test
    public void shouldAllowNullSchemaElementType() throws Exception {
        this.procedure.setSchemaElementType( null, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyParameterName() throws Exception {
        this.procedure.addParameter( null, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyStatementOptionName() throws Exception {
        this.procedure.setStatementOption( null, StringConstants.EMPTY_STRING, "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullParameterName() throws Exception {
        this.procedure.addParameter( null, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullStatementOptionName() throws Exception {
        this.procedure.setStatementOption( null, null, "blah" );
    }

    @Test( expected = KException.class )
    public void shouldFailAddingNullStatementOptionValue() throws Exception {
        this.procedure.setStatementOption( null, "blah", null );
    }

    @Test( expected = KException.class )
    public void shouldFailASettingEmptyStatementOptionValueWhenNeverAdded() throws Exception {
        this.procedure.setStatementOption( null, "blah", StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyDescriptionWhenNeverAdded() throws Exception {
        this.procedure.setDescription( null, StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyNameInSourceWhenNeverAdded() throws Exception {
        this.procedure.setNameInSource( null, StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyUuidWhenNeverAdded() throws Exception {
        this.procedure.setUuid( null, StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullDescriptionWhenNeverAdded() throws Exception {
        this.procedure.setDescription( null, null );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullNameInSourceWhenNeverAdded() throws Exception {
        this.procedure.setNameInSource( null, null );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullUuidWhenNeverAdded() throws Exception {
        this.procedure.setUuid( null, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyParameterName() throws Exception {
        this.procedure.removeParameter( null, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyStatementOptionName() throws Exception {
        this.procedure.removeStatementOption( null, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullParameterName() throws Exception {
        this.procedure.removeParameter( null, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullStatementOptionName() throws Exception {
        this.procedure.removeStatementOption( null, null );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownParameter() throws Exception {
        this.procedure.removeParameter( null, "unknown" );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownStatementOption() throws Exception {
        this.procedure.removeStatementOption( null, "unknown" );
    }

    @Test
    public void shouldGetParameters() throws Exception {
        final int numParams = 5;

        for (int i = 0; i < numParams; ++i) {
            this.procedure.addParameter( null, "param" + i );
        }

        assertThat( this.procedure.getParameters( null ).length, is( numParams ) );
        assertThat( this.procedure.getChildrenOfType( null, CreateProcedure.PARAMETER ).length, is( numParams ) );
    }

    @Test
    public void shouldGetStatementOptions() throws Exception {
        final int numStatementOptions = 5;

        for (int i = 0; i < numStatementOptions; ++i) {
            this.procedure.setStatementOption( null, "statementoption" + i, "statementvalue" + i );
        }

        assertThat( this.procedure.getStatementOptions( null ).length, is( numStatementOptions ) );
    }

    @Test
    public void shouldHaveDefaultUpdateCountAfterConstruction() throws Exception {
        assertThat( this.procedure.getUpdateCount( null ), is( AbstractProcedure.DEFAULT_UPDATE_COUNT ) );
    }

    @Test
    public void shouldNotHaveParametersAfterConstruction() throws Exception {
        assertThat( this.procedure.getParameters( null ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveParameter() throws Exception {
        final String name = "param";
        this.procedure.addParameter( null, name );
        this.procedure.removeParameter( null, name );
        assertThat( this.procedure.getParameters( null ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveStatementOption() throws Exception {
        final String name = "statementoption";
        this.procedure.setStatementOption( null, name, "blah" );
        this.procedure.removeStatementOption( null, name );
        assertThat( this.procedure.getStatementOptions( null ).length, is( 0 ) );
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String value = "description";
        this.procedure.setDescription( null, value );
        assertThat( this.procedure.getDescription( null ), is( value ) );
    }

    @Test
    public void shouldSetNameInSource() throws Exception {
        final String value = "nameInSource";
        this.procedure.setNameInSource( null, value );
        assertThat( this.procedure.getNameInSource( null ), is( value ) );
    }

    @Test
    public void shouldSetSchemaElementTypeProperty() throws Exception {
        final SchemaElementType value = SchemaElementType.VIRTUAL;
        this.procedure.setSchemaElementType( null, value );
        assertThat( this.procedure.getSchemaElementType( null ), is( value ) );
        assertThat( this.procedure.getProperty( null, TeiidDdlLexicon.SchemaElement.TYPE ).getStringValue( null ),
                    is( value.toString() ) );
    }

    @Test
    public void shouldSetUpdateCount() throws Exception {
        final int value = 10;
        this.procedure.setUpdateCount( null, value );
        assertThat( this.procedure.getUpdateCount( null ), is( value ) );
    }

    @Test
    public void shouldSetUuid() throws Exception {
        final String value = "uuid";
        this.procedure.setUuid( null, value );
        assertThat( this.procedure.getUuid( null ), is( value ) );
    }

}
