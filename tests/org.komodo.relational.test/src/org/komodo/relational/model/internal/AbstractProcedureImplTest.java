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

@SuppressWarnings( { "javadoc", "nls" } )
public final class AbstractProcedureImplTest extends RelationalModelTest {

    private AbstractProcedure procedure;

    @Before
    public void init() throws Exception {
        final Vdb vdb = RelationalModelFactory.createVdb( this.uow, _repo, null, "vdb", "externalFilePath" );
        final Model model = RelationalModelFactory.createModel( this.uow, _repo, vdb, "model" );
        this.procedure = RelationalModelFactory.createVirtualProcedure( this.uow, _repo, model, "procedure" );
        commit();
    }

    @Test
    public void shouldAddParameter() throws Exception {
        final String name = "param";

        // setup
        this.procedure.addParameter( this.uow, name );

        // tests
        assertThat( this.procedure.getChildren( this.uow ).length, is( 1 ) );
        assertThat( this.procedure.getChildren( this.uow )[0].getName( this.uow ), is( name ) );
        assertThat( this.procedure.getChildren( this.uow )[0], is( instanceOf( Parameter.class ) ) );
    }

    @Test
    public void shouldAddStatementOption() throws Exception {
        // setup
        final String name = "statementoption";
        final String value = "statementvalue";
        final StatementOption statementOption = this.procedure.setStatementOption( this.uow, name, value );

        // tests
        assertThat( statementOption, is( notNullValue() ) );
        assertThat( statementOption.getName( this.uow ), is( name ) );
        assertThat( statementOption.getOption( this.uow ), is( value ) );
        assertThat( this.procedure.getChildren( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldAllowNullSchemaElementType() throws Exception {
        this.procedure.setSchemaElementType( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyParameterName() throws Exception {
        this.procedure.addParameter( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyStatementOptionName() throws Exception {
        this.procedure.setStatementOption( this.uow, StringConstants.EMPTY_STRING, "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullParameterName() throws Exception {
        this.procedure.addParameter( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullStatementOptionName() throws Exception {
        this.procedure.setStatementOption( this.uow, null, "blah" );
    }

    @Test( expected = KException.class )
    public void shouldFailAddingNullStatementOptionValue() throws Exception {
        this.procedure.setStatementOption( this.uow, "blah", null );
    }

    @Test( expected = KException.class )
    public void shouldFailASettingEmptyStatementOptionValueWhenNeverAdded() throws Exception {
        this.procedure.setStatementOption( this.uow, "blah", StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyDescriptionWhenNeverAdded() throws Exception {
        this.procedure.setDescription( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyNameInSourceWhenNeverAdded() throws Exception {
        this.procedure.setNameInSource( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyUuidWhenNeverAdded() throws Exception {
        this.procedure.setUuid( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullDescriptionWhenNeverAdded() throws Exception {
        this.procedure.setDescription( this.uow, null );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullNameInSourceWhenNeverAdded() throws Exception {
        this.procedure.setNameInSource( this.uow, null );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullUuidWhenNeverAdded() throws Exception {
        this.procedure.setUuid( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyParameterName() throws Exception {
        this.procedure.removeParameter( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyStatementOptionName() throws Exception {
        this.procedure.removeStatementOption( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullParameterName() throws Exception {
        this.procedure.removeParameter( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullStatementOptionName() throws Exception {
        this.procedure.removeStatementOption( this.uow, null );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownParameter() throws Exception {
        this.procedure.removeParameter( this.uow, "unknown" );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownStatementOption() throws Exception {
        this.procedure.removeStatementOption( this.uow, "unknown" );
    }

    @Test
    public void shouldGetParameters() throws Exception {
        // setup
        final int numParams = 5;

        for ( int i = 0; i < numParams; ++i ) {
            this.procedure.addParameter( this.uow, "param" + i );
        }

        // tests
        assertThat( this.procedure.getParameters( this.uow ).length, is( numParams ) );
        assertThat( this.procedure.getChildrenOfType( this.uow, CreateProcedure.PARAMETER ).length, is( numParams ) );
    }

    @Test
    public void shouldGetStatementOptions() throws Exception {
        // setup
        final int numStatementOptions = 5;

        for ( int i = 0; i < numStatementOptions; ++i ) {
            this.procedure.setStatementOption( this.uow, "statementoption" + i, "statementvalue" + i );
        }

        // tests
        assertThat( this.procedure.getStatementOptions( this.uow ).length, is( numStatementOptions ) );
    }

    @Test
    public void shouldHaveDefaultUpdateCountAfterConstruction() throws Exception {
        assertThat( this.procedure.getUpdateCount( this.uow ), is( AbstractProcedure.DEFAULT_UPDATE_COUNT ) );
    }

    @Test
    public void shouldNotCountStatementOptionsAsChildren() throws Exception {
        // setup
        this.procedure.setNameInSource( this.uow, "elvis" );
        this.procedure.setStatementOption( this.uow, "sledge", "hammer" );

        // tests
        assertThat( this.procedure.getChildren( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveParametersAfterConstruction() throws Exception {
        assertThat( this.procedure.getParameters( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveParameter() throws Exception {
        // setup
        final String name = "param";
        this.procedure.addParameter( this.uow, name );
        this.procedure.removeParameter( this.uow, name );

        // tests
        assertThat( this.procedure.getParameters( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveStatementOption() throws Exception {
        // setup
        final String name = "statementoption";
        this.procedure.setStatementOption( this.uow, name, "blah" );
        this.procedure.removeStatementOption( this.uow, name );

        // tests
        assertThat( this.procedure.getStatementOptions( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldSetDescription() throws Exception {
        // setup
        final String value = "description";
        this.procedure.setDescription( this.uow, value );

        // tests
        assertThat( this.procedure.getDescription( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetNameInSource() throws Exception {
        // setup
        final String value = "nameInSource";
        this.procedure.setNameInSource( this.uow, value );

        // tests
        assertThat( this.procedure.getNameInSource( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetSchemaElementTypeProperty() throws Exception {
        // setup
        final SchemaElementType value = SchemaElementType.VIRTUAL;
        this.procedure.setSchemaElementType( this.uow, value );

        // tests
        assertThat( this.procedure.getSchemaElementType( this.uow ), is( value ) );
        assertThat( this.procedure.getProperty( this.uow, TeiidDdlLexicon.SchemaElement.TYPE ).getStringValue( this.uow ),
                    is( value.name() ) );
    }

    @Test
    public void shouldSetUpdateCount() throws Exception {
        // setup
        final int value = 10;
        this.procedure.setUpdateCount( this.uow, value );

        // tests
        assertThat( this.procedure.getUpdateCount( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetUuid() throws Exception {
        // setup
        final String value = "uuid";
        this.procedure.setUuid( this.uow, value );

        // tests
        assertThat( this.procedure.getUuid( this.uow ), is( value ) );
    }

}
