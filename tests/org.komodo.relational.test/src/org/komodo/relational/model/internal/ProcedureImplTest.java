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
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.SchemaElement.SchemaElementType;
import org.komodo.relational.model.StatementOption;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class ProcedureImplTest extends RelationalModelTest {

    private static final String NAME = "procedure";

    private Procedure procedure;

    @Before
    public void init() throws Exception {
        this.procedure = RelationalModelFactory.createProcedure(null, _repo, mock(Model.class), NAME);
    }

    @Test
    public void shouldAddParameter() throws Exception {
        final String name = "param";
        final Parameter param = this.procedure.addParameter(null, name);
        assertThat(param, is(notNullValue()));
        assertThat(param.getName(null), is(name));
    }

    @Test
    public void shouldAddStatementOption() throws Exception {
        final String name = "statementoption";
        final String value = "statementvalue";
        final StatementOption statementOption = this.procedure.setStatementOption(null, name, value);
        assertThat(statementOption, is(notNullValue()));
        assertThat(statementOption.getName(null), is(name));
        assertThat(statementOption.getOption(null), is(value));
    }

    @Test
    public void shouldAllowEmptyAsClause() throws Exception {
        this.procedure.setAsClauseStatement(null, StringConstants.EMPTY_STRING);
    }

    @Test
    public void shouldAllowNullAsClause() throws Exception {
        this.procedure.setAsClauseStatement(null, null);
    }

    @Test
    public void shouldAllowNullSchemaElementType() throws Exception {
        this.procedure.setSchemaElementType(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyParameterName() throws Exception {
        this.procedure.addParameter(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyStatementOptionName() throws Exception {
        this.procedure.setStatementOption(null, StringConstants.EMPTY_STRING, "blah");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullParameterName() throws Exception {
        this.procedure.addParameter(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullStatementOptionName() throws Exception {
        this.procedure.setStatementOption(null, null, "blah");
    }

    @Test( expected = KException.class )
    public void shouldFailAddingNullStatementOptionValue() throws Exception {
        this.procedure.setStatementOption(null, "blah", null);
    }

    @Test( expected = KException.class )
    public void shouldFailASettingEmptyStatementOptionValueWhenNeverAdded() throws Exception {
        this.procedure.setStatementOption(null, "blah", StringConstants.EMPTY_STRING);
    }

    @Test
    public void shouldFailConstructionIfNotProcedure() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new ProcedureImpl(null, _repo, _repo.komodoLibrary(null).getAbsolutePath());
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyDescriptionWhenNeverAdded() throws Exception {
        this.procedure.setDescription(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyNameInSourceWhenNeverAdded() throws Exception {
        this.procedure.setNameInSource(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNativeQueryWithEmptyValueWhenItWasNeverAdded() throws Exception {
        this.procedure.setNativeQuery(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullDescriptionWhenNeverAdded() throws Exception {
        this.procedure.setDescription(null, null);
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullNameInSourceWhenNeverAdded() throws Exception {
        this.procedure.setNameInSource(null, null);
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullNativeQueryWhenNeverAdded() throws Exception {
        this.procedure.setNativeQuery(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyParameterName() throws Exception {
        this.procedure.removeParameter(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyStatementOptionName() throws Exception {
        this.procedure.removeStatementOption(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullParameterName() throws Exception {
        this.procedure.removeParameter(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullStatementOptionName() throws Exception {
        this.procedure.removeStatementOption(null, null);
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownParameter() throws Exception {
        this.procedure.removeParameter(null, "unknown");
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownStatementOption() throws Exception {
        this.procedure.removeStatementOption(null, "unknown");
    }

    @Test
    public void shouldGetParameters() throws Exception {
        final int numParams = 5;

        for (int i = 0; i < numParams; ++i) {
            this.procedure.addParameter(null, "param" + i);
        }

        assertThat(this.procedure.getParameters(null).length, is(numParams));
    }

    @Test
    public void shouldGetStatementOptions() throws Exception {
        final int numStatementOptions = 5;

        for (int i = 0; i < numStatementOptions; ++i) {
            this.procedure.setStatementOption(null, "statementoption" + i, "statementvalue" + i);
        }

        assertThat(this.procedure.getStatementOptions(null).length, is(numStatementOptions));
    }

    @Test
    public void shouldHaveDefaultUpdateCountAfterConstruction() throws Exception {
        assertThat(this.procedure.getUpdateCount(null), is(Procedure.DEFAULT_UPDATE_COUNT));
    }

    @Test
    public void shouldHaveResultSetAfterConstruction() throws Exception {
        assertThat(this.procedure.getResultSet(null), is(notNullValue()));
    }

    @Test
    public void shouldHaveSchemaElementTypePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat(this.procedure.getSchemaElementType(null), is(SchemaElementType.DEFAULT_VALUE));
        assertThat(this.procedure.hasProperty(null, StandardDdlLexicon.DEFAULT_VALUE), is(false));
    }

    @Test
    public void shouldNotHaveAsClauseStatementPropertyAfterConstruction() throws Exception {
        assertThat(this.procedure.getAsClauseStatement(null), is(nullValue()));
        assertThat(this.procedure.hasProperty(null, TeiidDdlLexicon.CreateProcedure.STATEMENT), is(false));
    }

    @Test
    public void shouldNotHaveParametersAfterConstruction() throws Exception {
        assertThat(this.procedure.getParameters(null).length, is(0));
    }

    @Test
    public void shouldRemoveParameter() throws Exception {
        final String name = "param";
        this.procedure.addParameter(null, name);
        this.procedure.removeParameter(null, name);
        assertThat(this.procedure.getParameters(null).length, is(0));
    }

    @Test
    public void shouldRemoveStatementOption() throws Exception {
        final String name = "statementoption";
        this.procedure.setStatementOption(null, name, "blah");
        this.procedure.removeStatementOption(null, name);
        assertThat(this.procedure.getStatementOptions(null).length, is(0));
    }

    @Test
    public void shouldSetAsClauseProperty() throws Exception {
        final String value = "asclause";
        this.procedure.setAsClauseStatement(null, value);
        assertThat(this.procedure.getAsClauseStatement(null), is(value));
        assertThat(this.procedure.getProperty(null, TeiidDdlLexicon.CreateProcedure.STATEMENT).getStringValue(null), is(value));
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String value = "description";
        this.procedure.setDescription(null, value);
        assertThat(this.procedure.getDescription(null), is(value));
    }

    @Test
    public void shouldSetNameInSource() throws Exception {
        final String value = "nameInSource";
        this.procedure.setNameInSource(null, value);
        assertThat(this.procedure.getNameInSource(null), is(value));
    }

    @Test
    public void shouldSetNativeQuery() throws Exception {
        final String value = "nativeQuery";
        this.procedure.setNativeQuery(null, value);
        assertThat(this.procedure.getNativeQuery(null), is(value));
    }

    @Test
    public void shouldSetSchemaElementTypeProperty() throws Exception {
        final SchemaElementType value = SchemaElementType.VIRTUAL;
        this.procedure.setSchemaElementType(null, value);
        assertThat(this.procedure.getSchemaElementType(null), is(value));
        assertThat(this.procedure.getProperty(null, TeiidDdlLexicon.SchemaElement.TYPE).getStringValue(null),
                   is(value.toString()));
    }

    @Test
    public void shouldSetUpdateCount() throws Exception {
        final int value = 10;
        this.procedure.setUpdateCount(null, value);
        assertThat(this.procedure.getUpdateCount(null), is(value));
    }

}
