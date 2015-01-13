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
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.SchemaElement.SchemaElementType;
import org.komodo.relational.model.StatementOption;
import org.komodo.spi.KException;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class ProcedureImplTest extends RelationalModelTest {

    private static final String NAME = "procedure";

    private Procedure modelObject;

    @Before
    public void init() throws Exception {
        this.modelObject = RelationalModelFactory.createProcedure(null, _repo, null, NAME);
    }

    @Test
    public void shouldAddParameter() throws Exception {
        final String name = "param";
        final Parameter param = this.modelObject.addParameter(null, name);
        assertThat(param, is(notNullValue()));
        assertThat(param.getName(null), is(name));
    }

    @Test
    public void shouldAddStatementOption() throws Exception {
        final String name = "statementoption";
        final String value = "statementvalue";
        final StatementOption statementOption = this.modelObject.addStatementOption(null, name, value);
        assertThat(statementOption, is(notNullValue()));
        assertThat(statementOption.getName(null), is(name));
        assertThat(statementOption.getOption(null), is(value));
    }

    @Test
    public void shouldAllowEmptyAsClause() throws Exception {
        this.modelObject.setAsClauseStatement(null, "");
    }

    @Test
    public void shouldAllowNullAsClause() throws Exception {
        this.modelObject.setAsClauseStatement(null, null);
    }

    @Test
    public void shouldAllowNullSchemaElementType() throws Exception {
        this.modelObject.setSchemaElementType(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyParameterName() throws Exception {
        this.modelObject.addParameter(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyStatementOptionName() throws Exception {
        this.modelObject.addStatementOption(null, "", "blah");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyStatementOptionValue() throws Exception {
        this.modelObject.addStatementOption(null, "blah", "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullParameterName() throws Exception {
        this.modelObject.addParameter(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullStatementOptionName() throws Exception {
        this.modelObject.addStatementOption(null, null, "blah");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullStatementOptionValue() throws Exception {
        this.modelObject.addStatementOption(null, "blah", null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyParameterName() throws Exception {
        this.modelObject.removeParameter(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyStatementOptionName() throws Exception {
        this.modelObject.removeStatementOption(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullParameterName() throws Exception {
        this.modelObject.removeParameter(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullStatementOptionName() throws Exception {
        this.modelObject.removeStatementOption(null, null);
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownParameter() throws Exception {
        this.modelObject.removeParameter(null, "unknown");
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownStatementOption() throws Exception {
        this.modelObject.removeStatementOption(null, "unknown");
    }

    @Test
    public void shouldGetParameters() throws Exception {
        final int numParams = 5;

        for (int i = 0; i < numParams; ++i) {
            this.modelObject.addParameter(null, "param" + i);
        }

        assertThat(this.modelObject.getParameters(null).length, is(numParams));
    }

    @Test
    public void shouldGetStatementOptions() throws Exception {
        final int numStatementOptions = 5;

        for (int i = 0; i < numStatementOptions; ++i) {
            this.modelObject.addStatementOption(null, "statementoption" + i, "statementvalue" + i);
        }

        assertThat(this.modelObject.getStatementOptions(null).length, is(numStatementOptions));
    }

    @Test
    public void shouldHaveProcedureDescriptorAfterConstruction() throws Exception {
        assertThat(this.modelObject.isFunction(null), is(false));
        assertThat(this.modelObject.hasDescriptor(null, TeiidDdlLexicon.CreateProcedure.PROCEDURE_STATEMENT), is(true));
        assertThat(this.modelObject.hasDescriptor(null, TeiidDdlLexicon.CreateProcedure.FUNCTION_STATEMENT), is(false));
    }

    @Test
    public void shouldHaveResultSetAfterConstruction() throws Exception {
        assertThat(this.modelObject.getResultSet(null), is(notNullValue()));
    }

    @Test
    public void shouldHaveSchemaElementTypePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat(this.modelObject.getSchemaElementType(null), is(SchemaElementType.DEFAULT_VALUE));
        assertThat(this.modelObject.hasProperty(null, StandardDdlLexicon.DEFAULT_VALUE), is(false));
    }

    @Test
    public void shouldNotHaveAsClauseStatementPropertyAfterConstruction() throws Exception {
        assertThat(this.modelObject.getAsClauseStatement(null), is(nullValue()));
        assertThat(this.modelObject.hasProperty(null, TeiidDdlLexicon.CreateProcedure.STATEMENT), is(false));
    }

    @Test
    public void shouldNotHaveParametersAfterConstruction() throws Exception {
        assertThat(this.modelObject.getParameters(null).length, is(0));
    }

    @Test
    public void shouldRemoveParameter() throws Exception {
        final String name = "param";
        this.modelObject.addParameter(null, name);
        this.modelObject.removeParameter(null, name);
        assertThat(this.modelObject.getParameters(null).length, is(0));
    }

    @Test
    public void shouldRemoveStatementOption() throws Exception {
        final String name = "statementoption";
        this.modelObject.addStatementOption(null, name, "blah");
        this.modelObject.removeStatementOption(null, name);
        assertThat(this.modelObject.getStatementOptions(null).length, is(0));
    }

    @Test
    public void shouldSetAsClauseProperty() throws Exception {
        final String value = "asclause";
        this.modelObject.setAsClauseStatement(null, value);
        assertThat(this.modelObject.getAsClauseStatement(null), is(value));
        assertThat(this.modelObject.getProperty(null, TeiidDdlLexicon.CreateProcedure.STATEMENT).getStringValue(), is(value));
    }

    @Test
    public void shouldSetSchemaElementTypeProperty() throws Exception {
        final SchemaElementType value = SchemaElementType.VIRTUAL;
        this.modelObject.setSchemaElementType(null, value);
        assertThat(this.modelObject.getSchemaElementType(null), is(value));
        assertThat(this.modelObject.getProperty(null, TeiidDdlLexicon.SchemaElement.TYPE).getStringValue(), is(value.toString()));
    }

    @Test
    public void shouldSetToFunctionDescriptorProperty() throws Exception {
        this.modelObject.setFunction(null, true);
        assertThat(this.modelObject.isFunction(null), is(true));
        assertThat(this.modelObject.hasDescriptor(null, TeiidDdlLexicon.CreateProcedure.PROCEDURE_STATEMENT), is(false));
        assertThat(this.modelObject.hasDescriptor(null, TeiidDdlLexicon.CreateProcedure.FUNCTION_STATEMENT), is(true));
    }

}
