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
import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalConstants.Direction;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.StatementOption;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;

@SuppressWarnings( {"javadoc", "nls"} )
public final class ParameterImplTest extends RelationalModelTest {

    private static final String NAME = "parameter";

    private Parameter parameter;
    private Procedure procedure;

    @Before
    public void init() throws Exception {
        this.procedure = RelationalModelFactory.createProcedure(null, _repo, mock(Model.class), "procedure");
        this.parameter = RelationalModelFactory.createParameter(null, _repo, this.procedure, NAME);
    }

    @Test
    public void shouldAddStatementOption() throws Exception {
        final String name = "statementoption";
        final String value = "statementvalue";
        final StatementOption statementOption = this.parameter.addStatementOption(null, name, value);
        assertThat(statementOption, is(notNullValue()));
        assertThat(statementOption.getName(null), is(name));
        assertThat(statementOption.getOption(null), is(value));
    }

    @Test
    public void shouldAllowEmptyDatatypeName() throws Exception {
        this.parameter.setDatatypeName(null, StringConstants.EMPTY_STRING);
    }

    @Test
    public void shouldAllowEmptyDefaultValue() throws Exception {
        this.parameter.setDefaultValue(null, StringConstants.EMPTY_STRING);
    }

    @Test
    public void shouldAllowNullDatatypeName() throws Exception {
        this.parameter.setDatatypeName(null, null);
    }

    @Test
    public void shouldAllowNullDefaultValue() throws Exception {
        this.parameter.setDefaultValue(null, null);
    }

    @Test
    public void shouldAllowNullDirection() throws Exception {
        this.parameter.setDirection(null, null);
    }

    @Test
    public void shouldAllowNullNullable() throws Exception {
        this.parameter.setNullable(null, null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyStatementOptionName() throws Exception {
        this.parameter.addStatementOption(null, StringConstants.EMPTY_STRING, "blah");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyStatementOptionValue() throws Exception {
        this.parameter.addStatementOption(null, "blah", StringConstants.EMPTY_STRING);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullStatementOptionName() throws Exception {
        this.parameter.addStatementOption(null, null, "blah");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullStatementOptionValue() throws Exception {
        this.parameter.addStatementOption(null, "blah", null);
    }

    @Test
    public void shouldFailConstructionIfNotParameter() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new ParameterImpl(null, _repo, this.procedure.getAbsolutePath());
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyStatementOptionName() throws Exception {
        this.parameter.removeStatementOption(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullStatementOptionName() throws Exception {
        this.parameter.removeStatementOption(null, null);
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownStatementOption() throws Exception {
        this.parameter.removeStatementOption(null, "unknown");
    }

    @Test
    public void shouldGetStatementOptions() throws Exception {
        final int numStatementOptions = 5;

        for (int i = 0; i < numStatementOptions; ++i) {
            this.parameter.addStatementOption(null, "statementoption" + i, "statementvalue" + i);
        }

        assertThat(this.parameter.getStatementOptions(null).length, is(numStatementOptions));
    }

    @Test
    public void shouldHaveCorrectDescriptor() throws Exception {
        assertThat(this.parameter.hasDescriptor(null, CreateProcedure.PARAMETER), is(true));
    }

    @Test
    public void shouldHaveDatatypeLengthPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat(this.parameter.getLength(null), is(RelationalConstants.DEFAULT_LENGTH));
        assertThat(this.parameter.hasProperty(null, StandardDdlLexicon.DATATYPE_LENGTH), is(false));
    }

    @Test
    public void shouldHaveDatatypeNamePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat(this.parameter.getDatatypeName(null), is(RelationalConstants.DEFAULT_DATATYPE_NAME));
        assertThat(this.parameter.hasProperty(null, StandardDdlLexicon.DATATYPE_NAME), is(false));
    }

    @Test
    public void shouldHaveDatatypePrecisionPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat(this.parameter.getPrecision(null), is(RelationalConstants.DEFAULT_PRECISION));
        assertThat(this.parameter.hasProperty(null, StandardDdlLexicon.DATATYPE_PRECISION), is(false));
    }

    @Test
    public void shouldHaveDatatypeScalePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat(this.parameter.getScale(null), is(RelationalConstants.DEFAULT_SCALE));
        assertThat(this.parameter.hasProperty(null, StandardDdlLexicon.DATATYPE_SCALE), is(false));
    }

    @Test
    public void shouldHaveDirectionPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat(this.parameter.getDirection(null), is(Direction.DEFAULT_VALUE));
        assertThat(this.parameter.hasProperty(null, TeiidDdlLexicon.CreateProcedure.PARAMETER_TYPE), is(true));
        assertThat(this.parameter.getProperty(null, TeiidDdlLexicon.CreateProcedure.PARAMETER_TYPE).getStringValue(null),
                   is(RelationalConstants.Direction.DEFAULT_VALUE.toString()));
    }

    @Test
    public void shouldHaveNullablePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat(this.parameter.getNullable(null), is(Nullable.DEFAULT_VALUE));
        assertThat(this.parameter.hasProperty(null, StandardDdlLexicon.NULLABLE), is(true));
        assertThat(this.parameter.getProperty(null, StandardDdlLexicon.NULLABLE).getStringValue(null),
                   is(RelationalConstants.Nullable.DEFAULT_VALUE.toString()));
    }

    @Test
    public void shouldHaveParentProcedureAfterConstruction() throws Exception {
        assertThat(this.parameter.getProcedure(null), is(this.procedure));
    }

    @Test
    public void shouldNotHaveDefaultValueAfterConstruction() throws Exception {
        assertThat(this.parameter.getDefaultValue(null), is(nullValue()));
        assertThat(this.parameter.hasProperty(null, StandardDdlLexicon.DEFAULT_VALUE), is(false));
    }

    @Test
    public void shouldRemoveStatementOption() throws Exception {
        final String name = "statementoption";
        this.parameter.addStatementOption(null, name, "blah");
        this.parameter.removeStatementOption(null, name);
        assertThat(this.parameter.getStatementOptions(null).length, is(0));
    }

    @Test
    public void shouldSetDatatypeLengthProperty() throws Exception {
        final long value = 10;
        this.parameter.setLength(null, value);
        assertThat(this.parameter.getLength(null), is(value));
        assertThat(this.parameter.getProperty(null, StandardDdlLexicon.DATATYPE_LENGTH).getLongValue(null), is(value));
    }

    @Test
    public void shouldSetDatatypeNameProperty() throws Exception {
        final String value = "datatypename";
        this.parameter.setDatatypeName(null, value);
        assertThat(this.parameter.getDatatypeName(null), is(value));
        assertThat(this.parameter.getProperty(null, StandardDdlLexicon.DATATYPE_NAME).getStringValue(null), is(value));
    }

    @Test
    public void shouldSetDatatypePrecisionProperty() throws Exception {
        final int value = 10;
        this.parameter.setPrecision(null, value);
        assertThat(this.parameter.getPrecision(null), is(value));
        assertThat(this.parameter.getProperty(null, StandardDdlLexicon.DATATYPE_PRECISION).getLongValue(null), is((long)value));
    }

    @Test
    public void shouldSetDatatypeScaleProperty() throws Exception {
        final int value = 10;
        this.parameter.setScale(null, value);
        assertThat(this.parameter.getScale(null), is(value));
        assertThat(this.parameter.getProperty(null, StandardDdlLexicon.DATATYPE_SCALE).getLongValue(null), is((long)value));
    }

    @Test
    public void shouldSetDefaultValueProperty() throws Exception {
        final String value = "defaultvalue";
        this.parameter.setDefaultValue(null, value);
        assertThat(this.parameter.getDefaultValue(null), is(value));
        assertThat(this.parameter.getProperty(null, StandardDdlLexicon.DEFAULT_VALUE).getStringValue(null), is(value));
    }

    @Test
    public void shouldSetDirectionProperty() throws Exception {
        final Direction value = Direction.IN_OUT;
        this.parameter.setDirection(null, value);
        assertThat(this.parameter.getDirection(null), is(value));
        assertThat(this.parameter.getProperty(null, TeiidDdlLexicon.CreateProcedure.PARAMETER_TYPE).getStringValue(null),
                   is(value.toString()));
    }

    @Test
    public void shouldSetNullableProperty() throws Exception {
        final Nullable value = Nullable.NO_NULLS;
        this.parameter.setNullable(null, value);
        assertThat(this.parameter.getNullable(null), is(value));
        assertThat(this.parameter.getProperty(null, StandardDdlLexicon.NULLABLE).getStringValue(null), is(value.toString()));
    }

}
