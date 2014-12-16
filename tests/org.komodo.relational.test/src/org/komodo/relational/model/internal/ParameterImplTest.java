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
import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalConstants.Direction;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.RelationalModelFactory;
import org.komodo.relational.model.StatementOption;
import org.komodo.spi.KException;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class ParameterImplTest extends RelationalModelTest {

    private static final String NAME = "parameter";

    private Procedure parent;
    private Parameter modelObject;

    @Before
    public void init() throws Exception {
        this.parent = RelationalModelFactory.createProcedure(null, _repo, null, "procedure");
        this.modelObject = RelationalModelFactory.createParameter(null, _repo, this.parent, NAME);
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
    public void shouldAllowEmptyDatatypeName() throws Exception {
        this.modelObject.setDatatypeName(null, "");
    }

    @Test
    public void shouldAllowEmptyDefaultValue() throws Exception {
        this.modelObject.setDefaultValue(null, null);
    }

    @Test
    public void shouldAllowNullDatatypeName() throws Exception {
        this.modelObject.setDatatypeName(null, null);
    }

    @Test
    public void shouldAllowNullDefaultValue() throws Exception {
        this.modelObject.setDefaultValue(null, null);
    }

    @Test
    public void shouldAllowNullDirection() throws Exception {
        this.modelObject.setDirection(null, null);
    }

    @Test
    public void shouldAllowNullNullable() throws Exception {
        this.modelObject.setNullable(null, null);
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
    public void shouldFailAddingNullStatementOptionName() throws Exception {
        this.modelObject.addStatementOption(null, null, "blah");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullStatementOptionValue() throws Exception {
        this.modelObject.addStatementOption(null, "blah", null);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyStatementOptionName() throws Exception {
        this.modelObject.removeStatementOption(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullStatementOptionName() throws Exception {
        this.modelObject.removeStatementOption(null, null);
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownStatementOption() throws Exception {
        this.modelObject.removeStatementOption(null, "unknown");
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
    public void shouldHaveDatatypeLengthPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat(this.modelObject.getLength(null), is(RelationalConstants.DEFAULT_LENGTH));
        assertThat(this.modelObject.hasProperty(null, StandardDdlLexicon.DATATYPE_LENGTH), is(false));
    }

    @Test
    public void shouldHaveDatatypeNamePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat(this.modelObject.getDatatypeName(null), is(RelationalConstants.DEFAULT_DATATYPE_NAME));
        assertThat(this.modelObject.hasProperty(null, StandardDdlLexicon.DATATYPE_NAME), is(false));
    }

    @Test
    public void shouldHaveDatatypePrecisionPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat(this.modelObject.getPrecision(null), is(RelationalConstants.DEFAULT_PRECISION));
        assertThat(this.modelObject.hasProperty(null, StandardDdlLexicon.DATATYPE_PRECISION), is(false));
    }

    @Test
    public void shouldHaveDatatypeScalePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat(this.modelObject.getScale(null), is(RelationalConstants.DEFAULT_SCALE));
        assertThat(this.modelObject.hasProperty(null, StandardDdlLexicon.DATATYPE_SCALE), is(false));
    }

    @Test
    public void shouldHaveDefaultValuePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat(this.modelObject.getDefaultValue(null), is(RelationalConstants.DEFAULT_VALUE));
        assertThat(this.modelObject.hasProperty(null, StandardDdlLexicon.DEFAULT_VALUE), is(false));
    }

    @Test
    public void shouldHaveDirectionPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat(this.modelObject.getDirection(null), is(Direction.DEFAULT_VALUE));
        assertThat(this.modelObject.hasProperty(null, TeiidDdlLexicon.CreateProcedure.PARAMETER_TYPE), is(true));
        assertThat(this.modelObject.getProperty(null, TeiidDdlLexicon.CreateProcedure.PARAMETER_TYPE).getStringValue(),
                   is(RelationalConstants.Direction.DEFAULT_VALUE.toString()));
    }

    @Test
    public void shouldHaveNullablePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat(this.modelObject.getNullable(null), is(Nullable.DEFAULT_VALUE));
        assertThat(this.modelObject.hasProperty(null, StandardDdlLexicon.NULLABLE), is(true));
        assertThat(this.modelObject.getProperty(null, StandardDdlLexicon.NULLABLE).getStringValue(),
                   is(RelationalConstants.Nullable.DEFAULT_VALUE.toString()));
    }

    @Test
    public void shouldHaveParentProcedureAfterConstruction() throws Exception {
        assertThat(this.modelObject.getProcedure(null), is(this.parent));
    }

    @Test
    public void shouldRemoveStatementOption() throws Exception {
        final String name = "statementoption";
        this.modelObject.addStatementOption(null, name, "blah");
        this.modelObject.removeStatementOption(null, name);
        assertThat(this.modelObject.getStatementOptions(null).length, is(0));
    }

    @Test
    public void shouldSetDatatypeLengthProperty() throws Exception {
        final long value = 10;
        this.modelObject.setLength(null, value);
        assertThat(this.modelObject.getLength(null), is(value));
        assertThat(this.modelObject.getProperty(null, StandardDdlLexicon.DATATYPE_LENGTH).getLongValue(), is(value));
    }

    @Test
    public void shouldSetDatatypeNameProperty() throws Exception {
        final String value = "datatypename";
        this.modelObject.setDatatypeName(null, value);
        assertThat(this.modelObject.getDatatypeName(null), is(value));
        assertThat(this.modelObject.getProperty(null, StandardDdlLexicon.DATATYPE_NAME).getStringValue(), is(value));
    }

    @Test
    public void shouldSetDatatypePrecisionProperty() throws Exception {
        final int value = 10;
        this.modelObject.setPrecision(null, value);
        assertThat(this.modelObject.getPrecision(null), is(value));
        assertThat(this.modelObject.getProperty(null, StandardDdlLexicon.DATATYPE_PRECISION).getLongValue(), is((long)value));
    }

    @Test
    public void shouldSetDatatypeScaleProperty() throws Exception {
        final int value = 10;
        this.modelObject.setScale(null, value);
        assertThat(this.modelObject.getScale(null), is(value));
        assertThat(this.modelObject.getProperty(null, StandardDdlLexicon.DATATYPE_SCALE).getLongValue(), is((long)value));
    }

    @Test
    public void shouldSetDefaultValueProperty() throws Exception {
        final String value = "defaultvalue";
        this.modelObject.setDefaultValue(null, value);
        assertThat(this.modelObject.getDefaultValue(null), is(value));
        assertThat(this.modelObject.getProperty(null, StandardDdlLexicon.DEFAULT_VALUE).getStringValue(), is(value));
    }

    @Test
    public void shouldSetDirectionProperty() throws Exception {
        final Direction value = Direction.IN_OUT;
        this.modelObject.setDirection(null, value);
        assertThat(this.modelObject.getDirection(null), is(value));
        assertThat(this.modelObject.getProperty(null, TeiidDdlLexicon.CreateProcedure.PARAMETER_TYPE).getStringValue(),
                   is(value.toString()));
    }

    @Test
    public void shouldSetNullableProperty() throws Exception {
        final Nullable value = Nullable.NO_NULLS;
        this.modelObject.setNullable(null, value);
        assertThat(this.modelObject.getNullable(null), is(value));
        assertThat(this.modelObject.getProperty(null, StandardDdlLexicon.NULLABLE).getStringValue(), is(value.toString()));
    }

}
