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
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.RelationalModelFactory;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class ColumnImplTest extends RelationalModelTest {

    private static final String NAME = "column";

    private Column modelObject;

    @Before
    public void init() throws Exception {
        this.modelObject = RelationalModelFactory.createColumn(null, _repo, null, NAME);
    }

    @Test
    public void shouldHaveCollationNamePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat(this.modelObject.getCollationName(null), is(RelationalConstants.DEFAULT_COLLATION_NAME));
        assertThat(this.modelObject.hasProperty(null, StandardDdlLexicon.COLLATION_NAME), is(false));
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
    public void shouldHaveDefaultAutoIncrementPropertyValueAfterConstruction() throws Exception {
        assertThat(this.modelObject.isAutoIncremented(null), is(RelationalConstants.DEFAULT_AUTO_INCREMENTED));
        assertThat(this.modelObject.hasProperty(null, TeiidDdlLexicon.CreateTable.AUTO_INCREMENT), is(false));
    }

    @Test
    public void shouldHaveDefaultValuePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat(this.modelObject.getDefaultValue(null), is(RelationalConstants.DEFAULT_VALUE));
        assertThat(this.modelObject.hasProperty(null, StandardDdlLexicon.DEFAULT_VALUE), is(false));
    }

    @Test
    public void shouldHaveNullablePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat(this.modelObject.hasProperty(null, StandardDdlLexicon.NULLABLE), is(true));
        assertThat(this.modelObject.getNullable(null), is(RelationalConstants.Nullable.DEFAULT_VALUE));
        assertThat(this.modelObject.getProperty(null, StandardDdlLexicon.NULLABLE).getStringValue(),
                   is(RelationalConstants.Nullable.DEFAULT_VALUE.toString()));
    }

    @Test
    public void shouldSetAutoIncrementedProperty() throws Exception {
        final boolean value = true;
        this.modelObject.setAutoIncremented(null, value);
        assertThat(this.modelObject.isAutoIncremented(null), is(value));
        assertThat(this.modelObject.getProperty(null, TeiidDdlLexicon.CreateTable.AUTO_INCREMENT).getBooleanValue(), is(value));
    }

    @Test
    public void shouldSetCollationNameProperty() throws Exception {
        final String value = "collationname";
        this.modelObject.setCollationName(null, value);
        assertThat(this.modelObject.getCollationName(null), is(value));
        assertThat(this.modelObject.getProperty(null, StandardDdlLexicon.COLLATION_NAME).getStringValue(), is(value));
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
    public void shouldSetNullableProperty() throws Exception {
        final Nullable value = Nullable.NO_NULLS;
        this.modelObject.setNullable(null, value);
        assertThat(this.modelObject.getNullable(null), is(value));
        assertThat(this.modelObject.getProperty(null, StandardDdlLexicon.NULLABLE).getStringValue(), is(value.toString()));
    }

}
