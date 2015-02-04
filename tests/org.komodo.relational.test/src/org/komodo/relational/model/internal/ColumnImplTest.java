/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Table;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class ColumnImplTest extends RelationalModelTest {

    private static final String NAME = "column";

    private Column column;

    @Before
    public void init() throws Exception {
        this.column = RelationalModelFactory.createColumn(null, _repo, mock(Table.class), NAME);
    }

    @Test
    public void shouldFailConstructionIfNotColumn() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new ColumnImpl(null, _repo, _repo.komodoLibrary(null).getAbsolutePath());
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectName() throws Exception {
        assertThat(this.column.getName(null), is(NAME));
    }

    @Test
    public void shouldHaveDatatypeLengthPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat(this.column.getLength(null), is(RelationalConstants.DEFAULT_LENGTH));
        assertThat(this.column.hasProperty(null, StandardDdlLexicon.DATATYPE_LENGTH), is(false));
    }

    @Test
    public void shouldHaveDatatypeNamePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat(this.column.getDatatypeName(null), is(RelationalConstants.DEFAULT_DATATYPE_NAME));
        assertThat(this.column.hasProperty(null, StandardDdlLexicon.DATATYPE_NAME), is(false));
    }

    @Test
    public void shouldHaveDatatypePrecisionPropertyDefaultValueAfterConstruction() throws Exception {
        assertThat(this.column.getPrecision(null), is(RelationalConstants.DEFAULT_PRECISION));
        assertThat(this.column.hasProperty(null, StandardDdlLexicon.DATATYPE_PRECISION), is(false));
    }

    @Test
    public void shouldHaveDatatypeScalePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat(this.column.getScale(null), is(RelationalConstants.DEFAULT_SCALE));
        assertThat(this.column.hasProperty(null, StandardDdlLexicon.DATATYPE_SCALE), is(false));
    }

    @Test
    public void shouldHaveDefaultAutoIncrementPropertyValueAfterConstruction() throws Exception {
        assertThat(this.column.isAutoIncremented(null), is(RelationalConstants.DEFAULT_AUTO_INCREMENTED));
        assertThat(this.column.hasProperty(null, TeiidDdlLexicon.CreateTable.AUTO_INCREMENT), is(false));
    }

    @Test
    public void shouldHaveNullablePropertyDefaultValueAfterConstruction() throws Exception {
        assertThat(this.column.hasProperty(null, StandardDdlLexicon.NULLABLE), is(true));
        assertThat(this.column.getNullable(null), is(RelationalConstants.Nullable.DEFAULT_VALUE));
        assertThat(this.column.getProperty(null, StandardDdlLexicon.NULLABLE).getStringValue(null),
                   is(RelationalConstants.Nullable.DEFAULT_VALUE.toString()));
    }

    @Test
    public void shouldNotHaveCollationNameAfterConstruction() throws Exception {
        assertThat(this.column.hasProperty(null, StandardDdlLexicon.COLLATION_NAME), is(false));
        assertThat(this.column.getCollationName(null), is(nullValue()));
    }

    @Test
    public void shouldNotHaveDefaultValueAfterConstruction() throws Exception {
        assertThat(this.column.getDefaultValue(null), is(nullValue()));
        assertThat(this.column.hasProperty(null, StandardDdlLexicon.DEFAULT_VALUE), is(false));
    }

    @Test
    public void shouldRemoveCollationNameWithEmptyString() throws Exception {
        this.column.setCollationName(null, "collationName");
        this.column.setCollationName(null, StringConstants.EMPTY_STRING);
        assertThat(this.column.getCollationName(null), is(nullValue()));
        assertThat(this.column.hasProperty(null, StandardDdlLexicon.COLLATION_NAME), is(false));
    }

    @Test
    public void shouldRemoveCollationNameWithNull() throws Exception {
        this.column.setCollationName(null, "collationName");
        this.column.setCollationName(null, null);
        assertThat(this.column.getCollationName(null), is(nullValue()));
        assertThat(this.column.hasProperty(null, StandardDdlLexicon.COLLATION_NAME), is(false));
    }

    @Test
    public void shouldRemoveDefaultValueWithEmptyString() throws Exception {
        this.column.setDefaultValue(null, "defaultValue");
        this.column.setDefaultValue(null, StringConstants.EMPTY_STRING);
        assertThat(this.column.getDefaultValue(null), is(nullValue()));
        assertThat(this.column.hasProperty(null, StandardDdlLexicon.DEFAULT_VALUE), is(false));
    }

    @Test
    public void shouldRemoveDefaultValueWithNull() throws Exception {
        this.column.setDefaultValue(null, "defaultValue");
        this.column.setDefaultValue(null, null);
        assertThat(this.column.getDefaultValue(null), is(nullValue()));
        assertThat(this.column.hasProperty(null, StandardDdlLexicon.DEFAULT_VALUE), is(false));
    }

    @Test
    public void shouldSetAutoIncrementedProperty() throws Exception {
        final boolean value = true;
        this.column.setAutoIncremented(null, value);
        assertThat(this.column.isAutoIncremented(null), is(value));
        assertThat(this.column.getProperty(null, TeiidDdlLexicon.CreateTable.AUTO_INCREMENT).getBooleanValue(null), is(value));
    }

    @Test
    public void shouldSetCollationNameProperty() throws Exception {
        final String value = "collationname";
        this.column.setCollationName(null, value);
        assertThat(this.column.getCollationName(null), is(value));
        assertThat(this.column.getProperty(null, StandardDdlLexicon.COLLATION_NAME).getStringValue(null), is(value));
    }

    @Test
    public void shouldSetDatatypeLengthProperty() throws Exception {
        final long value = 10;
        this.column.setLength(null, value);
        assertThat(this.column.getLength(null), is(value));
        assertThat(this.column.getProperty(null, StandardDdlLexicon.DATATYPE_LENGTH).getLongValue(null), is(value));
    }

    @Test
    public void shouldSetDatatypeNameProperty() throws Exception {
        final String value = "datatypename";
        this.column.setDatatypeName(null, value);
        assertThat(this.column.getDatatypeName(null), is(value));
        assertThat(this.column.getProperty(null, StandardDdlLexicon.DATATYPE_NAME).getStringValue(null), is(value));
    }

    @Test
    public void shouldSetDatatypePrecisionProperty() throws Exception {
        final int value = 10;
        this.column.setPrecision(null, value);
        assertThat(this.column.getPrecision(null), is(value));
        assertThat(this.column.getProperty(null, StandardDdlLexicon.DATATYPE_PRECISION).getLongValue(null), is((long)value));
    }

    @Test
    public void shouldSetDatatypeScaleProperty() throws Exception {
        final int value = 10;
        this.column.setScale(null, value);
        assertThat(this.column.getScale(null), is(value));
        assertThat(this.column.getProperty(null, StandardDdlLexicon.DATATYPE_SCALE).getLongValue(null), is((long)value));
    }

    @Test
    public void shouldSetDefaultValueProperty() throws Exception {
        final String value = "defaultvalue";
        this.column.setDefaultValue(null, value);
        assertThat(this.column.getDefaultValue(null), is(value));
        assertThat(this.column.getProperty(null, StandardDdlLexicon.DEFAULT_VALUE).getStringValue(null), is(value));
    }

    @Test
    public void shouldSetNullableProperty() throws Exception {
        final Nullable value = Nullable.NO_NULLS;
        this.column.setNullable(null, value);
        assertThat(this.column.getNullable(null), is(value));
        assertThat(this.column.getProperty(null, StandardDdlLexicon.NULLABLE).getStringValue(null), is(value.toString()));
    }

}
