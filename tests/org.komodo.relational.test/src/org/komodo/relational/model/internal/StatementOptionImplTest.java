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
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.model.StatementOption;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class StatementOptionImplTest extends RelationalModelTest {

    private static final String NAME = "statementoption";

    private StatementOption modelObject;

    @Before
    public void init() throws Exception {
        this.modelObject = RelationalModelFactory.createStatementOption(null, _repo, null, NAME, "initialValue");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotAllowEmptyOptionValueProperty() throws Exception {
        this.modelObject.setOption(null, "");
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotAllowNullOptionValueProperty() throws Exception {
        this.modelObject.setOption(null, null);
    }

    @Test
    public void shouldSetOptionValueProperty() throws Exception {
        final String value = "optionvalue";
        this.modelObject.setOption(null, value);
        assertThat(this.modelObject.getOption(null), is(value));
        assertThat(this.modelObject.getProperty(null, StandardDdlLexicon.VALUE).getStringValue(), is(value));
    }

}
