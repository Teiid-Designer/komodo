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
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.OptionContainer;
import org.komodo.relational.model.StatementOption;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class StatementOptionImplTest extends RelationalModelTest {

    private static final String NAME = "statementoption";

    private StatementOption option;

    @Before
    public void init() throws Exception {
        this.option = RelationalModelFactory.createStatementOption(null, _repo, mock(OptionContainer.class), NAME, "initialValue");
    }

    @Test
    public void shouldFailConstructionIfNotStatementOption() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new StatementOptionImpl(null, _repo, _repo.komodoLibrary(null).getAbsolutePath());
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectDescriptor() throws Exception {
        assertThat(this.option.hasDescriptor(null, StandardDdlLexicon.TYPE_STATEMENT_OPTION), is(true));
    }

    @Test
    public void shouldHaveCorrectName() throws Exception {
        assertThat(this.option.getName(null), is(NAME));
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotAllowEmptyOptionValueProperty() throws Exception {
        this.option.setOption(null, StringConstants.EMPTY_STRING);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotAllowNullOptionValueProperty() throws Exception {
        this.option.setOption(null, null);
    }

    @Test
    public void shouldSetOptionValueProperty() throws Exception {
        final String value = "optionvalue";
        this.option.setOption(null, value);
        assertThat(this.option.getOption(null), is(value));
        assertThat(this.option.getProperty(null, StandardDdlLexicon.VALUE).getStringValue(null), is(value));
    }

}
