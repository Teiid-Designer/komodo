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
import static org.mockito.Mockito.mock;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.model.Function;
import org.komodo.relational.model.Function.Determinism;
import org.komodo.relational.model.Model;

@SuppressWarnings( {"javadoc", "nls"} )
public final class FunctionImplTest extends RelationalModelTest {

    private static final String NAME = "function";

    private Function function;

    @Before
    public void init() throws Exception {
        this.function = RelationalModelFactory.createPushdownFunction(null, _repo, mock(Model.class), NAME);
    }

    /////////////////////////////////////////////////
    // AGGREGATE Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultAggregateAfterConstruction() throws Exception {
        assertThat(this.function.isAggregate(null), is(Function.DEFAULT_AGGREGATE));
    }

    @Test
    public void shouldSetAggregate() throws Exception {
        final boolean value = !Function.DEFAULT_AGGREGATE;
        this.function.setAggregate(null, value);
        assertThat(this.function.isAggregate(null), is(value));
    }

    /////////////////////////////////////////////////
    // ALLOWS_DISTINCT Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultAllowsDistinctAfterConstruction() throws Exception {
        assertThat(this.function.isAllowsDistinct(null), is(Function.DEFAULT_ALLOWS_DISTINCT));
    }

    @Test
    public void shouldSetAllowsDistinct() throws Exception {
        final boolean value = !Function.DEFAULT_ALLOWS_DISTINCT;
        this.function.setAllowsDistinct(null, value);
        assertThat(this.function.isAllowsDistinct(null), is(value));
    }

    /////////////////////////////////////////////////
    // ALLOWS_ORDERBY Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultAllowsOrderByAfterConstruction() throws Exception {
        assertThat(this.function.isAllowsOrderBy(null), is(Function.DEFAULT_ALLOWS_ORDER_BY));
    }

    @Test
    public void shouldSetAllowsOrderBy() throws Exception {
        final boolean value = !Function.DEFAULT_ALLOWS_ORDER_BY;
        this.function.setAllowsOrderBy(null, value);
        assertThat(this.function.isAllowsOrderBy(null), is(value));
    }

    /////////////////////////////////////////////////
    // ANALYTIC Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultAnalyticAfterConstruction() throws Exception {
        assertThat(this.function.isAnalytic(null), is(Function.DEFAULT_ANALYTIC));
    }

    @Test
    public void shouldSetAnalytic() throws Exception {
        final boolean value = !Function.DEFAULT_ANALYTIC;
        this.function.setAnalytic(null, value);
        assertThat(this.function.isAnalytic(null), is(value));
    }

    /////////////////////////////////////////////////
    // DECOMPOSABLE Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultDecomposableAfterConstruction() throws Exception {
        assertThat(this.function.isDecomposable(null), is(Function.DEFAULT_DECOMPOSABLE));
    }

    @Test
    public void shouldSetDecomposable() throws Exception {
        final boolean value = !Function.DEFAULT_DECOMPOSABLE;
        this.function.setDecomposable(null, value);
        assertThat(this.function.isDecomposable(null), is(value));
    }

    /////////////////////////////////////////////////
    // DETERMINISM Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultDeterminismAfterConstruction() throws Exception {
        assertThat(this.function.getDeterminism(null), is(Determinism.DEFAULT_VALUE));
    }

    @Test
    public void shouldSetDeterminism() throws Exception {
        final Determinism value = Determinism.COMMAND_DETERMINISTIC;
        this.function.setDeterminism(null, value);
        assertThat(this.function.getDeterminism(null), is(value));
    }

    @Test
    public void shouldSetDeterminismToDefaultWhenNull() throws Exception {
        this.function.setDeterminism(null, Determinism.COMMAND_DETERMINISTIC);
        this.function.setDeterminism(null, null);
        assertThat(this.function.getDeterminism(null), is(Determinism.DEFAULT_VALUE));
    }

    /////////////////////////////////////////////////
    // NULL_ON_NULL Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultNullOnNullAfterConstruction() throws Exception {
        assertThat(this.function.isNullOnNull(null), is(Function.DEFAULT_NULL_ON_NULL));
    }

    @Test
    public void shouldSetNullOnNull() throws Exception {
        final boolean value = !Function.DEFAULT_NULL_ON_NULL;
        this.function.setNullOnNull(null, value);
        assertThat(this.function.isNullOnNull(null), is(value));
    }

    /////////////////////////////////////////////////
    // USES_DISTINCT_ROWS Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultUsesDistinctRowsAfterConstruction() throws Exception {
        assertThat(this.function.isUsesDistinctRows(null), is(Function.DEFAULT_USES_DISTINCT_ROWS));
    }

    @Test
    public void shouldSetUsesDistinctRows() throws Exception {
        final boolean value = !Function.DEFAULT_USES_DISTINCT_ROWS;
        this.function.setUsesDistinctRows(null, value);
        assertThat(this.function.isUsesDistinctRows(null), is(value));
    }

    /////////////////////////////////////////////////
    // VARARGS Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultVarArgsAfterConstruction() throws Exception {
        assertThat(this.function.isVarArgs(null), is(Function.DEFAULT_VARARGS));
    }

    @Test
    public void shouldSetVarArgsNull() throws Exception {
        final boolean value = !Function.DEFAULT_VARARGS;
        this.function.setVarArgs(null, value);
        assertThat(this.function.isVarArgs(null), is(value));
    }

}
