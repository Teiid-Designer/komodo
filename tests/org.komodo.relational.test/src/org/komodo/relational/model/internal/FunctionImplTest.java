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
import org.komodo.relational.model.RelationalObject.Filter;

@SuppressWarnings( { "javadoc", "nls" } )
public final class FunctionImplTest extends RelationalModelTest {

    private static final String NAME = "function";

    private Function function;

    @Before
    public void init() throws Exception {
        this.function = RelationalModelFactory.createPushdownFunction( this.uow, _repo, mock( Model.class ), NAME );
        commit();
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.function.getPropertyNames( this.uow );
        final String[] rawProps = this.function.getRawPropertyNames( this.uow );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.function.getPropertyNames( this.uow );
        final Filter[] filters = this.function.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotCountStatementOptionsAsChildren() throws Exception {
        this.function.setAnalytic( this.uow, true );
        this.function.setStatementOption( this.uow, "sledge", "hammer" );
        assertThat( this.function.getChildren( this.uow ).length, is( 0 ) );
    }

    /////////////////////////////////////////////////
    // AGGREGATE Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultAggregateAfterConstruction() throws Exception {
        assertThat( this.function.isAggregate( this.uow ), is( Function.DEFAULT_AGGREGATE ) );
    }

    @Test
    public void shouldSetAggregate() throws Exception {
        final boolean value = !Function.DEFAULT_AGGREGATE;
        this.function.setAggregate( this.uow, value );
        assertThat( this.function.isAggregate( this.uow ), is( value ) );
    }

    /////////////////////////////////////////////////
    // ALLOWS_DISTINCT Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultAllowsDistinctAfterConstruction() throws Exception {
        assertThat( this.function.isAllowsDistinct( this.uow ), is( Function.DEFAULT_ALLOWS_DISTINCT ) );
    }

    @Test
    public void shouldSetAllowsDistinct() throws Exception {
        final boolean value = !Function.DEFAULT_ALLOWS_DISTINCT;
        this.function.setAllowsDistinct( this.uow, value );
        assertThat( this.function.isAllowsDistinct( this.uow ), is( value ) );
    }

    /////////////////////////////////////////////////
    // ALLOWS_ORDERBY Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultAllowsOrderByAfterConstruction() throws Exception {
        assertThat( this.function.isAllowsOrderBy( this.uow ), is( Function.DEFAULT_ALLOWS_ORDER_BY ) );
    }

    @Test
    public void shouldSetAllowsOrderBy() throws Exception {
        final boolean value = !Function.DEFAULT_ALLOWS_ORDER_BY;
        this.function.setAllowsOrderBy( this.uow, value );
        assertThat( this.function.isAllowsOrderBy( this.uow ), is( value ) );
    }

    /////////////////////////////////////////////////
    // ANALYTIC Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultAnalyticAfterConstruction() throws Exception {
        assertThat( this.function.isAnalytic( this.uow ), is( Function.DEFAULT_ANALYTIC ) );
    }

    @Test
    public void shouldSetAnalytic() throws Exception {
        final boolean value = !Function.DEFAULT_ANALYTIC;
        this.function.setAnalytic( this.uow, value );
        assertThat( this.function.isAnalytic( this.uow ), is( value ) );
    }

    /////////////////////////////////////////////////
    // DECOMPOSABLE Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultDecomposableAfterConstruction() throws Exception {
        assertThat( this.function.isDecomposable( this.uow ), is( Function.DEFAULT_DECOMPOSABLE ) );
    }

    @Test
    public void shouldSetDecomposable() throws Exception {
        final boolean value = !Function.DEFAULT_DECOMPOSABLE;
        this.function.setDecomposable( this.uow, value );
        assertThat( this.function.isDecomposable( this.uow ), is( value ) );
    }

    /////////////////////////////////////////////////
    // DETERMINISM Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultDeterminismAfterConstruction() throws Exception {
        assertThat( this.function.getDeterminism( this.uow ), is( Determinism.DEFAULT_VALUE ) );
    }

    @Test
    public void shouldSetDeterminism() throws Exception {
        final Determinism value = Determinism.COMMAND_DETERMINISTIC;
        this.function.setDeterminism( this.uow, value );
        assertThat( this.function.getDeterminism( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetDeterminismToDefaultWhenNull() throws Exception {
        this.function.setDeterminism( this.uow, Determinism.COMMAND_DETERMINISTIC );
        this.function.setDeterminism( this.uow, null );
        assertThat( this.function.getDeterminism( this.uow ), is( Determinism.DEFAULT_VALUE ) );
    }

    /////////////////////////////////////////////////
    // NULL_ON_NULL Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultNullOnNullAfterConstruction() throws Exception {
        assertThat( this.function.isNullOnNull( this.uow ), is( Function.DEFAULT_NULL_ON_NULL ) );
    }

    @Test
    public void shouldSetNullOnNull() throws Exception {
        final boolean value = !Function.DEFAULT_NULL_ON_NULL;
        this.function.setNullOnNull( this.uow, value );
        assertThat( this.function.isNullOnNull( this.uow ), is( value ) );
    }

    /////////////////////////////////////////////////
    // USES_DISTINCT_ROWS Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultUsesDistinctRowsAfterConstruction() throws Exception {
        assertThat( this.function.isUsesDistinctRows( this.uow ), is( Function.DEFAULT_USES_DISTINCT_ROWS ) );
    }

    @Test
    public void shouldSetUsesDistinctRows() throws Exception {
        final boolean value = !Function.DEFAULT_USES_DISTINCT_ROWS;
        this.function.setUsesDistinctRows( this.uow, value );
        assertThat( this.function.isUsesDistinctRows( this.uow ), is( value ) );
    }

    /////////////////////////////////////////////////
    // VARARGS Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultVarArgsAfterConstruction() throws Exception {
        assertThat( this.function.isVarArgs( this.uow ), is( Function.DEFAULT_VARARGS ) );
    }

    @Test
    public void shouldSetVarArgsNull() throws Exception {
        final boolean value = !Function.DEFAULT_VARARGS;
        this.function.setVarArgs( this.uow, value );
        assertThat( this.function.isVarArgs( this.uow ), is( value ) );
    }

}
