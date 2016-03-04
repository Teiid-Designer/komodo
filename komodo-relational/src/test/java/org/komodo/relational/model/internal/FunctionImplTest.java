/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.relational.model.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsCollectionContaining.hasItems;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import java.util.Arrays;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.model.Function;
import org.komodo.relational.model.Function.Determinism;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.StatementOption;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.utils.StringUtils;

@SuppressWarnings( { "javadoc", "nls" } )
public final class FunctionImplTest extends RelationalModelTest {

    private static final String NAME = "function";

    private Function function;

    @Before
    public void init() throws Exception {
        final Model model = createModel();
        this.function = model.addPushdownFunction( getTransaction(), NAME );
        commit();
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.function.getPropertyNames( getTransaction() );
        final String[] rawProps = this.function.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldIncludeStandardOptionDefaultValuesWithPropertyDescriptors() throws Exception {
        final Map< String, String > options = this.function.getStandardOptions();
        final PropertyDescriptor[] propDescriptors = this.function.getPropertyDescriptors( getTransaction() );

        for ( final Entry< String, String > entry : options.entrySet() ) {
            for ( final PropertyDescriptor descriptor : propDescriptors ) {
                if ( entry.getKey().equals( descriptor.getName() ) ) {
                    final String value = entry.getValue();
                    final Object[] defaultValues = descriptor.getDefaultValues();

                    if ( StringUtils.isBlank( value ) ) {
                        assertThat( defaultValues.length, is( 0 ) );
                    } else {
                        assertThat( defaultValues.length, is( 1 ) );
                        assertThat( value, is( defaultValues[0] ) );
                    }
                }
            }
        }
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.function.getPropertyNames( getTransaction() );
        final Filter[] filters = this.function.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotCountStatementOptionsAsChildren() throws Exception {
        this.function.setAnalytic( getTransaction(), true );
        this.function.setStatementOption( getTransaction(), "sledge", "hammer" );
        assertThat( this.function.getChildren( getTransaction() ).length, is( 0 ) );
    }

    /////////////////////////////////////////////////
    // AGGREGATE Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultAggregateAfterConstruction() throws Exception {
        assertThat( this.function.isAggregate( getTransaction() ), is( Function.DEFAULT_AGGREGATE ) );
    }

    @Test
    public void shouldSetAggregate() throws Exception {
        final boolean value = !Function.DEFAULT_AGGREGATE;
        this.function.setAggregate( getTransaction(), value );
        assertThat( this.function.isAggregate( getTransaction() ), is( value ) );
    }

    /////////////////////////////////////////////////
    // ALLOWS_DISTINCT Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultAllowsDistinctAfterConstruction() throws Exception {
        assertThat( this.function.isAllowsDistinct( getTransaction() ), is( Function.DEFAULT_ALLOWS_DISTINCT ) );
    }

    @Test
    public void shouldSetAllowsDistinct() throws Exception {
        final boolean value = !Function.DEFAULT_ALLOWS_DISTINCT;
        this.function.setAllowsDistinct( getTransaction(), value );
        assertThat( this.function.isAllowsDistinct( getTransaction() ), is( value ) );
    }

    /////////////////////////////////////////////////
    // ALLOWS_ORDERBY Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultAllowsOrderByAfterConstruction() throws Exception {
        assertThat( this.function.isAllowsOrderBy( getTransaction() ), is( Function.DEFAULT_ALLOWS_ORDER_BY ) );
    }

    @Test
    public void shouldSetAllowsOrderBy() throws Exception {
        final boolean value = !Function.DEFAULT_ALLOWS_ORDER_BY;
        this.function.setAllowsOrderBy( getTransaction(), value );
        assertThat( this.function.isAllowsOrderBy( getTransaction() ), is( value ) );
    }

    /////////////////////////////////////////////////
    // ANALYTIC Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultAnalyticAfterConstruction() throws Exception {
        assertThat( this.function.isAnalytic( getTransaction() ), is( Function.DEFAULT_ANALYTIC ) );
    }

    @Test
    public void shouldSetAnalytic() throws Exception {
        final boolean value = !Function.DEFAULT_ANALYTIC;
        this.function.setAnalytic( getTransaction(), value );
        assertThat( this.function.isAnalytic( getTransaction() ), is( value ) );
    }

    /////////////////////////////////////////////////
    // DECOMPOSABLE Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultDecomposableAfterConstruction() throws Exception {
        assertThat( this.function.isDecomposable( getTransaction() ), is( Function.DEFAULT_DECOMPOSABLE ) );
    }

    @Test
    public void shouldSetDecomposable() throws Exception {
        final boolean value = !Function.DEFAULT_DECOMPOSABLE;
        this.function.setDecomposable( getTransaction(), value );
        assertThat( this.function.isDecomposable( getTransaction() ), is( value ) );
    }

    /////////////////////////////////////////////////
    // DETERMINISM Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultDeterminismAfterConstruction() throws Exception {
        assertThat( this.function.getDeterminism( getTransaction() ), is( Determinism.DEFAULT_VALUE ) );
    }

    @Test
    public void shouldSetDeterminism() throws Exception {
        final Determinism value = Determinism.COMMAND_DETERMINISTIC;
        this.function.setDeterminism( getTransaction(), value );
        assertThat( this.function.getDeterminism( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetDeterminismToDefaultWhenNull() throws Exception {
        this.function.setDeterminism( getTransaction(), Determinism.COMMAND_DETERMINISTIC );
        this.function.setDeterminism( getTransaction(), null );
        assertThat( this.function.getDeterminism( getTransaction() ), is( Determinism.DEFAULT_VALUE ) );
    }

    /////////////////////////////////////////////////
    // NULL_ON_NULL Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultNullOnNullAfterConstruction() throws Exception {
        assertThat( this.function.isNullOnNull( getTransaction() ), is( Function.DEFAULT_NULL_ON_NULL ) );
    }

    @Test
    public void shouldSetNullOnNull() throws Exception {
        final boolean value = !Function.DEFAULT_NULL_ON_NULL;
        this.function.setNullOnNull( getTransaction(), value );
        assertThat( this.function.isNullOnNull( getTransaction() ), is( value ) );
    }

    /////////////////////////////////////////////////
    // USES_DISTINCT_ROWS Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultUsesDistinctRowsAfterConstruction() throws Exception {
        assertThat( this.function.isUsesDistinctRows( getTransaction() ), is( Function.DEFAULT_USES_DISTINCT_ROWS ) );
    }

    @Test
    public void shouldSetUsesDistinctRows() throws Exception {
        final boolean value = !Function.DEFAULT_USES_DISTINCT_ROWS;
        this.function.setUsesDistinctRows( getTransaction(), value );
        assertThat( this.function.isUsesDistinctRows( getTransaction() ), is( value ) );
    }

    /////////////////////////////////////////////////
    // VARARGS Option tests
    /////////////////////////////////////////////////

    @Test
    public void shouldHaveDefaultVarArgsAfterConstruction() throws Exception {
        assertThat( this.function.isVarArgs( getTransaction() ), is( Function.DEFAULT_VARARGS ) );
    }

    @Test
    public void shouldSetVarArgsNull() throws Exception {
        final boolean value = !Function.DEFAULT_VARARGS;
        this.function.setVarArgs( getTransaction(), value );
        assertThat( this.function.isVarArgs( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldIncludeCustomOptionsWithPropertyDescriptors() throws Exception {
        final String customName = "blah";
        this.function.setStatementOption( getTransaction(), customName, "elvis" );

        final PropertyDescriptor[] propDescriptors = this.function.getPropertyDescriptors( getTransaction() );
        boolean found = false;

        for ( final PropertyDescriptor descriptor : propDescriptors ) {
            if ( customName.equals( descriptor.getName() ) ) {
                found = true;
                break;
            }
        }

        if ( !found ) {
            fail( "Custom option '" + customName + "'was not included in the property descriptors" );
        }
    }

    @Test
    public void shouldIncludeOptionsWithPropertyNames() throws Exception {
        final String custom = "blah";
        this.function.setStatementOption( getTransaction(), custom, "sledge" );
        boolean customFound = false;

        final String standard = this.function.getStandardOptions().keySet().iterator().next();
        this.function.setStatementOption( getTransaction(), standard, "hammer" );
        boolean standardFound = false;

        for ( final String prop : this.function.getPropertyNames( getTransaction() ) ) {
            if ( custom.equals( prop ) ) {
                if ( customFound ) {
                    fail( "Custom option included multiple times in property names" );
                }

                customFound = true;
            } else if ( standard.equals( prop ) ) {
                if ( standardFound ) {
                    fail( "Standard option included multiple times in property names" );
                }

                standardFound = true;
            }

            if ( customFound && standardFound ) {
                break;
            }
        }

        if ( !customFound ) {
            fail( "Custom option not included in property names" );
        }

        if ( !standardFound ) {
            fail( "Standard option not included in property names" );
        }
    }

    @Test
    public void shouldIncludeStandardOptionsWithPrimaryTypePropertyDescriptors() throws Exception {
        final Set< String > optionNames = this.function.getStandardOptions().keySet();
        final PropertyDescriptor[] propDescriptors = this.function.getPrimaryType( getTransaction() ).getPropertyDescriptors( getTransaction() );

        for ( final String optionName : optionNames ) {
            boolean found = false;

            for ( final PropertyDescriptor descriptor : propDescriptors ) {
                if ( optionName.equals( descriptor.getName() ) ) {
                    found = true;
                    break;
                }
            }

            if ( !found ) {
                fail( "Option '" + optionName + "'was not included in the primary type property descriptors" );
            }
        }
    }

    @Test
    public void shouldIncludeStandardOptionsWithPropertyDescriptors() throws Exception {
        final Set< String > optionNames = this.function.getStandardOptions().keySet();
        final PropertyDescriptor[] propDescriptors = this.function.getPropertyDescriptors( getTransaction() );

        for ( final String optionName : optionNames ) {
            boolean found = false;

            for ( final PropertyDescriptor descriptor : propDescriptors ) {
                if ( optionName.equals( descriptor.getName() ) ) {
                    found = true;
                    break;
                }
            }

            if ( !found ) {
                fail( "Option '" + optionName + "'was not included in the property descriptors" );
            }
        }
    }

    @Test
    public void shouldObtainCustomOptions() throws Exception {
        final String sledge = "sledge";
        this.function.setStatementOption( getTransaction(), sledge, "hammer" );

        final String elvis = "elvis";
        this.function.setStatementOption( getTransaction(), elvis, "presley" );

        assertThat( this.function.getCustomOptions( getTransaction() ).length, is( 2 ) );
        assertThat( Arrays.asList( this.function.getStatementOptionNames( getTransaction() ) ), hasItems( sledge, elvis ) );
    }

    @Test
    public void shouldObtainPropertyDescriptorOfCustomOption() throws Exception {
        final String custom = "sledge";
        this.function.setStatementOption( getTransaction(), custom, "hammer" );

        assertThat( this.function.getPropertyDescriptor( getTransaction(), custom ), is( notNullValue() ) );
        assertThat( this.function.getPropertyDescriptor( getTransaction(), custom ).getName(), is( custom ) );
    }

    @Test
    public void shouldObtainPropertyDescriptorOfStandardOption() throws Exception {
        final String standard = this.function.getStandardOptions().keySet().iterator().next();
        this.function.setStatementOption( getTransaction(), standard, "blah" );

        assertThat( this.function.getPropertyDescriptor( getTransaction(), standard ), is( notNullValue() ) );
        assertThat( this.function.getPropertyDescriptor( getTransaction(), standard ).getName(), is( standard ) );
    }

    @Test
    public void shouldObtainStatementOptionNames() throws Exception {
        final String custom = "blah";
        this.function.setStatementOption( getTransaction(), custom, "sledge" );

        final String standard = this.function.getStandardOptions().keySet().iterator().next();
        this.function.setStatementOption( getTransaction(), standard, "hammer" );

        assertThat( this.function.getStatementOptionNames( getTransaction() ).length, is( 2 ) );
        assertThat( Arrays.asList( this.function.getStatementOptionNames( getTransaction() ) ), hasItems( custom, standard ) );
    }

    @Test
    public void shouldRemoveStandardOptionAsIfProperty() throws Exception {
        final String option = this.function.getStandardOptions().keySet().iterator().next();
        final String value = "newValue";
        this.function.setProperty( getTransaction(), option, value ); // add
        this.function.setProperty( getTransaction(), option, (Object)null ); // remove
        assertThat( this.function.hasProperty( getTransaction(), option ), is( false ) );
        assertThat( this.function.hasChild( getTransaction(), option ), is( false ) );
    }

    @Test
    public void shouldSetCustomOptionAsIfProperty() throws Exception {
        final String option = "blah";
        this.function.setStatementOption( getTransaction(), option, "initialValue" );

        final String value = "newValue";
        this.function.setProperty( getTransaction(), option, value );

        assertThat( this.function.hasProperty( getTransaction(), option ), is( true ) );
        assertThat( this.function.getProperty( getTransaction(), option ), is( instanceOf( StatementOption.class ) ) );
        assertThat( this.function.getStatementOptions( getTransaction() ).length, is( 1 ) );
        assertThat( this.function.isCustomOption( getTransaction(), option ), is( true ) );

        final StatementOption statementOption = this.function.getStatementOptions( getTransaction() )[0];
        assertThat( statementOption.getName( getTransaction() ), is( option ) );
        assertThat( statementOption.getValue( getTransaction() ), is( ( Object )value ) );
    }

    @Test
    public void shouldSetStandardOptionAsIfProperty() throws Exception {
        final String option = this.function.getStandardOptions().keySet().iterator().next();
        this.function.setStatementOption( getTransaction(), option, "initialValue" );

        final String value = "newValue";
        this.function.setProperty( getTransaction(), option, value );

        assertThat( this.function.hasProperty( getTransaction(), option ), is( true ) );
        assertThat( this.function.getProperty( getTransaction(), option ), is( instanceOf( StatementOption.class ) ) );
        assertThat( this.function.isCustomOption( getTransaction(), option ), is( false ) );
        assertThat( this.function.getStatementOptions( getTransaction() ).length, is( 1 ) );

        final StatementOption statementOption = this.function.getStatementOptions( getTransaction() )[0];
        assertThat( statementOption.getName( getTransaction() ), is( option ) );
        assertThat( statementOption.getValue( getTransaction() ), is( ( Object )value ) );
    }

}
