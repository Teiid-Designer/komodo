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
import static org.hamcrest.core.IsNull.nullValue;
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
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.SchemaElement.SchemaElementType;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.utils.StringUtils;

@SuppressWarnings( { "javadoc", "nls" } )
public final class StoredProcedureImplTest extends RelationalModelTest {

    private StoredProcedure procedure;

    @Before
    public void init() throws Exception {
        final Model model = createModel();
        this.procedure = model.addStoredProcedure( getTransaction(), "procedure" );
        commit();
    }

    @Test
    public void shouldFailConstructionIfNotStoredProcedure() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new StoredProcedureImpl( getTransaction(), _repo, _repo.komodoLibrary( getTransaction() ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test( expected = KException.class )
    public void shouldFailRemovingResultSetIfOneDoesNotExist() throws Exception {
        this.procedure.removeResultSet( getTransaction() );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNativeQueryWithEmptyValueWhenItWasNeverAdded() throws Exception {
        this.procedure.setNativeQuery( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullNativeQueryWhenNeverAdded() throws Exception {
        this.procedure.setNativeQuery( getTransaction(), null );
    }

    @Test
    public void shouldGetOnlyResultSetWhenGettingChildren() throws Exception {
        final TabularResultSet resultSet = this.procedure.setResultSet( getTransaction(), TabularResultSet.class );
        assertThat( this.procedure.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( this.procedure.getChildren( getTransaction() )[0], is( ( KomodoObject )resultSet ) );
    }

    @Test
    public void shouldGetChildren() throws Exception {
        this.procedure.addParameter( getTransaction(), "param" );
        this.procedure.setResultSet( getTransaction(), DataTypeResultSet.class );
        assertThat( this.procedure.getChildren( getTransaction() ).length, is( 2 ) );
    }

    @Test
    public void shouldHaveCorrectSchemaElementType() throws Exception {
        assertThat( this.procedure.getSchemaElementType( getTransaction() ), is( SchemaElementType.FOREIGN ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.procedure.getTypeIdentifier( getTransaction() ), is(KomodoType.STORED_PROCEDURE));
    }

    @Test
    public void shouldHaveDefaultNonPreparedAfterConstruction() throws Exception {
        assertThat( this.procedure.isNonPrepared( getTransaction() ), is( StoredProcedure.DEFAULT_NON_PREPARED ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.procedure.getPropertyNames( getTransaction() );
        final String[] rawProps = this.procedure.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.procedure.getPropertyNames( getTransaction() );
        final Filter[] filters = this.procedure.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotCountStatementOptionsAsChildren() throws Exception {
        this.procedure.setNativeQuery( getTransaction(), "blah" );
        this.procedure.setStatementOption( getTransaction(), "sledge", "hammer" );
        assertThat( this.procedure.getChildren( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveResultSetAfterConstruction() throws Exception {
        assertThat( this.procedure.getResultSet( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveResultSet() throws Exception {
        this.procedure.setResultSet( getTransaction(), TabularResultSet.class );
        this.procedure.removeResultSet( getTransaction() );
        assertThat( this.procedure.getResultSet( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldSetDataTypeResultSet() throws Exception {
        assertThat( this.procedure.setResultSet( getTransaction(), DataTypeResultSet.class ), is( notNullValue() ) );
        assertThat( this.procedure.getResultSet( getTransaction() ), is( instanceOf( DataTypeResultSet.class ) ) );
    }

    @Test
    public void shouldSetNativeQuery() throws Exception {
        final String value = "nativeQuery";
        this.procedure.setNativeQuery( getTransaction(), value );
        assertThat( this.procedure.getNativeQuery( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetNonPrepared() throws Exception {
        final boolean value = !StoredProcedure.DEFAULT_NON_PREPARED;
        this.procedure.setNonPrepared( getTransaction(), value );
        assertThat( this.procedure.isNonPrepared( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetTabularResultSet() throws Exception {
        assertThat( this.procedure.setResultSet( getTransaction(), TabularResultSet.class ), is( notNullValue() ) );
        assertThat( this.procedure.getResultSet( getTransaction() ), is( instanceOf( TabularResultSet.class ) ) );
    }

    @Test
    public void shouldIncludeCustomOptionsWithPropertyDescriptors() throws Exception {
        final String customName = "blah";
        this.procedure.setStatementOption( getTransaction(), customName, "elvis" );

        final PropertyDescriptor[] propDescriptors = this.procedure.getPropertyDescriptors( getTransaction() );
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
        this.procedure.setStatementOption( getTransaction(), custom, "sledge" );
        boolean customFound = false;

        final String standard = this.procedure.getStandardOptions().keySet().iterator().next();
        this.procedure.setStatementOption( getTransaction(), standard, "hammer" );
        boolean standardFound = false;

        for ( final String prop : this.procedure.getPropertyNames( getTransaction() ) ) {
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
    public void shouldIncludeStandardOptionDefaultValuesWithPropertyDescriptors() throws Exception {
        final Map< String, String > options = this.procedure.getStandardOptions();
        final PropertyDescriptor[] propDescriptors = this.procedure.getPropertyDescriptors( getTransaction() );

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
    public void shouldIncludeStandardOptionsWithPrimaryTypePropertyDescriptors() throws Exception {
        final Set< String > optionNames = this.procedure.getStandardOptions().keySet();
        final PropertyDescriptor[] propDescriptors = this.procedure.getPrimaryType( getTransaction() ).getPropertyDescriptors( getTransaction() );

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
        final Set< String > optionNames = this.procedure.getStandardOptions().keySet();
        final PropertyDescriptor[] propDescriptors = this.procedure.getPropertyDescriptors( getTransaction() );

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
        this.procedure.setStatementOption( getTransaction(), sledge, "hammer" );

        final String elvis = "elvis";
        this.procedure.setStatementOption( getTransaction(), elvis, "presley" );

        assertThat( this.procedure.getCustomOptions( getTransaction() ).length, is( 2 ) );
        assertThat( Arrays.asList( this.procedure.getStatementOptionNames( getTransaction() ) ), hasItems( sledge, elvis ) );
    }

    @Test
    public void shouldObtainPropertyDescriptorOfCustomOption() throws Exception {
        final String custom = "sledge";
        this.procedure.setStatementOption( getTransaction(), custom, "hammer" );

        assertThat( this.procedure.getPropertyDescriptor( getTransaction(), custom ), is( notNullValue() ) );
        assertThat( this.procedure.getPropertyDescriptor( getTransaction(), custom ).getName(), is( custom ) );
    }

    @Test
    public void shouldObtainPropertyDescriptorOfStandardOption() throws Exception {
        final String standard = this.procedure.getStandardOptions().keySet().iterator().next();
        this.procedure.setStatementOption( getTransaction(), standard, "blah" );

        assertThat( this.procedure.getPropertyDescriptor( getTransaction(), standard ), is( notNullValue() ) );
        assertThat( this.procedure.getPropertyDescriptor( getTransaction(), standard ).getName(), is( standard ) );
    }

    @Test
    public void shouldObtainStatementOptionNames() throws Exception {
        final String custom = "blah";
        this.procedure.setStatementOption( getTransaction(), custom, "sledge" );

        final String standard = this.procedure.getStandardOptions().keySet().iterator().next();
        this.procedure.setStatementOption( getTransaction(), standard, "hammer" );

        assertThat( this.procedure.getStatementOptionNames( getTransaction() ).length, is( 2 ) );
        assertThat( Arrays.asList( this.procedure.getStatementOptionNames( getTransaction() ) ), hasItems( custom, standard ) );
    }

    @Test
    public void shouldRemoveStandardOptionAsIfProperty() throws Exception {
        final String option = this.procedure.getStandardOptions().keySet().iterator().next();
        final String value = "newValue";
        this.procedure.setProperty( getTransaction(), option, value ); // add
        this.procedure.setProperty( getTransaction(), option, (Object)null ); // remove
        assertThat( this.procedure.hasProperty( getTransaction(), option ), is( false ) );
        assertThat( this.procedure.hasChild( getTransaction(), option ), is( false ) );
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "blah";
        this.procedure.rename( getTransaction(), newName );
        assertThat( this.procedure.getName( getTransaction() ), is( newName ) );
    }

    @Test
    public void shouldSetCustomOptionAsIfProperty() throws Exception {
        final String option = "blah";
        this.procedure.setStatementOption( getTransaction(), option, "initialValue" );

        final String value = "newValue";
        this.procedure.setProperty( getTransaction(), option, value );

        assertThat( this.procedure.hasProperty( getTransaction(), option ), is( true ) );
        assertThat( this.procedure.getProperty( getTransaction(), option ), is( instanceOf( StatementOption.class ) ) );
        assertThat( this.procedure.getStatementOptions( getTransaction() ).length, is( 1 ) );
        assertThat( this.procedure.isCustomOption( getTransaction(), option ), is( true ) );

        final StatementOption statementOption = this.procedure.getStatementOptions( getTransaction() )[0];
        assertThat( statementOption.getName( getTransaction() ), is( option ) );
        assertThat( statementOption.getValue( getTransaction() ), is( ( Object )value ) );
    }

    @Test
    public void shouldSetStandardOptionAsIfProperty() throws Exception {
        final String option = this.procedure.getStandardOptions().keySet().iterator().next();
        this.procedure.setStatementOption( getTransaction(), option, "initialValue" );

        final String value = "newValue";
        this.procedure.setProperty( getTransaction(), option, value );

        assertThat( this.procedure.hasProperty( getTransaction(), option ), is( true ) );
        assertThat( this.procedure.getProperty( getTransaction(), option ), is( instanceOf( StatementOption.class ) ) );
        assertThat( this.procedure.isCustomOption( getTransaction(), option ), is( false ) );
        assertThat( this.procedure.getStatementOptions( getTransaction() ).length, is( 1 ) );

        final StatementOption statementOption = this.procedure.getStatementOptions( getTransaction() )[0];
        assertThat( statementOption.getName( getTransaction() ), is( option ) );
        assertThat( statementOption.getValue( getTransaction() ), is( ( Object )value ) );
    }

}
