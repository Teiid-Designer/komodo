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
import static org.hamcrest.core.IsCollectionContaining.hasItem;
import static org.hamcrest.core.IsCollectionContaining.hasItems;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import java.util.Arrays;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.model.AbstractProcedure;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.SchemaElement.SchemaElementType;
import org.komodo.relational.model.StatementOption;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.PropertyDescriptor;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.CreateProcedure;

@SuppressWarnings( { "javadoc", "nls" } )
public final class AbstractProcedureImplTest extends RelationalModelTest {

    private AbstractProcedure procedure;

    @Before
    public void init() throws Exception {
        final Model model = createModel();
        this.procedure = model.addVirtualProcedure( getTransaction(), "procedure" );
        commit();
    }

    @Test
    public void shouldAddParameter() throws Exception {
        final String name = "param";

        // setup
        this.procedure.addParameter( getTransaction(), name );

        // tests
        assertThat( this.procedure.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( this.procedure.getChildren( getTransaction() )[0].getName( getTransaction() ), is( name ) );
        assertThat( this.procedure.getChildren( getTransaction() )[0], is( instanceOf( Parameter.class ) ) );
    }

    @Test
    public void shouldAddStatementOption() throws Exception {
        // setup
        final String name = "statementoption";
        final String value = "statementvalue";
        final StatementOption statementOption = this.procedure.setStatementOption( getTransaction(), name, value );

        // tests
        assertThat( statementOption, is( notNullValue() ) );
        assertThat( statementOption.getName( getTransaction() ), is( name ) );
        assertThat( statementOption.getOption( getTransaction() ), is( value ) );
        assertThat( this.procedure.getChildren( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldAllowNullSchemaElementType() throws Exception {
        this.procedure.setSchemaElementType( getTransaction(), null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyParameterName() throws Exception {
        this.procedure.addParameter( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyStatementOptionName() throws Exception {
        this.procedure.setStatementOption( getTransaction(), StringConstants.EMPTY_STRING, "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullParameterName() throws Exception {
        this.procedure.addParameter( getTransaction(), null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullStatementOptionName() throws Exception {
        this.procedure.setStatementOption( getTransaction(), null, "blah" );
    }

    @Test( expected = KException.class )
    public void shouldFailAddingNullStatementOptionValue() throws Exception {
        this.procedure.setStatementOption( getTransaction(), "blah", null );
    }

    @Test( expected = KException.class )
    public void shouldFailASettingEmptyStatementOptionValueWhenNeverAdded() throws Exception {
        this.procedure.setStatementOption( getTransaction(), "blah", StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyDescriptionWhenNeverAdded() throws Exception {
        this.procedure.setDescription( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyNameInSourceWhenNeverAdded() throws Exception {
        this.procedure.setNameInSource( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingEmptyUuidWhenNeverAdded() throws Exception {
        this.procedure.setUuid( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullDescriptionWhenNeverAdded() throws Exception {
        this.procedure.setDescription( getTransaction(), null );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullNameInSourceWhenNeverAdded() throws Exception {
        this.procedure.setNameInSource( getTransaction(), null );
    }

    @Test( expected = KException.class )
    public void shouldFailSettingNullUuidWhenNeverAdded() throws Exception {
        this.procedure.setUuid( getTransaction(), null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyParameterName() throws Exception {
        this.procedure.removeParameter( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyStatementOptionName() throws Exception {
        this.procedure.removeStatementOption( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullParameterName() throws Exception {
        this.procedure.removeParameter( getTransaction(), null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullStatementOptionName() throws Exception {
        this.procedure.removeStatementOption( getTransaction(), null );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownParameter() throws Exception {
        this.procedure.removeParameter( getTransaction(), "unknown" );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownStatementOption() throws Exception {
        this.procedure.removeStatementOption( getTransaction(), "unknown" );
    }

    @Test
    public void shouldGetParameters() throws Exception {
        // setup
        final int numParams = 5;

        for ( int i = 0; i < numParams; ++i ) {
            this.procedure.addParameter( getTransaction(), "param" + i );
        }

        // tests
        assertThat( this.procedure.getParameters( getTransaction() ).length, is( numParams ) );
        assertThat( this.procedure.getChildrenOfType( getTransaction(), CreateProcedure.PARAMETER ).length, is( numParams ) );
    }

    @Test
    public void shouldGetStatementOptions() throws Exception {
        // setup
        final int numStatementOptions = 5;

        for ( int i = 0; i < numStatementOptions; ++i ) {
            this.procedure.setStatementOption( getTransaction(), "statementoption" + i, "statementvalue" + i );
        }

        // tests
        assertThat( this.procedure.getStatementOptions( getTransaction() ).length, is( numStatementOptions ) );
    }

    @Test
    public void shouldHaveCorrectChildTypes() {
        assertThat( Arrays.asList( this.procedure.getChildTypes() ), hasItem( Parameter.IDENTIFIER ) );
        assertThat( this.procedure.getChildTypes().length, is( 1 ) );
    }

    @Test
    public void shouldHaveDefaultUpdateCountAfterConstruction() throws Exception {
        assertThat( this.procedure.getUpdateCount( getTransaction() ), is( AbstractProcedure.DEFAULT_UPDATE_COUNT ) );
    }

    @Test
    public void shouldNotCountStatementOptionsAsChildren() throws Exception {
        // setup
        this.procedure.setNameInSource( getTransaction(), "elvis" );
        this.procedure.setStatementOption( getTransaction(), "sledge", "hammer" );

        // tests
        assertThat( this.procedure.getChildren( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveParametersAfterConstruction() throws Exception {
        assertThat( this.procedure.getParameters( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveParameter() throws Exception {
        // setup
        final String name = "param";
        this.procedure.addParameter( getTransaction(), name );
        this.procedure.removeParameter( getTransaction(), name );

        // tests
        assertThat( this.procedure.getParameters( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveStatementOption() throws Exception {
        // setup
        final String name = "statementoption";
        this.procedure.setStatementOption( getTransaction(), name, "blah" );
        this.procedure.removeStatementOption( getTransaction(), name );

        // tests
        assertThat( this.procedure.getStatementOptions( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldSetDescription() throws Exception {
        // setup
        final String value = "description";
        this.procedure.setDescription( getTransaction(), value );

        // tests
        assertThat( this.procedure.getDescription( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetNameInSource() throws Exception {
        // setup
        final String value = "nameInSource";
        this.procedure.setNameInSource( getTransaction(), value );

        // tests
        assertThat( this.procedure.getNameInSource( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetSchemaElementTypeProperty() throws Exception {
        // setup
        final SchemaElementType value = SchemaElementType.VIRTUAL;
        this.procedure.setSchemaElementType( getTransaction(), value );

        // tests
        assertThat( this.procedure.getSchemaElementType( getTransaction() ), is( value ) );
        assertThat( this.procedure.getProperty( getTransaction(), TeiidDdlLexicon.SchemaElement.TYPE ).getStringValue( getTransaction() ),
                    is( value.name() ) );
    }

    @Test
    public void shouldSetUpdateCount() throws Exception {
        // setup
        final int value = 10;
        this.procedure.setUpdateCount( getTransaction(), value );

        // tests
        assertThat( this.procedure.getUpdateCount( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetUuid() throws Exception {
        // setup
        final String value = "uuid";
        this.procedure.setUuid( getTransaction(), value );

        // tests
        assertThat( this.procedure.getUuid( getTransaction() ), is( value ) );
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
