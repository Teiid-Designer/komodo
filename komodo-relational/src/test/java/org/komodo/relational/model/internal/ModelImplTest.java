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
import java.util.Properties;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Model.Type;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.UserDefinedFunction;
import org.komodo.relational.model.View;
import org.komodo.relational.model.VirtualProcedure;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.CreateProcedure;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.CreateTable;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ModelImplTest extends RelationalModelTest {

    private Model model;

    @Before
    public void init() throws Exception {
        this.model = createModel();
        commit();
    }

    @Test
    public void shouldAddPushdownFunction() throws Exception {
        final String name = "function";
        final PushdownFunction function = this.model.addPushdownFunction( getTransaction(), name );
        assertThat( function, is( notNullValue() ) );
        assertThat( function.getName( getTransaction() ), is( name ) );
        assertThat( this.model.getFunctions( getTransaction() ).length, is( 1 ) );
        assertThat( this.model.getChildren( getTransaction() )[0], is( instanceOf( PushdownFunction.class ) ) );

        assertThat( this.model.hasChild( getTransaction(), name ), is( true ) );
        assertThat( this.model.hasChild( getTransaction(), name, CreateProcedure.FUNCTION_STATEMENT ), is( true ) );
        assertThat( this.model.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.model.getChild( getTransaction(), name ), is( function ) );
        assertThat( this.model.getChild( getTransaction(), name, CreateProcedure.FUNCTION_STATEMENT ), is( function ) );
    }

    @Test
    public void shouldAddSource() throws Exception {
        final String name = "source";
        final ModelSource source = this.model.addSource( getTransaction(), name );
        assertThat( source, is( notNullValue() ) );
        assertThat( source.getName( getTransaction() ), is( name ) );
        assertThat( this.model.getSources( getTransaction() ).length, is( 1 ) );
        assertThat( this.model.getChildren( getTransaction() )[0], is( instanceOf( ModelSource.class ) ) );

        assertThat( this.model.hasChild( getTransaction(), name ), is( true ) );
        assertThat( this.model.hasChild( getTransaction(), name, VdbLexicon.Source.SOURCE ), is( true ) );
        assertThat( this.model.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.model.getChild( getTransaction(), name ), is( source ) );
        assertThat( this.model.getChild( getTransaction(), name, VdbLexicon.Source.SOURCE ), is( source ) );
    }

    @Test
    public void shouldAddStoredProcedure() throws Exception {
        final String name = "procedure";
        final StoredProcedure procedure = this.model.addStoredProcedure( getTransaction(), name );
        assertThat( procedure, is( notNullValue() ) );
        assertThat( procedure.getName( getTransaction() ), is( name ) );
        assertThat( this.model.getProcedures( getTransaction() ).length, is( 1 ) );
        assertThat( this.model.getChildren( getTransaction() )[0], is( instanceOf( StoredProcedure.class ) ) );

        assertThat( this.model.hasChild( getTransaction(), name ), is( true ) );
        assertThat( this.model.hasChild( getTransaction(), name, CreateProcedure.PROCEDURE_STATEMENT ), is( true ) );
        assertThat( this.model.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.model.getChild( getTransaction(), name ), is( procedure ) );
        assertThat( this.model.getChild( getTransaction(), name, CreateProcedure.PROCEDURE_STATEMENT ), is( procedure ) );
    }

    @Test
    public void shouldAddTable() throws Exception {
        final String name = "table";
        final Table table = this.model.addTable( getTransaction(), name );
        assertThat( table, is( notNullValue() ) );
        assertThat( table.getName( getTransaction() ), is( name ) );
        assertThat( this.model.getTables( getTransaction() ).length, is( 1 ) );
        assertThat( this.model.getChildren( getTransaction() )[0], is( instanceOf( Table.class ) ) );

        assertThat( this.model.hasChild( getTransaction(), name ), is( true ) );
        assertThat( this.model.hasChild( getTransaction(), name, CreateTable.TABLE_STATEMENT ), is( true ) );
        assertThat( this.model.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.model.getChild( getTransaction(), name ), is( table ) );
        assertThat( this.model.getChild( getTransaction(), name, CreateTable.TABLE_STATEMENT ), is( table ) );
    }

    @Test
    public void shouldAddUserDefinedFunction() throws Exception {
        final String name = "function";
        final UserDefinedFunction function = this.model.addUserDefinedFunction( getTransaction(), name );
        assertThat( function, is( notNullValue() ) );
        assertThat( function.getName( getTransaction() ), is( name ) );
        assertThat( this.model.getFunctions( getTransaction() ).length, is( 1 ) );
        assertThat( this.model.getChildren( getTransaction() )[0], is( instanceOf( UserDefinedFunction.class ) ) );

        assertThat( this.model.hasChild( getTransaction(), name ), is( true ) );
        assertThat( this.model.hasChild( getTransaction(), name, CreateProcedure.FUNCTION_STATEMENT ), is( true ) );
        assertThat( this.model.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.model.getChild( getTransaction(), name ), is( function ) );
        assertThat( this.model.getChild( getTransaction(), name, CreateProcedure.FUNCTION_STATEMENT ), is( function ) );
    }

    @Test
    public void shouldAddView() throws Exception {
        final String name = "view";
        final View view = this.model.addView( getTransaction(), name );
        assertThat( view, is( notNullValue() ) );
        assertThat( view.getName( getTransaction() ), is( name ) );
        assertThat( this.model.getViews( getTransaction() ).length, is( 1 ) );
        assertThat( this.model.getChildren( getTransaction() )[0], is( instanceOf( View.class ) ) );

        assertThat( this.model.hasChild( getTransaction(), name ), is( true ) );
        assertThat( this.model.hasChild( getTransaction(), name, CreateTable.VIEW_STATEMENT ), is( true ) );
        assertThat( this.model.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.model.getChild( getTransaction(), name ), is( view ) );
        assertThat( this.model.getChild( getTransaction(), name, CreateTable.VIEW_STATEMENT ), is( view ) );
    }

    @Test
    public void shouldAddVirtualProcedure() throws Exception {
        final String name = "procedure";
        final VirtualProcedure procedure = this.model.addVirtualProcedure( getTransaction(), name );
        assertThat( procedure, is( notNullValue() ) );
        assertThat( procedure.getName( getTransaction() ), is( name ) );
        assertThat( this.model.getProcedures( getTransaction() ).length, is( 1 ) );
        assertThat( this.model.getChildren( getTransaction() )[0], is( instanceOf( VirtualProcedure.class ) ) );

        assertThat( this.model.hasChild( getTransaction(), name ), is( true ) );
        assertThat( this.model.hasChild( getTransaction(), name, CreateProcedure.PROCEDURE_STATEMENT ), is( true ) );
        assertThat( this.model.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.model.getChild( getTransaction(), name ), is( procedure ) );
        assertThat( this.model.getChild( getTransaction(), name, CreateProcedure.PROCEDURE_STATEMENT ), is( procedure ) );
    }

    @Test
    public void shouldAllowEmptyDescriptionWhenRemoving() throws Exception {
        this.model.setDescription( getTransaction(), "blah" );
        this.model.setDescription( getTransaction(), StringConstants.EMPTY_STRING );
        assertThat( this.model.getDescription( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullDescriptionWhenRemoving() throws Exception {
        this.model.setDescription( getTransaction(), "blah" );
        this.model.setDescription( getTransaction(), null );
        assertThat( this.model.getDescription( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullModelTypeWhenSettingToDefaultValue() throws Exception {
        this.model.setModelType( getTransaction(), Type.VIRTUAL );
        this.model.setModelType( getTransaction(), null );
        assertThat( this.model.getModelType( getTransaction() ), is( Type.DEFAULT_VALUE ) );
    }

    @Test
    public void shouldClearMetadataTypeWhenClearingModelDefinition() throws Exception {
        this.model.setMetadataType( getTransaction(), "blah" );
        this.model.setModelDefinition( getTransaction(), "blah blah blah" );
        this.model.setModelDefinition( getTransaction(), EMPTY_STRING );
        assertThat( this.model.getMetadataType( getTransaction() ), is( EMPTY_STRING ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyPushdownFunctionName() throws Exception {
        this.model.addPushdownFunction( getTransaction(), EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptySourceName() throws Exception {
        this.model.addSource( getTransaction(), EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyStoredProcedureName() throws Exception {
        this.model.addStoredProcedure( getTransaction(), EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyTableName() throws Exception {
        this.model.addTable( getTransaction(), EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyUserDefinedFunctionName() throws Exception {
        this.model.addUserDefinedFunction( getTransaction(), EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyViewName() throws Exception {
        this.model.addView( getTransaction(), EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyVirtualProcedureName() throws Exception {
        this.model.addVirtualProcedure( getTransaction(), EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullPushdownFunctionName() throws Exception {
        this.model.addPushdownFunction( getTransaction(), null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullSourceName() throws Exception {
        this.model.addSource( getTransaction(), null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullStoredProcedureName() throws Exception {
        this.model.addStoredProcedure( getTransaction(), null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullTableName() throws Exception {
        this.model.addTable( getTransaction(), null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullUserDefinedFunctionName() throws Exception {
        this.model.addUserDefinedFunction( getTransaction(), null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullViewName() throws Exception {
        this.model.addView( getTransaction(), null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullVirtualProcedureName() throws Exception {
        this.model.addVirtualProcedure( getTransaction(), null );
    }

    @Test
    public void shouldFailConstructionIfNotModel() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new ModelImpl( getTransaction(), _repo, _repo.komodoLibrary( getTransaction() ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test( expected = KException.class )
    public void shouldFailGetChildWhenTypeIsWrong() throws Exception {
        final String name = "blah";
        this.model.addSource( getTransaction(), name );
        this.model.getChild( getTransaction(), name, "bogusType" );
    }

    @Test( expected = KException.class )
    public void shouldFailWhenChildNotFound() throws Exception {
        this.model.getChild( getTransaction(), "bogus" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyFunctionName() throws Exception {
        this.model.removeFunction( getTransaction(), EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyProcedureName() throws Exception {
        this.model.removeProcedure( getTransaction(), EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptySourceName() throws Exception {
        this.model.removeSource( getTransaction(), EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyTableName() throws Exception {
        this.model.removeTable( getTransaction(), EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyViewName() throws Exception {
        this.model.removeView( getTransaction(), EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullFunctionName() throws Exception {
        this.model.removeFunction( getTransaction(), null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullProcedureName() throws Exception {
        this.model.removeProcedure( getTransaction(), null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullSourceName() throws Exception {
        this.model.removeSource( getTransaction(), null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullTableName() throws Exception {
        this.model.removeTable( getTransaction(), null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullViewName() throws Exception {
        this.model.removeView( getTransaction(), null );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownFunction() throws Exception {
        this.model.removeFunction( getTransaction(), "unknown" );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownProcedure() throws Exception {
        this.model.removeProcedure( getTransaction(), "unknown" );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownSource() throws Exception {
        this.model.removeSource( getTransaction(), "unknown" );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownTable() throws Exception {
        this.model.removeTable( getTransaction(), "unknown" );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownView() throws Exception {
        this.model.removeView( getTransaction(), "unknown" );
    }

    @Test
    public void shouldGetFunctions() throws Exception {
        final int loops = 5;

        for ( int i = 0; i < loops; ++i ) {
            this.model.addPushdownFunction( getTransaction(), "pushdownFunction" + i );
            this.model.addUserDefinedFunction( getTransaction(), "udf" + i );
            this.model.addStoredProcedure( getTransaction(), "procedure" + i ); // these should not count
        }

        assertThat( this.model.getFunctions( getTransaction() ).length, is( loops * 2 ) );
    }

    @Test
    public void shouldGetProcedures() throws Exception {
        final int loops = 5;

        for ( int i = 0; i < loops; ++i ) {
            this.model.addStoredProcedure( getTransaction(), "storedProcedure" + i );
            this.model.addVirtualProcedure( getTransaction(), "virtualProcedure" + i );
            this.model.addPushdownFunction( getTransaction(), "pushdownFunction" + i ); // these should not count
        }

        assertThat( this.model.getProcedures( getTransaction() ).length, is( loops * 2 ) );
    }

    @Test
    public void shouldGetSources() throws Exception {
        final int numSources = 5;

        for ( int i = 0; i < numSources; ++i ) {
            this.model.addSource( getTransaction(), "source" + i );
        }

        assertThat( this.model.getSources( getTransaction() ).length, is( numSources ) );
    }

    @Test
    public void shouldGetTables() throws Exception {
        final int numTables = 5;

        for ( int i = 0; i < numTables; ++i ) {
            this.model.addTable( getTransaction(), "table" + i );
        }

        assertThat( this.model.getTables( getTransaction() ).length, is( numTables ) );
    }

    @Test
    public void shouldGetViews() throws Exception {
        final int numViews = 5;

        for ( int i = 0; i < numViews; ++i ) {
            this.model.addView( getTransaction(), "view" + i );
        }

        assertThat( this.model.getViews( getTransaction() ).length, is( numViews ) );
    }

    @Test
    public void shouldHaveCorrectChildTypes() {
        assertThat( Arrays.asList( this.model.getChildTypes() ),
                    hasItems( PushdownFunction.IDENTIFIER,
                              ModelSource.IDENTIFIER,
                              StoredProcedure.IDENTIFIER,
                              Table.IDENTIFIER,
                              UserDefinedFunction.IDENTIFIER,
                              View.IDENTIFIER,
                              VirtualProcedure.IDENTIFIER ) );
        assertThat( this.model.getChildTypes().length, is( 7 ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.model.getTypeIdentifier( getTransaction() ), is(KomodoType.MODEL));
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.model.getPropertyNames( getTransaction() );
        final String[] rawProps = this.model.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveStrongTypeChildren() throws Exception {
        this.model.addUserDefinedFunction( getTransaction(), "udf" );
        this.model.addPushdownFunction( getTransaction(), "pushdownFunction" );
        this.model.addStoredProcedure( getTransaction(), "storedProcedure" );
        this.model.addVirtualProcedure( getTransaction(), "virtualProcedure" );
        this.model.addTable( getTransaction(), "table" );
        this.model.addView( getTransaction(), "view" );
        this.model.addSource( getTransaction(), "source" );

        final KomodoObject[] kids = this.model.getChildren( getTransaction() );
        assertThat( kids.length, is( 7 ) );

        boolean foundUdf = false;
        boolean foundPushdownFunction = false;
        boolean foundStoredProcedure = false;
        boolean foundVirtualProcedure = false;
        boolean foundTable = false;
        boolean foundView = false;
        boolean foundSource = false;

        for ( final KomodoObject child : kids ) {
            if ( child instanceof UserDefinedFunction ) {
                if ( foundUdf ) {
                    fail();
                }

                foundUdf = true;
            } else if ( child instanceof PushdownFunction ) {
                if ( foundPushdownFunction ) {
                    fail();
                }

                foundPushdownFunction = true;
            } else if ( child instanceof StoredProcedure ) {
                if ( foundStoredProcedure ) {
                    fail();
                }

                foundStoredProcedure = true;
            } else if ( child instanceof VirtualProcedure ) {
                if ( foundVirtualProcedure ) {
                    fail();
                }

                foundVirtualProcedure = true;
            } else if ( child instanceof View ) { // must appear before Table
                if ( foundView ) {
                    fail();
                }

                foundView = true;
            } else if ( child instanceof Table ) {
                if ( foundTable ) {
                    fail();
                }

                foundTable = true;
            } else if ( child instanceof ModelSource ) {
                if ( foundSource ) {
                    fail();
                }

                foundSource = true;
            } else {
                fail();
            }
        }

        assertThat( foundPushdownFunction, is( true ) );
        assertThat( foundSource, is( true ) );
        assertThat( foundStoredProcedure, is( true ) );
        assertThat( foundTable, is( true ) );
        assertThat( foundUdf, is( true ) );
        assertThat( foundView, is( true ) );
        assertThat( foundVirtualProcedure, is( true ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.model.getPropertyNames( getTransaction() );
        final Filter[] filters = this.model.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldRemovePushdownFunction() throws Exception {
        final String name = "function";
        this.model.addPushdownFunction( getTransaction(), name );
        this.model.removeFunction( getTransaction(), name );
        assertThat( this.model.getFunctions( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveSource() throws Exception {
        final String name = "source";
        this.model.addSource( getTransaction(), name );
        this.model.removeSource( getTransaction(), name );
        assertThat( this.model.getSources( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveStoredProcedure() throws Exception {
        final String name = "procedure";
        this.model.addStoredProcedure( getTransaction(), name );
        this.model.removeProcedure( getTransaction(), name );
        assertThat( this.model.getProcedures( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveTable() throws Exception {
        final String name = "table";
        this.model.addTable( getTransaction(), name );
        this.model.removeTable( getTransaction(), name );
        assertThat( this.model.getTables( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveUserDefinedFunction() throws Exception {
        final String name = "function";
        this.model.addUserDefinedFunction( getTransaction(), name );
        this.model.removeFunction( getTransaction(), name );
        assertThat( this.model.getFunctions( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveView() throws Exception {
        final String name = "view";
        this.model.addView( getTransaction(), name );
        this.model.removeView( getTransaction(), name );
        assertThat( this.model.getViews( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveVirtualProcedure() throws Exception {
        final String name = "procedure";
        this.model.addVirtualProcedure( getTransaction(), name );
        this.model.removeProcedure( getTransaction(), name );
        assertThat( this.model.getProcedures( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "blah";
        this.model.rename( getTransaction(), newName );
        assertThat( this.model.getName( getTransaction() ), is( newName ) );
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String value = "description";
        this.model.setDescription( getTransaction(), value );
        assertThat( this.model.getDescription( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetMetadataType() throws Exception {
        final String value = "metadataType";
        this.model.setMetadataType( getTransaction(), value );
        assertThat( this.model.getMetadataType( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetMetadataTypeToDefaultWhenSettingModelDefinition() throws Exception {
        this.model.setMetadataType( getTransaction(), EMPTY_STRING );
        this.model.setModelDefinition( getTransaction(), "blah blah blah" );
        assertThat( this.model.getMetadataType( getTransaction() ), is( Model.DEFAULT_METADATA_TYPE ) );
    }
    
    @Test
    public void shouldSetModelDefinition() throws Exception {
        this.model.setModelDefinition( getTransaction(), "CREATE VIEW Tweet AS select * FROM twitterview.getTweets;" );
        commit();
        
        String mDefn = this.model.getModelDefinition(getTransaction());
        assertThat( mDefn, is( "CREATE VIEW Tweet AS select * FROM twitterview.getTweets;" ) );
    }

    @Test
    public void shouldSetModelType() throws Exception {
        final Type value = Type.VIRTUAL;
        this.model.setModelType( getTransaction(), value );
        assertThat( this.model.getModelType( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldSetVisible() throws Exception {
        final boolean value = !Model.DEFAULT_VISIBLE;
        this.model.setVisible( getTransaction(), value );
        assertThat( this.model.isVisible( getTransaction() ), is( value ) );
    }

    @Test
    public void shouldExportDdl() throws Exception {
        final int numTables = 5;

        for ( int i = 0; i < numTables; ++i ) {
            this.model.addTable( getTransaction(), "table" + i );
        }
        
        byte[] bytes = this.model.export(getTransaction(), new Properties());
        String exportedDdl = new String(bytes);
        
        assertThat( exportedDdl.contains("CREATE FOREIGN TABLE table1"), is( true ) );
    }

}
