/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Model.Type;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.relational.model.RelationalObject.Filter;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.UserDefinedFunction;
import org.komodo.relational.model.View;
import org.komodo.relational.model.VirtualProcedure;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;

@SuppressWarnings( { "javadoc", "nls" } )
public final class ModelImplTest extends RelationalModelTest {

    private static final String NAME = "model";

    private Model model;

    @Before
    public void init() throws Exception {
        final WorkspaceManager wsMgr = WorkspaceManager.getInstance( this.uow, _repo );
        final KomodoObject workspace = _repo.komodoWorkspace( this.uow );
        final Vdb vdb = RelationalModelFactory.createVdb( this.uow, _repo, workspace.getAbsolutePath(), "parentVdb", "/test1" );
        this.model = wsMgr.createModel( this.uow, vdb, NAME );
        commit();
    }

    @Test
    public void shouldAddPushdownFunction() throws Exception {
        final String name = "function";
        final PushdownFunction function = this.model.addPushdownFunction( this.uow, name );
        assertThat( function, is( notNullValue() ) );
        assertThat( function.getName( this.uow ), is( name ) );
        assertThat( this.model.getFunctions( this.uow ).length, is( 1 ) );
        assertThat( this.model.getChildren( this.uow )[0], is( instanceOf( PushdownFunction.class ) ) );
    }

    @Test
    public void shouldAddSource() throws Exception {
        final String name = "source";
        final ModelSource source = this.model.addSource( this.uow, name );
        assertThat( source, is( notNullValue() ) );
        assertThat( source.getName( this.uow ), is( name ) );
        assertThat( this.model.getSources( this.uow ).length, is( 1 ) );
        assertThat( this.model.getChildren( this.uow )[0], is( instanceOf( ModelSource.class ) ) );
    }

    @Test
    public void shouldAddStoredProcedure() throws Exception {
        final String name = "procedure";
        final StoredProcedure procedure = this.model.addStoredProcedure( this.uow, name );
        assertThat( procedure, is( notNullValue() ) );
        assertThat( procedure.getName( this.uow ), is( name ) );
        assertThat( this.model.getProcedures( this.uow ).length, is( 1 ) );
        assertThat( this.model.getChildren( this.uow )[0], is( instanceOf( StoredProcedure.class ) ) );
    }

    @Test
    public void shouldAddTable() throws Exception {
        final String name = "table";
        final Table table = this.model.addTable( this.uow, name );
        assertThat( table, is( notNullValue() ) );
        assertThat( table.getName( this.uow ), is( name ) );
        assertThat( this.model.getTables( this.uow ).length, is( 1 ) );
        assertThat( this.model.getChildren( this.uow )[0], is( instanceOf( Table.class ) ) );
    }

    @Test
    public void shouldAddUserDefinedFunction() throws Exception {
        final String name = "function";
        final UserDefinedFunction function = this.model.addUserDefinedFunction( this.uow, name );
        assertThat( function, is( notNullValue() ) );
        assertThat( function.getName( this.uow ), is( name ) );
        assertThat( this.model.getFunctions( this.uow ).length, is( 1 ) );
        assertThat( this.model.getChildren( this.uow )[0], is( instanceOf( UserDefinedFunction.class ) ) );
    }

    @Test
    public void shouldAddView() throws Exception {
        final String name = "view";
        final View view = this.model.addView( this.uow, name );
        assertThat( view, is( notNullValue() ) );
        assertThat( view.getName( this.uow ), is( name ) );
        assertThat( this.model.getViews( this.uow ).length, is( 1 ) );
        assertThat( this.model.getChildren( this.uow )[0], is( instanceOf( View.class ) ) );
    }

    @Test
    public void shouldAddVirtualProcedure() throws Exception {
        final String name = "procedure";
        final VirtualProcedure procedure = this.model.addVirtualProcedure( this.uow, name );
        assertThat( procedure, is( notNullValue() ) );
        assertThat( procedure.getName( this.uow ), is( name ) );
        assertThat( this.model.getProcedures( this.uow ).length, is( 1 ) );
        assertThat( this.model.getChildren( this.uow )[0], is( instanceOf( VirtualProcedure.class ) ) );
    }

    @Test
    public void shouldAllowEmptyDescriptionWhenRemoving() throws Exception {
        this.model.setDescription( this.uow, "blah" );
        this.model.setDescription( this.uow, StringConstants.EMPTY_STRING );
        assertThat( this.model.getDescription( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullDescriptionWhenRemoving() throws Exception {
        this.model.setDescription( this.uow, "blah" );
        this.model.setDescription( this.uow, null );
        assertThat( this.model.getDescription( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldAllowNullModelTypeWhenSettingToDefaultValue() throws Exception {
        this.model.setModelType( this.uow, Type.VIRTUAL );
        this.model.setModelType( this.uow, null );
        assertThat( this.model.getModelType( this.uow ), is( Type.DEFAULT_VALUE ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyPushdownFunctionName() throws Exception {
        this.model.addPushdownFunction( this.uow, EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptySourceName() throws Exception {
        this.model.addSource( this.uow, EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyStoredProcedureName() throws Exception {
        this.model.addStoredProcedure( this.uow, EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyTableName() throws Exception {
        this.model.addTable( this.uow, EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyUserDefinedFunctionName() throws Exception {
        this.model.addUserDefinedFunction( this.uow, EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyViewName() throws Exception {
        this.model.addView( this.uow, EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingEmptyVirtualProcedureName() throws Exception {
        this.model.addVirtualProcedure( this.uow, EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullPushdownFunctionName() throws Exception {
        this.model.addPushdownFunction( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullSourceName() throws Exception {
        this.model.addSource( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullStoredProcedureName() throws Exception {
        this.model.addStoredProcedure( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullTableName() throws Exception {
        this.model.addTable( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullUserDefinedFunctionName() throws Exception {
        this.model.addUserDefinedFunction( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullViewName() throws Exception {
        this.model.addView( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingNullVirtualProcedureName() throws Exception {
        this.model.addVirtualProcedure( this.uow, null );
    }

    @Test
    public void shouldFailConstructionIfNotModel() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new ModelImpl( this.uow, _repo, _repo.komodoLibrary( this.uow ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyFunctionName() throws Exception {
        this.model.removeFunction( this.uow, EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyProcedureName() throws Exception {
        this.model.removeProcedure( this.uow, EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptySourceName() throws Exception {
        this.model.removeSource( this.uow, EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyTableName() throws Exception {
        this.model.removeTable( this.uow, EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveEmptyViewName() throws Exception {
        this.model.removeView( this.uow, EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullFunctionName() throws Exception {
        this.model.removeFunction( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullProcedureName() throws Exception {
        this.model.removeProcedure( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullSourceName() throws Exception {
        this.model.removeSource( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullTableName() throws Exception {
        this.model.removeTable( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailTryingToRemoveNullViewName() throws Exception {
        this.model.removeView( this.uow, null );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownFunction() throws Exception {
        this.model.removeFunction( this.uow, "unknown" );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownProcedure() throws Exception {
        this.model.removeProcedure( this.uow, "unknown" );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownSource() throws Exception {
        this.model.removeSource( this.uow, "unknown" );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownTable() throws Exception {
        this.model.removeTable( this.uow, "unknown" );
    }

    @Test( expected = KException.class )
    public void shouldFailTryingToRemoveUnknownView() throws Exception {
        this.model.removeView( this.uow, "unknown" );
    }

    @Test
    public void shouldGetFunctions() throws Exception {
        final int loops = 5;

        for ( int i = 0; i < loops; ++i ) {
            this.model.addPushdownFunction( this.uow, "pushdownFunction" + i );
            this.model.addUserDefinedFunction( this.uow, "udf" + i );
            this.model.addStoredProcedure( this.uow, "procedure" + i ); // these should not count
        }

        assertThat( this.model.getFunctions( this.uow ).length, is( loops * 2 ) );
    }

    @Test
    public void shouldGetProcedures() throws Exception {
        final int loops = 5;

        for ( int i = 0; i < loops; ++i ) {
            this.model.addStoredProcedure( this.uow, "storedProcedure" + i );
            this.model.addVirtualProcedure( this.uow, "virtualProcedure" + i );
            this.model.addPushdownFunction( this.uow, "pushdownFunction" + i ); // these should not count
        }

        assertThat( this.model.getProcedures( this.uow ).length, is( loops * 2 ) );
    }

    @Test
    public void shouldGetSources() throws Exception {
        final int numSources = 5;

        for ( int i = 0; i < numSources; ++i ) {
            this.model.addSource( this.uow, "source" + i );
        }

        assertThat( this.model.getSources( this.uow ).length, is( numSources ) );
    }

    @Test
    public void shouldGetTables() throws Exception {
        final int numTables = 5;

        for ( int i = 0; i < numTables; ++i ) {
            this.model.addTable( this.uow, "table" + i );
        }

        assertThat( this.model.getTables( this.uow ).length, is( numTables ) );
    }

    @Test
    public void shouldGetViews() throws Exception {
        final int numViews = 5;

        for ( int i = 0; i < numViews; ++i ) {
            this.model.addView( this.uow, "view" + i );
        }

        assertThat( this.model.getViews( this.uow ).length, is( numViews ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.model.getPropertyNames( this.uow );
        final String[] rawProps = this.model.getRawPropertyNames( this.uow );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveStrongTypeChildren() throws Exception {
        this.model.addUserDefinedFunction( this.uow, "udf" );
        this.model.addPushdownFunction( this.uow, "pushdownFunction" );
        this.model.addStoredProcedure( this.uow, "storedProcedure" );
        this.model.addVirtualProcedure( this.uow, "virtualProcedure" );
        this.model.addTable( this.uow, "table" );
        this.model.addView( this.uow, "view" );
        this.model.addSource( this.uow, "source" );

        final KomodoObject[] kids = this.model.getChildren( this.uow );
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
        final String[] filteredProps = this.model.getPropertyNames( this.uow );
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
        this.model.addPushdownFunction( this.uow, name );
        this.model.removeFunction( this.uow, name );
        assertThat( this.model.getFunctions( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveSource() throws Exception {
        final String name = "source";
        this.model.addSource( this.uow, name );
        this.model.removeSource( this.uow, name );
        assertThat( this.model.getSources( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveStoredProcedure() throws Exception {
        final String name = "procedure";
        this.model.addStoredProcedure( this.uow, name );
        this.model.removeProcedure( this.uow, name );
        assertThat( this.model.getProcedures( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveTable() throws Exception {
        final String name = "table";
        this.model.addTable( this.uow, name );
        this.model.removeTable( this.uow, name );
        assertThat( this.model.getTables( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveUserDefinedFunction() throws Exception {
        final String name = "function";
        this.model.addUserDefinedFunction( this.uow, name );
        this.model.removeFunction( this.uow, name );
        assertThat( this.model.getFunctions( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveView() throws Exception {
        final String name = "view";
        this.model.addView( this.uow, name );
        this.model.removeView( this.uow, name );
        assertThat( this.model.getViews( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveVirtualProcedure() throws Exception {
        final String name = "procedure";
        this.model.addVirtualProcedure( this.uow, name );
        this.model.removeProcedure( this.uow, name );
        assertThat( this.model.getProcedures( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String value = "description";
        this.model.setDescription( this.uow, value );
        assertThat( this.model.getDescription( this.uow ), is( value ) );
    }

    @Test
    public void shouldSetModelType() throws Exception {
        final Type value = Type.VIRTUAL;
        this.model.setModelType( this.uow, value );
        assertThat( this.model.getModelType( this.uow ), is( value ) );
    }

}
