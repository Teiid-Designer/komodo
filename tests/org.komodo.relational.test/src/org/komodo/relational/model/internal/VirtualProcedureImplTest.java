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
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.SchemaElement.SchemaElementType;
import org.komodo.relational.model.VirtualProcedure;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoType;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class VirtualProcedureImplTest extends RelationalModelTest {

    private static final String AS_ClAUSE = "BEGIN SELECT a,b FROM x; END";

    private VirtualProcedure procedure;

    @Before
    public void init() throws Exception {
        final Vdb vdb = RelationalModelFactory.createVdb( this.uow, _repo, null, "vdb", "externalFilePath" );
        final Model model = RelationalModelFactory.createModel( this.uow, _repo, vdb, "model" );
        this.procedure = RelationalModelFactory.createVirtualProcedure( this.uow, _repo, model, "myProcedure" );
        commit();
    }

    @Test
    public void shouldClearAsClauseWithEmptyString() throws Exception {
        this.procedure.setAsClauseStatement( this.uow, AS_ClAUSE );
        this.procedure.setAsClauseStatement( this.uow, StringConstants.EMPTY_STRING );
        assertThat( this.procedure.getAsClauseStatement( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldClearAsClauseWithNullString() throws Exception {
        this.procedure.setAsClauseStatement( this.uow, AS_ClAUSE );
        this.procedure.setAsClauseStatement( this.uow, null );
        assertThat( this.procedure.getAsClauseStatement( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldFailConstructionIfNotVirtualProcedure() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new VirtualProcedureImpl( this.uow, _repo, _repo.komodoLibrary( this.uow ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectSchemaElementType() throws Exception {
        assertThat( this.procedure.getSchemaElementType( this.uow ), is( SchemaElementType.VIRTUAL ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.procedure.getTypeIdentifier( this.uow ), is(KomodoType.VIRTUAL_PROCEDURE));
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.procedure.getPropertyNames( this.uow );
        final String[] rawProps = this.procedure.getRawPropertyNames( this.uow );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.procedure.getPropertyNames( this.uow );
        final Filter[] filters = this.procedure.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotCountStatementOptionsAsChildren() throws Exception {
        this.procedure.setStatementOption( this.uow, "sledge", "hammer" );
        assertThat( this.procedure.getChildren( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveAsClauseStatementPropertyAfterConstruction() throws Exception {
        assertThat( this.procedure.getAsClauseStatement( this.uow ), is( nullValue() ) );
        assertThat( this.procedure.hasProperty( this.uow, TeiidDdlLexicon.CreateProcedure.STATEMENT ), is( false ) );
    }

    @Test
    public void shouldSetAsClauseProperty() throws Exception {
        final String value = AS_ClAUSE;
        this.procedure.setAsClauseStatement( this.uow, value );
        assertThat( this.procedure.getAsClauseStatement( this.uow ), is( value ) );
        assertThat( this.procedure.getProperty( this.uow, TeiidDdlLexicon.CreateProcedure.STATEMENT ).getStringValue( this.uow ),
                    is( value ) );
    }

}
