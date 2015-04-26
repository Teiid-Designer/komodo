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
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.SchemaElement.SchemaElementType;
import org.komodo.relational.model.VirtualProcedure;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class VirtualProcedureImplTest extends RelationalModelTest {

    private static final String AS_ClAUSE = "BEGIN SELECT a,b FROM x; END";

    private VirtualProcedure procedure;

    @Before
    public void init() throws Exception {
        final Vdb vdb = RelationalModelFactory.createVdb( null, _repo, null, "vdb", "externalFilePath" );
        final Model model = RelationalModelFactory.createModel( null, _repo, vdb, "model" );
        this.procedure = RelationalModelFactory.createVirtualProcedure( null, _repo, model, "myProcedure" );
    }

    @Test
    public void shouldClearAsClauseWithEmptyString() throws Exception {
        this.procedure.setAsClauseStatement( null, AS_ClAUSE );
        this.procedure.setAsClauseStatement( null, StringConstants.EMPTY_STRING );
        assertThat( this.procedure.getAsClauseStatement( null ), is( nullValue() ) );
    }

    @Test
    public void shouldClearAsClauseWithNullString() throws Exception {
        this.procedure.setAsClauseStatement( null, AS_ClAUSE );
        this.procedure.setAsClauseStatement( null, null );
        assertThat( this.procedure.getAsClauseStatement( null ), is( nullValue() ) );
    }

    @Test
    public void shouldFailConstructionIfNotVirtualProcedure() {
        if (RelationalObjectImpl.VALIDATE_INITIAL_STATE) {
            try {
                new VirtualProcedureImpl(null, _repo, _repo.komodoLibrary(null).getAbsolutePath());
                fail();
            } catch (final KException e) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectSchemaElementType() throws Exception {
        assertThat( this.procedure.getSchemaElementType( null ), is( SchemaElementType.VIRTUAL ) );
    }

    @Test
    public void shouldNotCountStatementOptionsAsChildren() throws Exception {
        this.procedure.setStatementOption( null, "sledge", "hammer" );
        assertThat( this.procedure.getChildren( null ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveAsClauseStatementPropertyAfterConstruction() throws Exception {
        assertThat( this.procedure.getAsClauseStatement( null ), is( nullValue() ) );
        assertThat( this.procedure.hasProperty( null, TeiidDdlLexicon.CreateProcedure.STATEMENT ), is( false ) );
    }

    @Test
    public void shouldSetAsClauseProperty() throws Exception {
        final String value = AS_ClAUSE;
        this.procedure.setAsClauseStatement( null, value );
        assertThat( this.procedure.getAsClauseStatement( null ), is( value ) );
        assertThat( this.procedure.getProperty( null, TeiidDdlLexicon.CreateProcedure.STATEMENT ).getStringValue( null ),
                    is( value ) );
    }

}
