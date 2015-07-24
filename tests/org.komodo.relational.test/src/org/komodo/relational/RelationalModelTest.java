/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.test.utils.AbstractLocalRepositoryTest;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public class RelationalModelTest extends AbstractLocalRepositoryTest {

    protected static final String VDB_PATH = "/vdb/path/vdb.vdb";

    protected Model createModel() throws Exception {
        return createModel( this.name.getMethodName() + "-VDB", VDB_PATH, this.name.getMethodName() + "-Model" );
    }

    protected Model createModel( final String vdbName,
                                 final String vdbPath,
                                 final String modelName ) throws Exception {
        final WorkspaceManager mgr = WorkspaceManager.getInstance( _repo );
        final Vdb vdb = mgr.createVdb( this.uow, null, vdbName, vdbPath );
        final Model model = vdb.addModel( this.uow, modelName );

        assertThat( model.getPrimaryType( this.uow ).getName(), is( VdbLexicon.Vdb.DECLARATIVE_MODEL ) );
        assertThat( model.getName( this.uow ), is( modelName ) );
        return model;
    }

    protected Table createTable() throws Exception {
        return createTable( getDefaultVdbName(), VDB_PATH, getDefaultModelName(), getDefaultTableName() );
    }

    protected Table createTable( final String vdbName,
                                 final String vdbPath,
                                 final String modelName,
                                 final String tableName ) throws Exception {
        final WorkspaceManager mgr = WorkspaceManager.getInstance( _repo );
        final Vdb vdb = mgr.createVdb( this.uow, null, vdbName, vdbPath );
        final Model model = vdb.addModel( this.uow, modelName );
        return model.addTable( this.uow, tableName );
    }

    protected Vdb createVdb() throws Exception {
        return createVdb( getDefaultVdbName(), VDB_PATH );
    }

    protected Vdb createVdb( final String vdbName ) throws Exception {
        return createVdb( vdbName, VDB_PATH );
    }

    protected Vdb createVdb( final String vdbName,
                             final String originalFilePath ) throws Exception {
        return createVdb(vdbName, null, originalFilePath);
    }

    protected Vdb createVdb( final String vdbName,
                             final KomodoObject parent,
                             final String originalFilePath ) throws Exception {
        final WorkspaceManager mgr = WorkspaceManager.getInstance( _repo );
        final Vdb vdb = mgr.createVdb( this.uow, parent, vdbName, originalFilePath );

        assertThat( vdb.getPrimaryType( this.uow ).getName(), is( VdbLexicon.Vdb.VIRTUAL_DATABASE ) );
        assertThat( vdb.getName( this.uow ), is( vdbName ) );
        assertThat( vdb.getOriginalFilePath( this.uow ), is( originalFilePath ) );
        return vdb;
    }

    protected String getDefaultModelName() {
        return ( this.name.getMethodName() + "-Model" );
    }

    protected String getDefaultTableName() {
        return ( this.name.getMethodName() + "-Table" );
    }

    protected String getDefaultVdbName() {
        return ( this.name.getMethodName() + "-Vdb" );
    }

}
