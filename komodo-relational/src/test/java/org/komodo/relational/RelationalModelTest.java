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
package org.komodo.relational;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

import org.komodo.core.KomodoLexicon;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.folder.Folder;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Schema;
import org.komodo.relational.model.Table;
import org.komodo.relational.resource.Driver;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.test.utils.AbstractLocalRepositoryTest;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public class RelationalModelTest extends AbstractLocalRepositoryTest {

    protected static final String VDB_PATH = "/vdb/path/vdb.vdb";

    protected Model createModel() throws Exception {
        return createModel( this.name.getMethodName() + "-VDB", VDB_PATH, this.name.getMethodName() + "-Model" );
    }

    protected Model createModel( final String vdbName,
                                 final String vdbPath,
                                 final String modelName ) throws Exception {
        final WorkspaceManager mgr = WorkspaceManager.getInstance(_repo, getTransaction());
        final Vdb vdb = mgr.createVdb( getTransaction(), null, vdbName, vdbPath );
        final Model model = vdb.addModel( getTransaction(), modelName );

        assertThat( model.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.Vdb.DECLARATIVE_MODEL ) );
        assertThat( model.getName( getTransaction() ), is( modelName ) );
        return model;
    }

    protected Table createTable() throws Exception {
        return createTable( getDefaultVdbName(), VDB_PATH, getDefaultModelName(), getDefaultTableName() );
    }

    protected Table createTable( final String vdbName,
                                 final String vdbPath,
                                 final String modelName,
                                 final String tableName ) throws Exception {
        final WorkspaceManager mgr = WorkspaceManager.getInstance(_repo, getTransaction());
        final Vdb vdb = mgr.createVdb( getTransaction(), null, vdbName, vdbPath );
        final Model model = vdb.addModel( getTransaction(), modelName );
        return model.addTable( getTransaction(), tableName );
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
        final WorkspaceManager mgr = WorkspaceManager.getInstance(_repo, getTransaction());
        final Vdb vdb = mgr.createVdb( getTransaction(), parent, vdbName, originalFilePath );

        assertThat( vdb.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.Vdb.VIRTUAL_DATABASE ) );
        assertThat( vdb.getName( getTransaction() ), is( vdbName ) );
        assertThat( vdb.getOriginalFilePath( getTransaction() ), is( originalFilePath ) );
        return vdb;
    }

    protected Schema createSchema() throws Exception {
        return createSchema( getDefaultSchemaName() );
    }

    protected Schema createSchema( final String schemaName ) throws Exception {
        return createSchema( schemaName, null );
    }

    protected Schema createSchema( final String schemaName,
                                   final KomodoObject parent ) throws Exception {
        final WorkspaceManager mgr = WorkspaceManager.getInstance(_repo, getTransaction());
        final Schema schema = mgr.createSchema( getTransaction(), parent, schemaName );

        assertThat( schema.getPrimaryType( getTransaction() ).getName(), is( KomodoLexicon.Schema.NODE_TYPE ) );
        assertThat( schema.getName( getTransaction() ), is( schemaName ) );
        return schema;
    }

    protected Teiid createTeiid() throws Exception {
        return createTeiid( getDefaultTeiidName() );
    }

    protected Teiid createTeiid( final String teiidName ) throws Exception {
        return createTeiid( teiidName, null );
    }

    protected Teiid createTeiid( final String teiidName,
                                 final KomodoObject parent ) throws Exception {
        final WorkspaceManager mgr = WorkspaceManager.getInstance(_repo, getTransaction());
        final Teiid teiid = mgr.createTeiid( getTransaction(), parent, teiidName );

        assertThat( teiid.getPrimaryType( getTransaction() ).getName(), is( KomodoLexicon.Teiid.NODE_TYPE ) );
        assertThat( teiid.getName( getTransaction() ), is( teiidName ) );
        commit();
        return teiid;
    }

    protected Dataservice createDataservice() throws Exception {
        return createDataservice( getDefaultDataserviceName() );
    }

    protected Dataservice createDataservice( final String serviceName ) throws Exception {
        return createDataservice( serviceName, null );
    }

    protected Dataservice createDataservice( final String serviceName,
                                             final KomodoObject parent ) throws Exception {
        final WorkspaceManager mgr = WorkspaceManager.getInstance(_repo, getTransaction());
        final Dataservice ds = mgr.createDataservice( getTransaction(), parent, serviceName );

        assertThat( ds.getPrimaryType( getTransaction() ).getName(), is( DataVirtLexicon.DataService.NODE_TYPE ) );
        assertThat( ds.getName( getTransaction() ), is( serviceName ) );
        return ds;
    }

    protected Connection createConnection() throws Exception {
        return createConnection( getDefaultConnectionName() );
    }

    protected Connection createConnection( final String dsName ) throws Exception {
        return createConnection( dsName, null );
    }

    protected Connection createConnection( final String dsName,
                                           final KomodoObject parent ) throws Exception {
        final WorkspaceManager mgr = WorkspaceManager.getInstance(_repo, getTransaction());
        final Connection ds = mgr.createConnection( getTransaction(), parent, dsName );

        assertThat( ds.getPrimaryType( getTransaction() ).getName(), is( DataVirtLexicon.Connection.NODE_TYPE ) );
        assertThat( ds.getName( getTransaction() ), is( dsName ) );
        return ds;
    }

    protected Driver createDriver( final String driverName,
                                   final byte[] content ) throws Exception {
        return createDriver( driverName, null );
    }

    protected Driver createDriver( final String driverName,
                                   final KomodoObject parent,
                                   final byte[] content ) throws Exception {
        final WorkspaceManager mgr = WorkspaceManager.getInstance( _repo, getTransaction() );
        final Driver driver = mgr.createDriver( getTransaction(), parent, driverName, content );

        assertThat( driver.getPrimaryType( getTransaction() ).getName(),
                    is( DataVirtLexicon.ResourceFile.DRIVER_FILE_NODE_TYPE ) );
        assertThat( driver.getName( getTransaction() ), is( driverName ) );
        return driver;
    }

    protected Folder createFolder() throws Exception {
        return createFolder( getDefaultFolderName() );
    }

    protected Folder createFolder( final String folderName ) throws Exception {
        return createFolder( folderName, null );
    }

    protected Folder createFolder( final String folderName,
                                             final KomodoObject parent ) throws Exception {
        final WorkspaceManager mgr = WorkspaceManager.getInstance(_repo, getTransaction());
        final Folder folder = mgr.createFolder( getTransaction(), parent, folderName );

        assertThat( folder.getPrimaryType( getTransaction() ).getName(), is( KomodoLexicon.Folder.NODE_TYPE ) );
        assertThat( folder.getName( getTransaction() ), is( folderName ) );
        return folder;
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

    protected String getDefaultSchemaName() {
        return ( this.name.getMethodName() + "-Schema" );
    }

    protected String getDefaultTeiidName() {
        return ( this.name.getMethodName() + "-Teiid" );
    }

    protected String getDefaultDataserviceName() {
        return ( this.name.getMethodName() + "-Dataservice" );
    }

    protected String getDefaultConnectionName() {
        return ( this.name.getMethodName() + "-Connection" );
    }

    protected String getDefaultFolderName() {
        return ( this.name.getMethodName() + "-Folder" );
    }

}
