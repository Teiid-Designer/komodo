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
package org.komodo.relational.dataservice.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Properties;
import java.util.TimeZone;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.junit.Before;
import org.junit.Test;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.dataservice.ConnectionEntry;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.dataservice.DataserviceManifest;
import org.komodo.relational.dataservice.DdlEntry;
import org.komodo.relational.dataservice.DriverEntry;
import org.komodo.relational.dataservice.ResourceEntry;
import org.komodo.relational.dataservice.ServiceVdbEntry;
import org.komodo.relational.dataservice.UdfEntry;
import org.komodo.relational.dataservice.VdbEntry;
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.model.Model;
import org.komodo.relational.resource.DdlFile;
import org.komodo.relational.resource.Driver;
import org.komodo.relational.resource.ResourceFile;
import org.komodo.relational.resource.UdfFile;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.test.utils.TestUtilities;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;
import org.w3c.dom.Document;

@SuppressWarnings( { "javadoc", "nls" } )
public final class DataserviceImplTest extends RelationalModelTest {

    private static final String SERVICE_NAME = "myService";

    protected Dataservice dataservice;
    private WorkspaceManager mgr;

    @Before
    public void init() throws Exception {
        this.dataservice = createDataservice( SERVICE_NAME );
        this.mgr = WorkspaceManager.getInstance( _repo, getTransaction() );

    }

    @Test
    public void shouldHaveName() throws Exception {
        assertThat( this.dataservice.getName( getTransaction() ), is( SERVICE_NAME ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.dataservice.getPropertyNames( getTransaction() );
        final String[] rawProps = this.dataservice.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.dataservice.getPropertyNames( getTransaction() );
        final Filter[] filters = this.dataservice.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.dataservice.getPrimaryType( getTransaction() ).getName(), is( DataVirtLexicon.DataService.NODE_TYPE ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.dataservice.getTypeIdentifier( getTransaction() ), is(KomodoType.DATASERVICE));
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String descr = "This is a description";
        this.dataservice.setDescription(getTransaction(), descr);

        assertThat( this.dataservice.getDescription( getTransaction() ), is( descr ) );
    }

    @Test
    public void shouldSetModifiedBy() throws Exception {
        final String user = "elvis";
        this.dataservice.setModifiedBy( getTransaction(), user );
        assertThat( this.dataservice.getModifiedBy( getTransaction() ), is( user ) );
    }

    @Test
    public void shouldSetLastModified() throws Exception {
        final Calendar date = Calendar.getInstance();
        date.setTimeZone(TimeZone.getTimeZone("Europe/London"));
        date.set( 2016, 8, 23, 13, 48, 33 );
        this.dataservice.setLastModified( getTransaction(), date );
        assertThat( this.dataservice.getLastModified( getTransaction() ), is( date ) );
    }

    @Test
    public void shouldAddAllChildren() throws Exception {
        final String vdbName = "MyServiceVdb";
        final int vdbVersion = 3;
        final Vdb serviceVdb = this.mgr.createVdb( getTransaction(), null, vdbName, "externalFilePath" );
        serviceVdb.setVdbName( getTransaction(), vdbName );
        serviceVdb.setVersion( getTransaction(), vdbVersion );
        commit(); // needed so that searching for reference will work

        final Vdb oldServiceVdb = this.dataservice.setServiceVdb( getTransaction(), serviceVdb );
        assertThat( oldServiceVdb, is( nullValue() ) );
        final ServiceVdbEntry entry = this.dataservice.getServiceVdbEntry( getTransaction() );
        assertThat( entry, is( notNullValue() ) );
        entry.addDependencyEntry( getTransaction(), "A" );
        entry.addDependencyEntry( getTransaction(), "B" );
        assertThat( entry.getDependencies( getTransaction() ).length, is( 2 ) );

        this.dataservice.addConnectionEntry( getTransaction(), "connection" );
        this.dataservice.addDdlEntry( getTransaction(), "ddl" );
        this.dataservice.addDriverEntry( getTransaction(), "driver" );
        this.dataservice.addResourceEntry( getTransaction(), "resource" );
        this.dataservice.addUdfEntry( getTransaction(), "udf" );
        this.dataservice.addVdbEntry( getTransaction(), "vdb" );

        assertThat( this.dataservice.getConnectionEntries( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getDdlEntries( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getDriverEntries( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getResourceEntries( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getUdfEntries( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getVdbEntries( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getServiceVdbEntry( getTransaction() ), is( notNullValue() ) );
        assertThat( this.dataservice.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.dataservice.getChildren( getTransaction() ).length, is( 7 ) );
    }

    @Test
    public void shouldAddConnection() throws Exception {
        final String connectionName = "MyConnection";
        final String jndiName = "jndiNameGoesHere";
        final Datasource connection = this.mgr.createDatasource( getTransaction(), null, connectionName );
        connection.setJndiName( getTransaction(), jndiName );
        commit(); // needed so that searching for reference will work

        final ConnectionEntry entry = this.dataservice.addConnection( getTransaction(), connection );
        assertThat( entry.getJndiName( getTransaction() ), is( jndiName ) );
        assertThat( entry.getReference( getTransaction() ), is( notNullValue() ) );
        assertThat( entry.getReference( getTransaction() ), is( instanceOf( Datasource.class ) ) );

        assertThat( this.dataservice.getConnectionEntries( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getConnectionEntries( getTransaction(), connectionName ).length, is( 1 ) );
        assertThat( this.dataservice.getConnectionPlan( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getConnectionPlan( getTransaction() )[ 0 ], is( connection.getAbsolutePath() ) );
        assertThat( this.dataservice.getConnections( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getConnections( getTransaction() )[ 0 ].getName( getTransaction() ), is( connectionName ) );
        assertThat( this.dataservice.hasChild( getTransaction(), connectionName ), is( true ) );
        assertThat( this.dataservice.hasChild( getTransaction(), connectionName, DataVirtLexicon.ConnectionEntry.NODE_TYPE ),
                    is( true ) );
        assertThat( this.dataservice.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.dataservice.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getChildren( getTransaction(), connectionName ).length, is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(), DataVirtLexicon.ConnectionEntry.NODE_TYPE ).length,
                    is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(),
                                                        DataVirtLexicon.ConnectionEntry.NODE_TYPE,
                                                        connectionName ).length,
                    is( 1 ) );
    }

    @Test
    public void shouldAddConnectionEntry() throws Exception {
        final String connectionName = "MyConnection";
        final ConnectionEntry entry = this.dataservice.addConnectionEntry( getTransaction(), connectionName );
        assertThat( entry.getJndiName( getTransaction() ), is( nullValue() ) );
        assertThat( entry.getReference( getTransaction() ), is( nullValue() ) );

        assertThat( this.dataservice.getConnectionEntries( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getConnectionEntries( getTransaction(), connectionName ).length, is( 1 ) );
        assertThat( this.dataservice.getConnectionPlan( getTransaction() ).length, is( 0 ) );
        assertThat( this.dataservice.getConnections( getTransaction() ).length, is( 0 ) );
        assertThat( this.dataservice.hasChild( getTransaction(), connectionName ), is( true ) );
        assertThat( this.dataservice.hasChild( getTransaction(), connectionName, DataVirtLexicon.ConnectionEntry.NODE_TYPE ),
                    is( true ) );
        assertThat( this.dataservice.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.dataservice.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getChildren( getTransaction(), connectionName ).length, is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(), DataVirtLexicon.ConnectionEntry.NODE_TYPE ).length,
                    is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(),
                                                        DataVirtLexicon.ConnectionEntry.NODE_TYPE,
                                                        connectionName ).length,
                    is( 1 ) );
    }

    @Test
    public void shouldAddDdlEntry() throws Exception {
        final String ddlName = "MyDdl";
        final DdlEntry entry = this.dataservice.addDdlEntry( getTransaction(), ddlName );
        assertThat( entry.getReference( getTransaction() ), is( nullValue() ) );
        assertThat( this.dataservice.getDdlEntries( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getDdlEntries( getTransaction(), ddlName ).length, is( 1 ) );
        assertThat( this.dataservice.getDdlFiles( getTransaction() ).length, is( 0 ) );
        assertThat( this.dataservice.getDdlPlan( getTransaction() ).length, is( 0 ) );
        assertThat( this.dataservice.hasChild( getTransaction(), ddlName ), is( true ) );
        assertThat( this.dataservice.hasChild( getTransaction(), ddlName, DataVirtLexicon.ResourceEntry.DDL_ENTRY_NODE_TYPE ),
                    is( true ) );
        assertThat( this.dataservice.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.dataservice.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getChildren( getTransaction(), ddlName ).length, is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(),
                                                        DataVirtLexicon.ResourceEntry.DDL_ENTRY_NODE_TYPE ).length,
                    is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(),
                                                        DataVirtLexicon.ResourceEntry.DDL_ENTRY_NODE_TYPE,
                                                        ddlName ).length,
                    is( 1 ) );
    }

    @Test
    public void shouldAddDdlFile() throws Exception {
        final String ddlName = "MyDdl";
        final byte[] content = "this is my DDL content".getBytes();
        final DdlFile ddl = this.mgr.createDdlFile( getTransaction(), null, ddlName, content );
        commit(); // needed so that searching for reference will work

        final DdlEntry entry = this.dataservice.addDdlFile( getTransaction(), ddl );
        assertThat( entry.getReference( getTransaction() ), is( notNullValue() ) );
        assertThat( entry.getReference( getTransaction() ), is( instanceOf( DdlFile.class ) ) );
        assertThat( entry.getReference( getTransaction() ).getContent( getTransaction() ), is( notNullValue() ) );
        assertThat( entry.getReference( getTransaction() ).export( getTransaction(), null ), is( content ) );

        assertThat( this.dataservice.getDdlEntries( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getDdlEntries( getTransaction(), ddlName ).length, is( 1 ) );
        assertThat( this.dataservice.getDdlPlan( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getDdlPlan( getTransaction() )[ 0 ], is( ddl.getAbsolutePath() ) );
        assertThat( this.dataservice.getDdlFiles( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getDdlFiles( getTransaction() )[ 0 ].getName( getTransaction() ), is( ddlName ) );
        assertThat( this.dataservice.hasChild( getTransaction(), ddlName ), is( true ) );
        assertThat( this.dataservice.hasChild( getTransaction(), ddlName, DataVirtLexicon.ResourceEntry.DDL_ENTRY_NODE_TYPE ),
                    is( true ) );
        assertThat( this.dataservice.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.dataservice.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getChildren( getTransaction(), ddlName ).length, is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(),
                                                        DataVirtLexicon.ResourceEntry.DDL_ENTRY_NODE_TYPE ).length,
                    is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(),
                                                        DataVirtLexicon.ResourceEntry.DDL_ENTRY_NODE_TYPE,
                                                        ddlName ).length,
                    is( 1 ) );
    }

    @Test
    public void shouldAddDriver() throws Exception {
        final String driverName = "MyDriver";
        final byte[] content = "this is my driver content".getBytes();
        final Driver driver = this.mgr.createDriver( getTransaction(), null, driverName, content );
        commit(); // needed so that searching for reference will work

        final DriverEntry entry = this.dataservice.addDriverFile( getTransaction(), driver );
        assertThat( entry.getReference( getTransaction() ), is( notNullValue() ) );
        assertThat( entry.getReference( getTransaction() ), is( instanceOf( Driver.class ) ) );
        assertThat( entry.getReference( getTransaction() ).getContent( getTransaction() ), is( notNullValue() ) );
        assertThat( entry.getReference( getTransaction() ).export( getTransaction(), null ), is( content ) );

        assertThat( this.dataservice.getDriverEntries( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getDriverEntries( getTransaction(), driverName ).length, is( 1 ) );
        assertThat( this.dataservice.getDrivers( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getDrivers( getTransaction() )[ 0 ].getName( getTransaction() ), is( driverName ) );
        assertThat( this.dataservice.getDriverPlan( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getDriverPlan( getTransaction() )[ 0 ], is( driver.getAbsolutePath() ) );
        assertThat( this.dataservice.hasChild( getTransaction(), driverName ), is( true ) );
        assertThat( this.dataservice.hasChild( getTransaction(),
                                               driverName,
                                               DataVirtLexicon.ResourceEntry.DRIVER_ENTRY_NODE_TYPE ),
                    is( true ) );
        assertThat( this.dataservice.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.dataservice.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getChildren( getTransaction(), driverName ).length, is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(),
                                                        DataVirtLexicon.ResourceEntry.DRIVER_ENTRY_NODE_TYPE ).length,
                    is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(),
                                                        DataVirtLexicon.ResourceEntry.DRIVER_ENTRY_NODE_TYPE,
                                                        driverName ).length,
                    is( 1 ) );
    }

    @Test
    public void shouldAddDriverEntry() throws Exception {
        final String driverName = "MyDriver";
        final DriverEntry entry = this.dataservice.addDriverEntry( getTransaction(), driverName );
        assertThat( entry.getReference( getTransaction() ), is( nullValue() ) );
        assertThat( this.dataservice.getDriverEntries( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getDriverEntries( getTransaction(), driverName ).length, is( 1 ) );
        assertThat( this.dataservice.getDriverPlan( getTransaction() ).length, is( 0 ) );
        assertThat( this.dataservice.getDrivers( getTransaction() ).length, is( 0 ) );
        assertThat( this.dataservice.hasChild( getTransaction(), driverName ), is( true ) );
        assertThat( this.dataservice.hasChild( getTransaction(),
                                               driverName,
                                               DataVirtLexicon.ResourceEntry.DRIVER_ENTRY_NODE_TYPE ),
                    is( true ) );
        assertThat( this.dataservice.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.dataservice.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getChildren( getTransaction(), driverName ).length, is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(),
                                                        DataVirtLexicon.ResourceEntry.DRIVER_ENTRY_NODE_TYPE ).length,
                    is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(),
                                                        DataVirtLexicon.ResourceEntry.DRIVER_ENTRY_NODE_TYPE,
                                                        driverName ).length,
                    is( 1 ) );
    }

    @Test
    public void shouldAddResourceEntry() throws Exception {
        final String resourceName = "MyResource";
        final ResourceEntry entry = this.dataservice.addResourceEntry( getTransaction(), resourceName );
        assertThat( entry.getReference( getTransaction() ), is( nullValue() ) );
        assertThat( this.dataservice.getResourceEntries( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getResourceEntries( getTransaction(), resourceName ).length, is( 1 ) );
        assertThat( this.dataservice.getResourceFiles( getTransaction() ).length, is( 0 ) );
        assertThat( this.dataservice.getResourcePlan( getTransaction() ).length, is( 0 ) );
        assertThat( this.dataservice.hasChild( getTransaction(), resourceName ), is( true ) );
        assertThat( this.dataservice.hasChild( getTransaction(), resourceName, DataVirtLexicon.ResourceEntry.NODE_TYPE ),
                    is( true ) );
        assertThat( this.dataservice.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.dataservice.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getChildren( getTransaction(), resourceName ).length, is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(), DataVirtLexicon.ResourceEntry.NODE_TYPE ).length,
                    is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(),
                                                        DataVirtLexicon.ResourceEntry.NODE_TYPE,
                                                        resourceName ).length,
                    is( 1 ) );
    }

    @Test
    public void shouldAddResourceFile() throws Exception {
        final String resourceName = "MyResource";
        final byte[] content = "this is my resource content".getBytes();
        final ResourceFile resource = this.mgr.createResourceFile( getTransaction(), null, resourceName, content );
        commit(); // needed so that searching for reference will work

        final ResourceEntry entry = this.dataservice.addResourceFile( getTransaction(), resource );
        assertThat( entry.getReference( getTransaction() ), is( notNullValue() ) );
        assertThat( entry.getReference( getTransaction() ), is( instanceOf( ResourceFile.class ) ) );
        assertThat( entry.getReference( getTransaction() ).getContent( getTransaction() ), is( notNullValue() ) );
        assertThat( entry.getReference( getTransaction() ).export( getTransaction(), null ), is( content ) );

        assertThat( this.dataservice.getResourceEntries( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getResourceEntries( getTransaction(), resourceName ).length, is( 1 ) );
        assertThat( this.dataservice.getResourceFiles( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getResourceFiles( getTransaction() )[ 0 ].getName( getTransaction() ), is( resourceName ) );
        assertThat( this.dataservice.getResourcePlan( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getResourcePlan( getTransaction() )[ 0 ], is( resource.getAbsolutePath() ) );
        assertThat( this.dataservice.hasChild( getTransaction(), resourceName ), is( true ) );
        assertThat( this.dataservice.hasChild( getTransaction(), resourceName, DataVirtLexicon.ResourceEntry.NODE_TYPE ),
                    is( true ) );
        assertThat( this.dataservice.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.dataservice.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getChildren( getTransaction(), resourceName ).length, is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(), DataVirtLexicon.ResourceEntry.NODE_TYPE ).length,
                    is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(),
                                                        DataVirtLexicon.ResourceEntry.NODE_TYPE,
                                                        resourceName ).length,
                    is( 1 ) );
    }

    @Test
    public void shouldAddUdfEntry() throws Exception {
        final String udfName = "MyUdf";
        final UdfEntry entry = this.dataservice.addUdfEntry( getTransaction(), udfName );
        assertThat( entry.getReference( getTransaction() ), is( nullValue() ) );
        assertThat( this.dataservice.getUdfEntries( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getUdfEntries( getTransaction(), udfName ).length, is( 1 ) );
        assertThat( this.dataservice.getUdfFiles( getTransaction() ).length, is( 0 ) );
        assertThat( this.dataservice.getUdfPlan( getTransaction() ).length, is( 0 ) );
        assertThat( this.dataservice.hasChild( getTransaction(), udfName ), is( true ) );
        assertThat( this.dataservice.hasChild( getTransaction(), udfName, DataVirtLexicon.ResourceEntry.UDF_ENTRY_NODE_TYPE ),
                    is( true ) );
        assertThat( this.dataservice.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.dataservice.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getChildren( getTransaction(), udfName ).length, is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(),
                                                        DataVirtLexicon.ResourceEntry.UDF_ENTRY_NODE_TYPE ).length,
                    is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(),
                                                        DataVirtLexicon.ResourceEntry.UDF_ENTRY_NODE_TYPE,
                                                        udfName ).length,
                    is( 1 ) );
    }

    @Test
    public void shouldAddUdfFile() throws Exception {
        final String udfName = "MyUdf";
        final byte[] content = "this is my UDF content".getBytes();
        final UdfFile udf = this.mgr.createUdfFile( getTransaction(), null, udfName, content );
        commit(); // needed so that searching for reference will work

        final UdfEntry entry = this.dataservice.addUdfFile( getTransaction(), udf );
        assertThat( entry.getReference( getTransaction() ), is( notNullValue() ) );
        assertThat( entry.getReference( getTransaction() ), is( instanceOf( UdfFile.class ) ) );
        assertThat( entry.getReference( getTransaction() ).getContent( getTransaction() ), is( notNullValue() ) );
        assertThat( entry.getReference( getTransaction() ).export( getTransaction(), null ), is( content ) );

        assertThat( this.dataservice.getUdfEntries( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getUdfEntries( getTransaction(), udfName ).length, is( 1 ) );
        assertThat( this.dataservice.getUdfFiles( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getUdfFiles( getTransaction() )[ 0 ].getName( getTransaction() ), is( udfName ) );
        assertThat( this.dataservice.getUdfPlan( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getUdfPlan( getTransaction() )[ 0 ], is( udf.getAbsolutePath() ) );
        assertThat( this.dataservice.hasChild( getTransaction(), udfName ), is( true ) );
        assertThat( this.dataservice.hasChild( getTransaction(), udfName, DataVirtLexicon.ResourceEntry.UDF_ENTRY_NODE_TYPE ),
                    is( true ) );
        assertThat( this.dataservice.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.dataservice.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getChildren( getTransaction(), udfName ).length, is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(),
                                                        DataVirtLexicon.ResourceEntry.UDF_ENTRY_NODE_TYPE ).length,
                    is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(),
                                                        DataVirtLexicon.ResourceEntry.UDF_ENTRY_NODE_TYPE,
                                                        udfName ).length,
                    is( 1 ) );
    }

    @Test
    public void shouldAddVdb() throws Exception {
        final String vdbName = "MyVdb";
        final int vdbVersion = 2;
        final Vdb vdb = this.mgr.createVdb( getTransaction(), null, vdbName, "externalFilePath" );
        vdb.setVdbName( getTransaction(), vdbName );
        vdb.setVersion( getTransaction(), vdbVersion );
        commit(); // needed so that searching for reference will work

        final VdbEntry entry = this.dataservice.addVdb( getTransaction(), vdb );
        assertThat( entry.getVdbName( getTransaction() ), is( vdbName ) );
        assertThat( entry.getVdbVersion( getTransaction() ), is( Integer.toString( vdbVersion ) ) );
        assertThat( entry.getReference( getTransaction() ), is( notNullValue() ) );
        assertThat( entry.getReference( getTransaction() ), is( instanceOf( Vdb.class ) ) );

        assertThat( this.dataservice.getVdbEntries( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getVdbEntries( getTransaction(), vdbName ).length, is( 1 ) );
        assertThat( this.dataservice.getVdbPlan( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getVdbPlan( getTransaction() )[ 0 ], is( vdb.getAbsolutePath() ) );
        assertThat( this.dataservice.getVdbs( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getVdbs( getTransaction() )[ 0 ].getName( getTransaction() ), is( vdbName ) );
        assertThat( this.dataservice.hasChild( getTransaction(), vdbName ), is( true ) );
        assertThat( this.dataservice.hasChild( getTransaction(), vdbName, DataVirtLexicon.VdbEntry.NODE_TYPE ),
                    is( true ) );
        assertThat( this.dataservice.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.dataservice.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getChildren( getTransaction(), vdbName ).length, is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(), DataVirtLexicon.VdbEntry.NODE_TYPE ).length,
                    is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(),
                                                        DataVirtLexicon.VdbEntry.NODE_TYPE,
                                                        vdbName ).length,
                    is( 1 ) );
    }

    @Test
    public void shouldAddVdbEntry() throws Exception {
        final String vdbName = "MyVdb";
        final VdbEntry entry = this.dataservice.addVdbEntry( getTransaction(), vdbName );
        assertThat( entry.getReference( getTransaction() ), is( nullValue() ) );
        assertThat( this.dataservice.getVdbEntries( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getVdbEntries( getTransaction(), vdbName ).length, is( 1 ) );
        assertThat( this.dataservice.getVdbPlan( getTransaction() ).length, is( 0 ) );
        assertThat( this.dataservice.getVdbs( getTransaction() ).length, is( 0 ) );
        assertThat( this.dataservice.hasChild( getTransaction(), vdbName ), is( true ) );
        assertThat( this.dataservice.hasChild( getTransaction(), vdbName, DataVirtLexicon.VdbEntry.NODE_TYPE ), is( true ) );
        assertThat( this.dataservice.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.dataservice.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getChildren( getTransaction(), vdbName ).length, is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(), DataVirtLexicon.VdbEntry.NODE_TYPE ).length, is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(), DataVirtLexicon.VdbEntry.NODE_TYPE, vdbName ).length,
                    is( 1 ) );
    }

    @Test
    public void shouldSetServiceVdb() throws Exception {
        final String vdbName = "MyServiceVdb";
        final int vdbVersion = 3;
        final Vdb vdb = this.mgr.createVdb( getTransaction(), null, vdbName, "externalFilePath" );
        vdb.setVdbName( getTransaction(), vdbName );
        vdb.setVersion( getTransaction(), vdbVersion );
        commit(); // needed so that searching for reference will work

        final Vdb oldServiceVdb = this.dataservice.setServiceVdb( getTransaction(), vdb );
        assertThat( oldServiceVdb, is( nullValue() ) );
        assertThat( this.dataservice.getServiceVdbEntry( getTransaction() ), is( notNullValue() ) );

        final ServiceVdbEntry entry = this.dataservice.getServiceVdbEntry( getTransaction() );
        assertThat( entry.getVdbName( getTransaction() ), is( vdbName ) );
        assertThat( entry.getVdbVersion( getTransaction() ), is( Integer.toString( vdbVersion ) ) );

        assertThat( this.dataservice.hasChild( getTransaction(), vdbName ), is( true ) );
        assertThat( this.dataservice.hasChild( getTransaction(), vdbName, DataVirtLexicon.ServiceVdbEntry.NODE_TYPE ),
                    is( true ) );
        assertThat( this.dataservice.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.dataservice.getChildren( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getChildren( getTransaction(), vdbName ).length, is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(), DataVirtLexicon.ServiceVdbEntry.NODE_TYPE ).length,
                    is( 1 ) );
        assertThat( this.dataservice.getChildrenOfType( getTransaction(),
                                                        DataVirtLexicon.ServiceVdbEntry.NODE_TYPE,
                                                        vdbName ).length,
                    is( 1 ) );
    }

    @Test
    public void shouldGetServiceVdbViewModelAndView() throws Exception {
        final String name = "childVdb";
        final int version = 2;
        final WorkspaceManager mgr = WorkspaceManager.getInstance( _repo, getTransaction() );
        final Vdb serviceVdb = mgr.createVdb( getTransaction(), null, name, "externalFilePath" );
        serviceVdb.setVersion( getTransaction(), version );

        // Add a physical model
        final Model physModel = serviceVdb.addModel( getTransaction(), "physicalModel" );
        physModel.setModelType( getTransaction(), Model.Type.PHYSICAL );

        // Add a virtual model
        final String serviceViewModel = "serviceViewModel";
        final Model virtualModel = serviceVdb.addModel( getTransaction(), serviceViewModel );
        virtualModel.setModelType( getTransaction(), Model.Type.VIRTUAL );

        // Add a view to the virtual model
        final String serviceView = "serviceView";
        virtualModel.addView( getTransaction(), serviceView );

        commit(); // need this so that VDB will be found by query that sets reference

        // Add VDB to data service
        final Vdb old = this.dataservice.setServiceVdb( getTransaction(), serviceVdb );
        assertThat( old, is( nullValue() ) ); // not replacing
        assertThat( this.dataservice.getServiceVdbEntry( getTransaction() ), is( notNullValue() ) );
        assertThat( this.dataservice.getServiceVdbEntry( getTransaction() ).getName( getTransaction() ), is( name ) );
        assertThat( this.dataservice.getServiceVdb( getTransaction() ), is( notNullValue() ) );
        assertThat( this.dataservice.getServiceViewModelName( getTransaction() ), is( serviceViewModel ) );
        assertThat( this.dataservice.getServiceViewName( getTransaction() ), is( serviceView ) );
    }

    @Test
    public void shouldExport() throws Exception {
        final String name1 = "childVdb1";
        final String name2 = "childVdb2";

        final Vdb vdb1 = this.mgr.createVdb(getTransaction(), null, name1, "externalFilePath1");
        final Vdb vdb2 = this.mgr.createVdb(getTransaction(), null, name2, "externalFilePath2");

        this.dataservice.addVdb(getTransaction(), vdb1);
        this.dataservice.addVdb(getTransaction(), vdb2);

        final Vdb serviceVdb = this.mgr.createVdb(getTransaction(), null, SERVICE_NAME, "externalSvcPath");
        this.dataservice.setServiceVdb( getTransaction(), serviceVdb );
        commit(); // needed so that searching for reference will work
        
        byte[] dsBytes = this.dataservice.export(getTransaction(), new Properties());
        assertNotNull(dsBytes);

        ByteArrayInputStream is = null;
        ZipInputStream zipStream = null;
        try {
            is = new ByteArrayInputStream(dsBytes);
            zipStream = new ZipInputStream(is);

            assertTrue(zipStream.available() > 0);

            int entries = 0;
            ZipEntry entry;
            while ((entry = zipStream.getNextEntry()) != null) {
                ByteArrayOutputStream bos = null;

                try {
                    String name = entry.getName();
                    if (DataserviceManifest.MANIFEST.equals(name) ||
                        name.startsWith("vdbs/" + name1) ||
                        name.startsWith("vdbs/" + name2) ||
                        name.startsWith(SERVICE_NAME)) {
                            entries++;
                    }

                    bos = new ByteArrayOutputStream();
                    final byte[] buf = new byte[DataserviceConveyor.BUFFER_SIZE];
                    int length;

                    while ((length = zipStream.read(buf, 0, buf.length)) >= 0) {
                        bos.write(buf, 0, length);
                    }

                    byte[] contentBytes = bos.toByteArray();
                    assertNotNull(contentBytes);

                    String content = new String(contentBytes);
                    assertNotNull(content);

                    InputStream contentStream = null;
                    try {
                        contentStream = new ByteArrayInputStream(content.getBytes());
                        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
                        DocumentBuilder builder = factory.newDocumentBuilder();
                        Document document = builder.parse(contentStream);
                        assertNotNull(document);
                    } finally {
                        if (contentStream != null)
                            contentStream.close();
                    }

                } finally {
                    if (bos != null)
                        bos.close();

                    zipStream.closeEntry();
                }
            }

            assertEquals(4, entries);
        } finally {
            if (zipStream != null)
                zipStream.close();
            if (is != null)
                is.close();
        }
    }

    @Test
    public void shouldRoundTripDataService() throws Exception {
        InputStream importStream = TestUtilities.sampleDataserviceExample();
        assertThat( importStream, is( notNullValue() ) );

        ImportMessages importMessages = new ImportMessages();
        ImportOptions importOptions = new ImportOptions();
        importOptions.setOption( OptionKeys.NAME, "MyDataService" );

        DataserviceConveyor conveyor = new DataserviceConveyor( _repo );
        KomodoObject parent = _repo.komodoWorkspace( getTransaction() );
        conveyor.dsImport( getTransaction(), importStream, parent, importOptions, importMessages );
        assertThat( importMessages.hasError(), is( false ) );
        commit();

        final String dataServiceName = "MyDataService";
        assertThat( parent.hasChild( getTransaction(), dataServiceName ), is( true ) );
        KomodoObject theDataService = parent.getChild( getTransaction(), dataServiceName );
        final List< String > paths = new ArrayList<String>();

        // children (entries)
        assertEntry( theDataService,
                     "product-view-vdb.xml",
                     DataVirtLexicon.ServiceVdbEntry.NODE_TYPE,
                     VdbLexicon.Vdb.VIRTUAL_DATABASE,
                     paths );
        assertEntry( theDataService,
                     "books-driver-1.jar",
                     DataVirtLexicon.ResourceEntry.DRIVER_ENTRY_NODE_TYPE,
                     DataVirtLexicon.ResourceFile.DRIVER_FILE_NODE_TYPE,
                     paths );
        assertEntry( theDataService,
                     "books-driver-2.jar",
                     DataVirtLexicon.ResourceEntry.DRIVER_ENTRY_NODE_TYPE,
                     DataVirtLexicon.ResourceFile.DRIVER_FILE_NODE_TYPE,
                     paths );
        assertEntry( theDataService,
                     "portfolio-driver.jar",
                     DataVirtLexicon.ResourceEntry.DRIVER_ENTRY_NODE_TYPE,
                     DataVirtLexicon.ResourceFile.DRIVER_FILE_NODE_TYPE,
                     paths );
        assertEntry( theDataService,
                     "firstDdl.ddl",
                     DataVirtLexicon.ResourceEntry.DDL_ENTRY_NODE_TYPE,
                     DataVirtLexicon.ResourceFile.DDL_FILE_NODE_TYPE,
                     paths );
        assertEntry( theDataService,
                     "secondDdl.ddl",
                     DataVirtLexicon.ResourceEntry.DDL_ENTRY_NODE_TYPE,
                     DataVirtLexicon.ResourceFile.DDL_FILE_NODE_TYPE,
                     paths );
        assertEntry( theDataService,
                     "firstResource.xml",
                     DataVirtLexicon.ResourceEntry.NODE_TYPE,
                     DataVirtLexicon.ResourceFile.NODE_TYPE,
                     paths );
        assertEntry( theDataService,
                     "secondResource.xml",
                     DataVirtLexicon.ResourceEntry.NODE_TYPE,
                     DataVirtLexicon.ResourceFile.NODE_TYPE,
                     paths );
        assertEntry( theDataService,
                     "firstUdf.jar",
                     DataVirtLexicon.ResourceEntry.UDF_ENTRY_NODE_TYPE,
                     null, // publish policy is NEVER
                     paths );
        assertEntry( theDataService,
                     "secondUdf.jar",
                     DataVirtLexicon.ResourceEntry.UDF_ENTRY_NODE_TYPE,
                     DataVirtLexicon.ResourceFile.UDF_FILE_NODE_TYPE,
                     paths );
        assertEntry( theDataService,
                     "books-connection.xml",
                     DataVirtLexicon.ConnectionEntry.NODE_TYPE,
                     DataVirtLexicon.Connection.NODE_TYPE,
                     paths );
        assertEntry( theDataService,
                     "portfolio-connection.xml",
                     DataVirtLexicon.ConnectionEntry.NODE_TYPE,
                     DataVirtLexicon.Connection.NODE_TYPE,
                     paths );
        assertEntry( theDataService,
                     "books-vdb.xml",
                     DataVirtLexicon.VdbEntry.NODE_TYPE,
                     VdbLexicon.Vdb.VIRTUAL_DATABASE,
                     paths );
        assertEntry( theDataService,
                     "Portfolio-vdb.xml",
                     DataVirtLexicon.VdbEntry.NODE_TYPE,
                     VdbLexicon.Vdb.VIRTUAL_DATABASE,
                     paths );

        WorkspaceManager mgr = WorkspaceManager.getInstance( _repo, getTransaction() );
        Dataservice ds = mgr.resolve( getTransaction(), theDataService, Dataservice.class );
        assertThat( ds, is( notNullValue() ) );

        String vdbName = ds.getServiceVdb( getTransaction() ).getVdbName( getTransaction() );
        assertThat( vdbName, is( "DynamicProducts" ) );

        // round trip
        final byte[] dsBytes = ds.export( getTransaction(), null );
        assertThat( dsBytes, is( notNullValue() ) );

        final int numPaths = ( paths.size() + 1 ); // add one for the manifest as it is not persisted
        int numEntries = 0;

        ZipInputStream zis = null;
        try {
            zis = new ZipInputStream( new ByteArrayInputStream( dsBytes ) );
            ZipEntry entry = null;

            while ( ( entry = zis.getNextEntry() ) != null ) {
                final String entryName = entry.getName();

                if ( entry.isDirectory() ) {
                    continue;
                } else {
                    paths.remove( entryName );
                    ++numEntries;
                }
            }
        } finally {
            if (zis != null)
                zis.close();
        }

        assertThat( paths.isEmpty(), is( true ) );
        assertThat( numPaths, is( numEntries ) );
    }

    private void assertEntry( final KomodoObject dataService,
                              final String entryName,
                              final String entryNodeType,
                              final String referenceNodeType,
                              final List< String > paths ) throws Exception {
        assertThat( dataService.hasChild( getTransaction(), entryName ), is( true ) );

        final KomodoObject entry = dataService.getChild( getTransaction(), entryName );
        assertThat( entry.getPrimaryType( getTransaction() ).getName(), is( entryNodeType ) );

        // paths will be compared to exported entry paths
        paths.add( entry.getProperty( getTransaction(), DataVirtLexicon.DataServiceEntry.PATH )
                        .getStringValue( getTransaction() ) );

        if ( referenceNodeType != null ) {
            assertThat( entry.hasProperty( getTransaction(), DataVirtLexicon.DataServiceEntry.SOURCE_RESOURCE ), is( true ) );

            // find reference
            final String refId = entry.getProperty( getTransaction(), DataVirtLexicon.DataServiceEntry.SOURCE_RESOURCE )
                                      .getStringValue( getTransaction() );
            final KomodoObject ref = _repo.getUsingId( getTransaction(), refId );
            assertThat( ref, is( notNullValue() ) );
            assertThat( ref.getPrimaryType( getTransaction() ).getName(), is( referenceNodeType ) );
        }
    }
}
