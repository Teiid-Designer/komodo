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
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.junit.Before;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.importer.vdb.VdbImporter;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.test.utils.TestUtilities;
import org.komodo.utils.FileUtils;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;
import org.w3c.dom.Document;

@SuppressWarnings( { "javadoc", "nls" } )
public final class DataserviceImplTest extends RelationalModelTest {

    private static final String SERVICE_NAME = "myService";

    protected Dataservice dataservice;

    @Before
    public void init() throws Exception {
        this.dataservice = createDataservice( SERVICE_NAME );
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
        assertThat( this.dataservice.getPrimaryType( getTransaction() ).getName(), is( KomodoLexicon.DataService.NODE_TYPE ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.dataservice.getTypeIdentifier( getTransaction() ), is(KomodoType.DATASERVICE));
    }
    
    @Test
    public void shouldAddVdb() throws Exception {
        final String name = "childVdb";
        final Vdb vdb = this.dataservice.addVdb(getTransaction(), name, "externalPath");
        
        assertThat( vdb, is( notNullValue() ) );
        assertThat( vdb.getName( getTransaction() ), is( name ) );
        assertThat( this.dataservice.getVdbs( getTransaction() ).length, is( 1 ) );
        assertThat( this.dataservice.getChildren( getTransaction() )[0], is( instanceOf( Vdb.class ) ) );

        assertThat( this.dataservice.hasChild( getTransaction(), name ), is( true ) );
        assertThat( this.dataservice.hasChild( getTransaction(), name, VdbLexicon.Vdb.VIRTUAL_DATABASE ), is( true ) );
        assertThat( this.dataservice.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.dataservice.getChild( getTransaction(), name ), is( vdb ) );
        assertThat( this.dataservice.getChild( getTransaction(), name, VdbLexicon.Vdb.VIRTUAL_DATABASE ), is( vdb ) );
    }

    @Test
    public void shouldGetVdbs() throws Exception {
        final String name1 = "childVdb1";
        final String name2 = "childVdb2";
        final Vdb vdb1 = this.dataservice.addVdb(getTransaction(), name1, "externalPath1");
        final Vdb vdb2 = this.dataservice.addVdb(getTransaction(), name2, "externalPath1");
        
        assertThat( vdb1, is( notNullValue() ) );
        assertThat( vdb2, is( notNullValue() ) );
        assertThat( vdb1.getName( getTransaction() ), is( name1 ) );
        assertThat( vdb2.getName( getTransaction() ), is( name2 ) );
        assertThat( this.dataservice.getVdbs( getTransaction() ).length, is( 2 ) );
        assertThat( this.dataservice.getChildren( getTransaction() )[1], is( instanceOf( Vdb.class ) ) );

        assertThat( this.dataservice.hasChild( getTransaction(), name1 ), is( true ) );
        assertThat( this.dataservice.hasChild( getTransaction(), name2 ), is( true ) );
        assertThat( this.dataservice.hasChild( getTransaction(), name1, VdbLexicon.Vdb.VIRTUAL_DATABASE ), is( true ) );
        assertThat( this.dataservice.hasChild( getTransaction(), name2, VdbLexicon.Vdb.VIRTUAL_DATABASE ), is( true ) );
        assertThat( this.dataservice.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.dataservice.getChild( getTransaction(), name1 ), is( vdb1 ) );
        assertThat( this.dataservice.getChild( getTransaction(), name2 ), is( vdb2 ) );
        assertThat( this.dataservice.getChild( getTransaction(), name1, VdbLexicon.Vdb.VIRTUAL_DATABASE ), is( vdb1) );
        assertThat( this.dataservice.getChild( getTransaction(), name2, VdbLexicon.Vdb.VIRTUAL_DATABASE ), is( vdb2) );
    }

    @Test
    public void shouldExport() throws Exception {
        final String name1 = "childVdb1";
        final String name2 = "childVdb2";
        this.dataservice.addVdb(getTransaction(), name1, "externalPath1");
        this.dataservice.addVdb(getTransaction(), name2, "externalPath1");

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
                    System.out.println("Entry Name: " + name);
                    if (DataserviceManifest.MANIFEST.equals(name) ||
                        name.startsWith(name1) ||
                        name.startsWith(name2)) {
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

            assertEquals(3, entries);
        } finally {
            if (zipStream != null)
                zipStream.close();
            if (is != null)
                is.close();
        }
    }

    @Test
    public void shouldExportPerfectZip() throws Exception {
        VdbImporter importer = new VdbImporter(_repo);
        ImportMessages importMessages = new ImportMessages();
        ImportOptions importOptions = new ImportOptions();

        importer.importVdb(getTransaction(),
                                               TestUtilities.portfolioExample(), dataservice,
                                               importOptions, importMessages);
        assertFalse(importMessages.hasError());

        importer.importVdb(getTransaction(),
                                               TestUtilities.tweetExample(), dataservice,
                                               importOptions, importMessages);
        assertFalse(importMessages.hasError());

        commit(State.COMMITTED);

        File dsZip = File.createTempFile("DSZip", DOT + ZIP);
        dsZip.deleteOnExit();
        byte[] dsBytes = this.dataservice.export(getTransaction(), new Properties());
        FileUtils.write(dsBytes, dsZip);

        ZipFile zipfile = null;
        try {
            zipfile = new ZipFile(dsZip);
        } catch (IOException e) {
            fail("Zip file created is corrupt " + e.getLocalizedMessage());
        } finally {
            try {
                if (zipfile != null) {
                    zipfile.close();
                    zipfile = null;
                }
            } catch (IOException e) {}
        }
    }

    @Test
    public void shouldImportDataService() throws Exception {
        InputStream importStream = TestUtilities.getResourceAsStream(DataserviceImplTest.class, "dataservice", "sample-ds.zip");
        assertNotNull(importStream);

        ImportMessages importMessages = new ImportMessages();
        ImportOptions importOptions = new ImportOptions();
        String serviceName = "dsService";
        importOptions.setOption(OptionKeys.NAME, serviceName);

        DataserviceConveyor conveyor = new DataserviceConveyor(_repo);
        KomodoObject parent = _repo.komodoWorkspace(getTransaction());
        conveyor.dsImport(getTransaction(), importStream, parent, importOptions, importMessages);
        assertFalse(importMessages.hasError());

        assertTrue(parent.hasChild(getTransaction(), serviceName));
        KomodoObject theDataService = parent.getChild(getTransaction(), serviceName);

        assertTrue(theDataService.hasChild(getTransaction(), TestUtilities.PORTFOLIO_VDB_NAME));
        assertTrue(theDataService.hasChild(getTransaction(), TestUtilities.TWEET_EXAMPLE_VDB_NAME));
    }
}
