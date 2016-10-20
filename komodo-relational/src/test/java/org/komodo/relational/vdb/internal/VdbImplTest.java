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
package org.komodo.relational.vdb.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsCollectionContaining.hasItems;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import org.junit.Before;
import org.junit.Test;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.importer.vdb.VdbImporter;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Entry;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.Vdb.VdbManifest;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyDescriptor;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

@SuppressWarnings( { "javadoc", "nls" } )
public final class VdbImplTest extends RelationalModelTest {

    private static final String PATH = "/Users/sledge/hammer/MyVdb.vdb";
    private static final String VDB_NAME = "vdb";

    protected Vdb vdb;

    @Before
    public void init() throws Exception {
        this.vdb = createVdb( VDB_NAME, PATH );
    }

    @Test
    public void shouldAddDataRole() throws Exception {
        final String name = "dataRole";
        final DataRole dataRole = this.vdb.addDataRole( getTransaction(), name );
        assertThat( dataRole, is( notNullValue() ) );
        assertThat( this.vdb.getDataRoles( getTransaction() ).length, is( 1 ) );

        final DataRole added = this.vdb.getDataRoles( getTransaction() )[0];
        assertThat( added, is( dataRole ) );
        assertThat( added.getName( getTransaction() ), is( name ) );
        assertThat( added.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.DataRole.DATA_ROLE ) );
        assertThat( this.vdb.getChildren( getTransaction() )[0], is( instanceOf( DataRole.class ) ) );

        assertThat( this.vdb.hasChild( getTransaction(), name ), is( true ) );
        assertThat( this.vdb.hasChild( getTransaction(), name, VdbLexicon.DataRole.DATA_ROLE ), is( true ) );
        assertThat( this.vdb.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.vdb.getChild( getTransaction(), name ), is( added ) );
        assertThat( this.vdb.getChild( getTransaction(), name, VdbLexicon.DataRole.DATA_ROLE ), is( added ) );
    }

    @Test
    public void shouldAddEntry() throws Exception {
        final String name = "entry";
        final String path = "/my/path";
        final Entry entry = this.vdb.addEntry( getTransaction(), name, path );
        assertThat( entry, is( notNullValue() ) );
        assertThat( this.vdb.getEntries( getTransaction() ).length, is( 1 ) );

        final Entry added = this.vdb.getEntries( getTransaction() )[0];
        assertThat( added, is( entry ) );
        assertThat( added.getName( getTransaction() ), is( name ) );
        assertThat( added.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.Entry.ENTRY ) );
        assertThat( added.getPath( getTransaction() ), is( path ) );
        assertThat( this.vdb.getChildren( getTransaction() )[0], is( instanceOf( Entry.class ) ) );

        assertThat( this.vdb.hasChild( getTransaction(), name ), is( true ) );
        assertThat( this.vdb.hasChild( getTransaction(), name, VdbLexicon.Entry.ENTRY ), is( true ) );
        assertThat( this.vdb.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.vdb.getChild( getTransaction(), name ), is( added ) );
        assertThat( this.vdb.getChild( getTransaction(), name, VdbLexicon.Entry.ENTRY ), is( added ) );
    }

    @Test
    public void shouldAddImport() throws Exception {
        final String name = "vdbImport";
        final VdbImport vdbImport = this.vdb.addImport( getTransaction(), name );
        assertThat( vdbImport, is( notNullValue() ) );
        assertThat( this.vdb.getImports( getTransaction() ).length, is( 1 ) );

        final VdbImport added = this.vdb.getImports( getTransaction() )[0];
        assertThat( added, is( vdbImport ) );
        assertThat( added.getName( getTransaction() ), is( name ) );
        assertThat( added.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.ImportVdb.IMPORT_VDB ) );
        assertThat( this.vdb.getChildren( getTransaction() )[0], is( instanceOf( VdbImport.class ) ) );

        assertThat( this.vdb.hasChild( getTransaction(), name ), is( true ) );
        assertThat( this.vdb.hasChild( getTransaction(), name, VdbLexicon.ImportVdb.IMPORT_VDB ), is( true ) );
        assertThat( this.vdb.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.vdb.getChild( getTransaction(), name ), is( added ) );
        assertThat( this.vdb.getChild( getTransaction(), name, VdbLexicon.ImportVdb.IMPORT_VDB ), is( added ) );
    }

    @Test
    public void shouldAddModel() throws Exception {
        final String name = "model";
        final Model model = this.vdb.addModel( getTransaction(), name );
        assertThat( model, is( notNullValue() ) );
        assertThat( this.vdb.getModels( getTransaction() ).length, is( 1 ) );

        final Model added = this.vdb.getModels( getTransaction() )[0];
        assertThat( added, is( model ) );
        assertThat( added.getName( getTransaction() ), is( name ) );
        assertThat( added.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.Vdb.DECLARATIVE_MODEL ) );
        assertThat( this.vdb.getChildren( getTransaction() )[0], is( instanceOf( Model.class ) ) );

        assertThat( this.vdb.hasChild( getTransaction(), name ), is( true ) );
        assertThat( this.vdb.hasChild( getTransaction(), name, VdbLexicon.Vdb.DECLARATIVE_MODEL ), is( true ) );
        assertThat( this.vdb.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.vdb.getChild( getTransaction(), name ), is( added ) );
        assertThat( this.vdb.getChild( getTransaction(), name, VdbLexicon.Vdb.DECLARATIVE_MODEL ), is( added ) );
    }

    @Test
    public void shouldAddTranslator() throws Exception {
        final String name = "translator";
        final String type = "oracle";
        final Translator translator = this.vdb.addTranslator( getTransaction(), name, type );
        assertThat( translator, is( notNullValue() ) );
        assertThat( this.vdb.getTranslators( getTransaction() ).length, is( 1 ) );

        final Translator added = this.vdb.getTranslators( getTransaction() )[0];
        assertThat( added, is( translator ) );
        assertThat( added.getName( getTransaction() ), is( name ) );
        assertThat( added.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.Translator.TRANSLATOR ) );
        assertThat( added.getType( getTransaction() ), is( type ) );
        assertThat( added, is( instanceOf( Translator.class ) ) );

        assertThat( this.vdb.hasChild( getTransaction(), name ), is( true ) );
        assertThat( this.vdb.hasChild( getTransaction(), name, VdbLexicon.Translator.TRANSLATOR ), is( true ) );
        assertThat( this.vdb.hasChildren( getTransaction() ), is( true ) );
        assertThat( this.vdb.getChild( getTransaction(), name ), is( added ) );
        assertThat( this.vdb.getChild( getTransaction(), name, VdbLexicon.Translator.TRANSLATOR ), is( added ) );
    }

    @Test
    public void shouldCreateManifestForEmptyVdb() throws Exception {
        final VdbManifest manifest = this.vdb.createManifest( getTransaction(), new Properties() );
        assertThat( manifest, is( notNullValue() ) );
        assertThat( manifest.asDocument(), is( notNullValue() ) );
    }

    @Test
    public void shouldCreateManifestForVdb() throws Exception {
        { // setup
            this.vdb.setVdbName( getTransaction(), "twitter" );
            this.vdb.setVersion( getTransaction(), 1 );
            this.vdb.setDescription( getTransaction(), "Shows how to call Web Services" );
            this.vdb.setProperty( getTransaction(), "UseConnectorMetadata", "cached" );

            final Model twitter = this.vdb.addModel( getTransaction(), "twitter" );
            twitter.setModelType( getTransaction(), Model.Type.PHYSICAL );

            final Model twitterview = this.vdb.addModel( getTransaction(), "twitterview" );
            twitterview.setModelType( getTransaction(), Model.Type.VIRTUAL );

            final Translator translator = this.vdb.addTranslator( getTransaction(), "rest", "ws" );
            translator.setProperty( getTransaction(), "DefaultBinding", "HTTP" );
        }

        final VdbManifest manifest = this.vdb.createManifest( getTransaction(), new Properties() );
        assertThat( manifest, is( notNullValue() ) );
        assertThat( manifest.asDocument(), is( notNullValue() ) );
    }

    @Test
    public void shouldExportEmptyVdb() throws Exception {
        byte[] manifest = this.vdb.export( getTransaction(), new Properties() );
        assertThat( manifest, is( notNullValue() ) );
        assertTrue( manifest.length > 0 );
    }

    @Test
    public void shouldExportVdb() throws Exception {
        { // setup
            this.vdb.setVdbName( getTransaction(), "twitter" );
            this.vdb.setVersion( getTransaction(), 1 );
            this.vdb.setDescription( getTransaction(), "Shows how to call Web Services" );
            this.vdb.setProperty( getTransaction(), "UseConnectorMetadata", "cached" );

            final Model twitter = this.vdb.addModel( getTransaction(), "twitter" );
            twitter.setModelType( getTransaction(), Model.Type.PHYSICAL );

            final Model twitterview = this.vdb.addModel( getTransaction(), "twitterview" );
            twitterview.setModelType( getTransaction(), Model.Type.VIRTUAL );

            final Translator translator = this.vdb.addTranslator( getTransaction(), "rest", "ws" );
            translator.setProperty( getTransaction(), "DefaultBinding", "HTTP" );
        }

        // test
        byte[] manifest = this.vdb.export( getTransaction(), new Properties() );
        assertThat( manifest, is( notNullValue() ) );
        assertTrue( manifest.length > 0 );
    }

    @Test
    public void shouldFailConstructionIfNotVdb() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new VdbImpl( getTransaction(), _repo, _repo.komodoLibrary( getTransaction() ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test( expected = KException.class )
    public void shouldFailGetChildWhenTypeIsWrong() throws Exception {
        final String name = "blah";
        this.vdb.addDataRole( getTransaction(), name );
        this.vdb.getChild( getTransaction(), name, "bogusType" );
    }

    @Test( expected = KException.class )
    public void shouldFailWhenChildNotFound() throws Exception {
        this.vdb.getChild( getTransaction(), "bogus" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailRenameWhenNewNameIsEmpty() throws Exception {
        this.vdb.rename( getTransaction(), EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailRenameWhenNewNameIsNull() throws Exception {
        this.vdb.rename( getTransaction(), null );
    }

    @Test
    public void shouldHaveCorrectChildTypes() {
        assertThat( Arrays.asList( this.vdb.getChildTypes() ),
                    hasItems( DataRole.IDENTIFIER,
                              Entry.IDENTIFIER,
                              Model.IDENTIFIER,
                              Translator.IDENTIFIER,
                              VdbImport.IDENTIFIER ) );
        assertThat(this.vdb.getChildTypes().length, is(5));
    }

    @Test
    public void shouldHaveCorrectName() throws Exception {
        assertThat( this.vdb.getName( getTransaction() ), is( VDB_NAME ) );
    }

    @Test
    public void shouldHaveCorrectOriginalFilePathAfterConstruction() throws Exception {
        assertThat( this.vdb.getOriginalFilePath( getTransaction() ), is( PATH ) );
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.vdb.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.Vdb.VIRTUAL_DATABASE ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.vdb.getTypeIdentifier( getTransaction() ), is(KomodoType.VDB));
    }

    @Test
    public void shouldHaveDefaultPreviewValueAfterConstruction() throws Exception {
        assertThat( this.vdb.isPreview( getTransaction() ), is( Vdb.DEFAULT_PREVIEW ) );
    }

    @Test
    public void shouldHaveDefaultVersionAfterConstruction() throws Exception {
        assertThat( this.vdb.getVersion( getTransaction() ), is( Vdb.DEFAULT_VERSION ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.vdb.getPropertyNames( getTransaction() );
        final String[] rawProps = this.vdb.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveStrongTypedChildren() throws Exception {
        this.vdb.addDataRole( getTransaction(), "dataRole" );
        this.vdb.addEntry( getTransaction(), "entry", "path" );
        this.vdb.addImport( getTransaction(), "vdbImport" );
        this.vdb.addModel( getTransaction(), "model" );
        assertThat( this.vdb.getChildren( getTransaction() ).length, is( 4 ) );
        assertThat( this.vdb.getChildren( getTransaction() )[0], is( instanceOf( DataRole.class ) ) );
        assertThat( this.vdb.getChildren( getTransaction() )[1], is( instanceOf( Entry.class ) ) );
        assertThat( this.vdb.getChildren( getTransaction() )[2], is( instanceOf( VdbImport.class ) ) );
        assertThat( this.vdb.getChildren( getTransaction() )[3], is( instanceOf( Model.class ) ) );
    }

    @Test
    public void shouldIncludeSpecialPropertiesInPrimaryTypePropertyDescriptors() throws Exception {
        final PropertyDescriptor[] descriptors = this.vdb.getPrimaryType( getTransaction() ).getPropertyDescriptors( getTransaction() );
        final List< String > specialProps = new ArrayList<>( Arrays.asList( VdbImpl.SpecialProperty.valuesAsTeiidNames() ) );

        // make sure we are returning more than just the special props
        assertThat( descriptors.length > specialProps.size(), is( true ) );

        for ( final PropertyDescriptor descriptor : descriptors ) {
            if ( specialProps.contains( descriptor.getName() ) ) {
                specialProps.remove( descriptor.getName() );
            }
        }

        // make sure we found all the special props
        assertThat( specialProps.isEmpty(), is( true ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddEmptyDataRole() throws Exception {
        this.vdb.addDataRole( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddEmptyEntry() throws Exception {
        this.vdb.addEntry( getTransaction(), StringConstants.EMPTY_STRING, "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddEmptyImport() throws Exception {
        this.vdb.addImport( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddEmptyModel() throws Exception {
        this.vdb.addModel( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddEmptyTranslator() throws Exception {
        this.vdb.addTranslator( getTransaction(), StringConstants.EMPTY_STRING, "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddNullDataRole() throws Exception {
        this.vdb.addDataRole( getTransaction(), null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddNullEntry() throws Exception {
        this.vdb.addEntry( getTransaction(), null, "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddNullImport() throws Exception {
        this.vdb.addImport( getTransaction(), null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddNullModel() throws Exception {
        this.vdb.addModel( getTransaction(), null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddNullTranslator() throws Exception {
        this.vdb.addTranslator( getTransaction(), null, "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToSetEmptyOriginalFilePath() throws Exception {
        this.vdb.setOriginalFilePath( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToSetNullOriginalFilePath() throws Exception {
        this.vdb.setOriginalFilePath( getTransaction(), null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.vdb.getPropertyNames( getTransaction() );
        final Filter[] filters = this.vdb.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotHaveConnectionTypeAfterConstruction() throws Exception {
        assertThat( this.vdb.getConnectionType( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldNotHaveDataRolesAfterConstruction() throws Exception {
        assertThat( this.vdb.getDataRoles( getTransaction() ), is( notNullValue() ) );
        assertThat( this.vdb.getDataRoles( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveEntriesAfterConstruction() throws Exception {
        assertThat( this.vdb.getEntries( getTransaction() ), is( notNullValue() ) );
        assertThat( this.vdb.getEntries( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveModelsAfterConstruction() throws Exception {
        assertThat( this.vdb.getModels( getTransaction() ), is( notNullValue() ) );
        assertThat( this.vdb.getModels( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveTranslatorsAfterConstruction() throws Exception {
        assertThat( this.vdb.getTranslators( getTransaction() ), is( notNullValue() ) );
        assertThat( this.vdb.getTranslators( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveVdbImportsAfterConstruction() throws Exception {
        assertThat( this.vdb.getImports( getTransaction() ), is( notNullValue() ) );
        assertThat( this.vdb.getImports( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveAllowedLanguages() throws Exception {
        final String newValue = "newAllowedLanguages";
        this.vdb.setAllowedLanguages( getTransaction(), newValue );
        this.vdb.setAllowedLanguages( getTransaction(), null );
        assertThat( this.vdb.getAllowedLanguages( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveAuthenticationType() throws Exception {
        final String newValue = "newAuthenticationType";
        this.vdb.setAuthenticationType( getTransaction(), newValue );
        this.vdb.setAuthenticationType( getTransaction(), null );
        assertThat( this.vdb.getAuthenticationType( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveDataRole() throws Exception {
        final String name = "dataRole";
        this.vdb.addDataRole( getTransaction(), name );
        assertThat( this.vdb.getDataRoles( getTransaction() ).length, is( 1 ) );

        this.vdb.removeDataRole( getTransaction(), name );
        assertThat( this.vdb.getDataRoles( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveEntry() throws Exception {
        final String name = "entry";
        this.vdb.addEntry( getTransaction(), name, "path" );
        assertThat( this.vdb.getEntries( getTransaction() ).length, is( 1 ) );

        this.vdb.removeEntry( getTransaction(), name );
        assertThat( this.vdb.getEntries( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveGssPattern() throws Exception {
        final String newValue = "newGssPattern";
        this.vdb.setGssPattern( getTransaction(), newValue );
        this.vdb.setGssPattern( getTransaction(), null );
        assertThat( this.vdb.getGssPattern( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveModel() throws Exception {
        final String name = "model";
        this.vdb.addModel( getTransaction(), name );
        assertThat( this.vdb.getModels( getTransaction() ).length, is( 1 ) );

        this.vdb.removeModel( getTransaction(), name );
        assertThat( this.vdb.getModels( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldRemovePasswordPattern() throws Exception {
        final String newValue = "newPasswordPattern";
        this.vdb.setPasswordPattern( getTransaction(), newValue );
        this.vdb.setPasswordPattern( getTransaction(), null );
        assertThat( this.vdb.getPasswordPattern( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveQueryTimeout() throws Exception {
        final int newValue = 10;
        this.vdb.setQueryTimeout( getTransaction(), newValue );
        this.vdb.setQueryTimeout( getTransaction(), -100 );
        assertThat( this.vdb.getQueryTimeout( getTransaction() ), is( -1 ) );
    }

    @Test
    public void shouldRemoveSecurityDomain() throws Exception {
        final String newValue = "newSecurityDomain";
        this.vdb.setSecurityDomain( getTransaction(), newValue );
        this.vdb.setSecurityDomain( getTransaction(), null );
        assertThat( this.vdb.getSecurityDomain( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveTranslator() throws Exception {
        final String name = "translator";
        this.vdb.addTranslator( getTransaction(), name, "oracle" );
        assertThat( this.vdb.getTranslators( getTransaction() ).length, is( 1 ) );

        this.vdb.removeTranslator( getTransaction(), name );
        assertThat( this.vdb.getTranslators( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveVdbImport() throws Exception {
        final String name = "vdbImport";
        this.vdb.addImport( getTransaction(), name );
        assertThat( this.vdb.getImports( getTransaction() ).length, is( 1 ) );

        this.vdb.removeImport( getTransaction(), name );
        assertThat( this.vdb.getImports( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "newVdbName";
        this.vdb.rename( getTransaction(), newName );
        assertThat( this.vdb.getName( getTransaction() ), is( newName ) );
        assertThat( this.vdb.getVdbName( getTransaction() ), is( newName ) );
    }

    @Test
    public void shouldRoundTripVdb() throws Exception {
        final InputStream vdbStream = getClass().getClassLoader().getResourceAsStream("AzureService-vdb.xml");
        assertThat( vdbStream, is( notNullValue() ) );

        final String name = "AzureService";
        final VdbImporter importer = new VdbImporter( _repo );
        final ImportOptions importOptions = new ImportOptions();
        importOptions.setOption( OptionKeys.NAME, name );
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        importer.importVdb( getTransaction(), vdbStream, workspace, importOptions, new ImportMessages() );

        commit(); // commit the import

        final Vdb[] vdbs = WorkspaceManager.getInstance( _repo, getTransaction() ).findVdbs( getTransaction() );
        assertThat( vdbs.length, is( 2 ) );

        // find the imported VDB
        Vdb importedVdb = null;

        if ( name.equals( vdbs[ 0 ].getName( getTransaction() ) ) ) {
            importedVdb = vdbs[ 0 ];
        } else if ( name.equals( vdbs[ 1 ].getName( getTransaction() ) ) ) {
            importedVdb = vdbs[ 1 ];
        } else {
            fail();
        }

        final Vdb.VdbManifest manifest = importedVdb.createManifest( getTransaction(), null );
        final Document doc = manifest.asDocument();
        final NodeList kids = doc.getChildNodes();
        assertThat( kids.getLength(), is( 1 ) );

        final Node vdbNode = kids.item( 0 );
        assertThat( vdbNode.getAttributes().getNamedItem( VdbLexicon.ManifestIds.NAME ).getNodeValue(), is( "AzureService" ) );
        assertThat( vdbNode.getAttributes().getNamedItem( VdbLexicon.ManifestIds.VERSION ).getNodeValue(), is( "1" ) );

        if ( vdbNode.getNodeType() != Node.ELEMENT_NODE ) {
            fail( "vdbNode is not an XML element" );
        }

        final Element vdbElement = ( Element )vdbNode;

        { // description
            final NodeList descriptionNodes = vdbElement.getElementsByTagName( VdbLexicon.ManifestIds.DESCRIPTION );
            assertThat( descriptionNodes.getLength(), is( 1 ) );
            assertThat( descriptionNodes.item( 0 ).getTextContent(), is( "VDB for: AzureService, Version: 1" ) );
        }

        { // connection type
            final NodeList connectionTypeNodes = vdbElement.getElementsByTagName( VdbLexicon.ManifestIds.CONNECTION_TYPE );
            assertThat( connectionTypeNodes.getLength(), is( 1 ) );
            assertThat( connectionTypeNodes.item( 0 ).getTextContent(), is( "BY_VERSION" ) );
        }

        { // properties
            final NodeList propertyNodes = vdbElement.getElementsByTagName( VdbLexicon.ManifestIds.PROPERTY );
            assertThat( propertyNodes.getLength(), is( 2 ) );

            final Node node1 = propertyNodes.item( 0 );
            final Node node2 = propertyNodes.item( 1 );
            boolean node1Taken = false;
            boolean node2Taken = false;

            { // auto-generate property
                final String autoGenerateProp = "{http://teiid.org/rest}auto-generate";

                if ( autoGenerateProp.equals( node1.getAttributes().getNamedItem( "name" ).getTextContent() ) ) {
                    assertThat( node1.getAttributes().getNamedItem( "value" ).getTextContent(), is( "true" ) );
                    node1Taken = true;
                } else if ( autoGenerateProp.equals( node2.getAttributes().getNamedItem( "name" ).getTextContent() ) ) {
                    assertThat( node2.getAttributes().getNamedItem( "value" ).getTextContent(), is( "true" ) );
                    node2Taken = true;
                } else {
                    fail( "auto-generate property failure" );
                }
            }

            { // data-services-view property
                final String dataServiceViewProp = "data-service-view";

                if ( !node1Taken
                     && dataServiceViewProp.equals( node1.getAttributes().getNamedItem( "name" ).getTextContent() ) ) {
                    assertThat( node1.getAttributes().getNamedItem( "value" ).getTextContent(), is( "SvcView" ) );
                } else if ( !node2Taken
                            && dataServiceViewProp.equals( node2.getAttributes().getNamedItem( "name" ).getTextContent() ) ) {
                    assertThat( node2.getAttributes().getNamedItem( "value" ).getTextContent(), is( "SvcView" ) );
                } else {
                    fail( "data-service-view property failure" );
                }
            }
        }

        { // import VDB
            final NodeList importVdbNodes = vdbElement.getElementsByTagName( VdbLexicon.ManifestIds.IMPORT_VDB );
            assertThat( importVdbNodes.getLength(), is( 1 ) );

            final Node node = importVdbNodes.item( 0 );
            final NamedNodeMap attributes = node.getAttributes();

            { // name
                final String attr = VdbLexicon.ManifestIds.NAME;
                assertThat( attributes.getNamedItem( attr ), is( notNullValue() ) );
                assertThat( attributes.getNamedItem( attr ).getTextContent(), is( "SvcSourceVdb_AzurePricesDS" ) );
            }

            { // version
                final String attr = VdbLexicon.ManifestIds.VERSION;
                assertThat( attributes.getNamedItem( attr ), is( notNullValue() ) );
                assertThat( attributes.getNamedItem( attr ).getTextContent(), is( "1" ) );
            }

            { // import-data-policies
                final String attr = VdbLexicon.ManifestIds.IMPORT_DATA_POLICIES;
                assertThat( attributes.getNamedItem( attr ), is( notNullValue() ) );
                assertThat( attributes.getNamedItem( attr ).getTextContent(), is( "true" ) );
            }
        }

        { // model
            final NodeList modelNodes = vdbElement.getElementsByTagName( VdbLexicon.ManifestIds.MODEL );
            assertThat( modelNodes.getLength(), is( 1 ) );

            final Node modelNode = modelNodes.item( 0 );
            final NamedNodeMap attributes = modelNode.getAttributes();

            if ( modelNode.getNodeType() != Node.ELEMENT_NODE ) {
                fail( "modelNode is not an XML element" );
            }

            final Element modelElement = ( Element )modelNode;

            { // name
                final String attr = VdbLexicon.ManifestIds.NAME;
                assertThat( attributes.getNamedItem( attr ), is( notNullValue() ) );
                assertThat( attributes.getNamedItem( attr ).getTextContent(), is( "AzureService" ) );
            }

            { // type
                final String attr = VdbLexicon.ManifestIds.TYPE;
                assertThat( attributes.getNamedItem( attr ), is( notNullValue() ) );
                assertThat( attributes.getNamedItem( attr ).getTextContent(), is( "VIRTUAL" ) );
            }
//
//            { // visible
//                final String attr = VdbLexicon.ManifestIds.VISIBLE;
//                assertThat( attributes.getNamedItem( attr ), is( notNullValue() ) );
//                assertThat( attributes.getNamedItem( attr ).getTextContent(), is( "true" ) );
//            }

            { // metadata
                final NodeList metaDataNodes = modelElement.getElementsByTagName( VdbLexicon.ManifestIds.METADATA );
                assertThat( metaDataNodes.getLength(), is( 1 ) );

                final Node metaDataNode = metaDataNodes.item( 0 );

                { // type
                    final String attr = VdbLexicon.ManifestIds.TYPE;
                    assertThat( metaDataNode.getAttributes().getNamedItem( attr ), is( notNullValue() ) );
                    assertThat( metaDataNode.getAttributes().getNamedItem( attr ).getTextContent(), is( "DDL" ) );
                }

                { // metadata
                    final String ddl = metaDataNode.getTextContent();
                    final String expected = "CREATE VIEW SvcView ( RowId integer, ProdCode string, SalePrice bigdecimal, PRIMARY KEY(RowId) ) AS SELECT ROW_NUMBER() OVER (ORDER BY ProdCode), ProdCode, SalePrice FROM Prices.dbo.PricesTable; SET NAMESPACE 'http://teiid.org/rest' AS REST; CREATE VIRTUAL PROCEDURE RestProc() RETURNS TABLE (result xml) OPTIONS (\"REST:URI\" 'rest', \"REST:METHOD\" 'GET') AS BEGIN SELECT XMLELEMENT(NAME Elems, XMLAGG(XMLELEMENT(NAME Elem, XMLFOREST(RowId, ProdCode, SalePrice)))) AS result FROM SvcView; END;";
                    assertThat( ddl, is( expected ) );

                    // since the actual export will have the CDATA marker make sure by actually doing an export here
                    byte[] exportBytes = importedVdb.export( getTransaction(), null );
                    String export = new String(exportBytes);
                    assertThat( export.contains( "<![CDATA[" ), is( true ) );
                    assertThat( export.contains( "]]>" ), is( true ) );
                }
            }
        }
    }

    @Test
    public void shouldSetAllowedLanguages() throws Exception {
        final String newValue = "newAllowedLanguages";
        this.vdb.setAllowedLanguages( getTransaction(), newValue );
        assertThat( this.vdb.getAllowedLanguages( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetAuthenticationType() throws Exception {
        final String newValue = "newAuthenticationType";
        this.vdb.setAuthenticationType( getTransaction(), newValue );
        assertThat( this.vdb.getAuthenticationType( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetConnectionType() throws Exception {
        final String newValue = "newConnectionType";
        this.vdb.setConnectionType( getTransaction(), newValue );
        assertThat( this.vdb.getConnectionType( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String newValue = "newDescription";
        this.vdb.setDescription( getTransaction(), newValue );
        assertThat( this.vdb.getDescription( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetGssPattern() throws Exception {
        final String newValue = "newGssPattern";
        this.vdb.setGssPattern( getTransaction(), newValue );
        assertThat( this.vdb.getGssPattern( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetOriginalFilePath() throws Exception {
        final String newValue = "newOriginalFilePath";
        this.vdb.setOriginalFilePath( getTransaction(), newValue );
        assertThat( this.vdb.getOriginalFilePath( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetPasswordPattern() throws Exception {
        final String newValue = "newPasswordPattern";
        this.vdb.setPasswordPattern( getTransaction(), newValue );
        assertThat( this.vdb.getPasswordPattern( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetPreviewValue() throws Exception {
        final boolean newValue = !Vdb.DEFAULT_PREVIEW;
        this.vdb.setPreview( getTransaction(), newValue );
        assertThat( this.vdb.isPreview( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetPreviewValueWithStringValue() throws Exception {
        this.vdb.setProperty( getTransaction(), VdbLexicon.Vdb.PREVIEW, "blah" );
        assertThat( this.vdb.isPreview( getTransaction() ), is( false ) );
    }

    @Test
    public void shouldSetQueryTimeout() throws Exception {
        final int newValue = 10;
        this.vdb.setQueryTimeout( getTransaction(), newValue );
        assertThat( this.vdb.getQueryTimeout( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetSecurityDomain() throws Exception {
        final String newValue = "newSecurityDomain";
        this.vdb.setSecurityDomain( getTransaction(), newValue );
        assertThat( this.vdb.getSecurityDomain( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetVdbName() throws Exception {
        final String newValue = "newName";
        this.vdb.setVdbName( getTransaction(), newValue );
        assertThat( this.vdb.getVdbName( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetVersion() throws Exception {
        final int newValue = ( Vdb.DEFAULT_VERSION + 10 );
        this.vdb.setVersion( getTransaction(), newValue );
        assertThat( this.vdb.getVersion( getTransaction() ), is( newValue ) );
    }

}
