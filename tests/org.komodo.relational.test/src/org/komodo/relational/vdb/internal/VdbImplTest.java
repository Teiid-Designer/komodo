/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.vdb.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsCollectionContaining.hasItems;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import java.io.File;
import java.io.FileInputStream;
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
import org.komodo.importer.vdb.VdbImporter;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.RelationalProperty;
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
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;
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
        final DataRole dataRole = this.vdb.addDataRole( this.uow, name );
        assertThat( dataRole, is( notNullValue() ) );
        assertThat( this.vdb.getDataRoles( this.uow ).length, is( 1 ) );

        final DataRole added = this.vdb.getDataRoles( this.uow )[0];
        assertThat( added, is( dataRole ) );
        assertThat( added.getName( this.uow ), is( name ) );
        assertThat( added.getPrimaryType( this.uow ).getName(), is( VdbLexicon.DataRole.DATA_ROLE ) );
        assertThat( this.vdb.getChildren( this.uow )[0], is( instanceOf( DataRole.class ) ) );
    }

    @Test
    public void shouldAddEntry() throws Exception {
        final String name = "entry";
        final String path = "/my/path";
        final Entry entry = this.vdb.addEntry( this.uow, name, path );
        assertThat( entry, is( notNullValue() ) );
        assertThat( this.vdb.getEntries( this.uow ).length, is( 1 ) );

        final Entry added = this.vdb.getEntries( this.uow )[0];
        assertThat( added, is( entry ) );
        assertThat( added.getName( this.uow ), is( name ) );
        assertThat( added.getPrimaryType( this.uow ).getName(), is( VdbLexicon.Entry.ENTRY ) );
        assertThat( added.getPath( this.uow ), is( path ) );
        assertThat( this.vdb.getChildren( this.uow )[0], is( instanceOf( Entry.class ) ) );
    }

    @Test
    public void shouldAddImport() throws Exception {
        final String name = "vdbImport";
        final VdbImport vdbImport = this.vdb.addImport( this.uow, name );
        assertThat( vdbImport, is( notNullValue() ) );
        assertThat( this.vdb.getImports( this.uow ).length, is( 1 ) );

        final VdbImport added = this.vdb.getImports( this.uow )[0];
        assertThat( added, is( vdbImport ) );
        assertThat( added.getName( this.uow ), is( name ) );
        assertThat( added.getPrimaryType( this.uow ).getName(), is( VdbLexicon.ImportVdb.IMPORT_VDB ) );
        assertThat( this.vdb.getChildren( this.uow )[0], is( instanceOf( VdbImport.class ) ) );
    }

    @Test
    public void shouldAddModel() throws Exception {
        final String name = "model";
        final Model model = this.vdb.addModel( this.uow, name );
        assertThat( model, is( notNullValue() ) );
        assertThat( this.vdb.getModels( this.uow ).length, is( 1 ) );

        final Model added = this.vdb.getModels( this.uow )[0];
        assertThat( added, is( model ) );
        assertThat( added.getName( this.uow ), is( name ) );
        assertThat( added.getPrimaryType( this.uow ).getName(), is( VdbLexicon.Vdb.DECLARATIVE_MODEL ) );
        assertThat( this.vdb.getChildren( this.uow )[0], is( instanceOf( Model.class ) ) );
    }

    @Test
    public void shouldAddTranslator() throws Exception {
        final String name = "translator";
        final String type = "oracle";
        final Translator translator = this.vdb.addTranslator( this.uow, name, type );
        assertThat( translator, is( notNullValue() ) );
        assertThat( this.vdb.getTranslators( this.uow ).length, is( 1 ) );

        final Translator added = this.vdb.getTranslators( this.uow )[0];
        assertThat( added, is( translator ) );
        assertThat( added.getName( this.uow ), is( name ) );
        assertThat( added.getPrimaryType( this.uow ).getName(), is( VdbLexicon.Translator.TRANSLATOR ) );
        assertThat( added.getType( this.uow ), is( type ) );
        assertThat( added, is( instanceOf( Translator.class ) ) );
    }

    @Test
    public void shouldCreateManifestForEmptyVdb() throws Exception {
        final VdbManifest manifest = this.vdb.createManifest( this.uow, new Properties() );
        assertThat( manifest, is( notNullValue() ) );
        assertThat( manifest.asDocument(), is( notNullValue() ) );
    }

    @Test
    public void shouldCreateManifestForVdb() throws Exception {
        { // setup
            this.vdb.setVdbName( this.uow, "twitter" );
            this.vdb.setVersion( this.uow, 1 );
            this.vdb.setDescription( this.uow, "Shows how to call Web Services" );
            this.vdb.setProperty( this.uow, "UseConnectorMetadata", "cached" );

            final Model twitter = this.vdb.addModel( this.uow, "twitter" );
            twitter.setModelType( this.uow, Model.Type.PHYSICAL );

            final Model twitterview = this.vdb.addModel( this.uow, "twitterview" );
            twitterview.setModelType( this.uow, Model.Type.VIRTUAL );

            final Translator translator = this.vdb.addTranslator( this.uow, "rest", "ws" );
            translator.setProperty( this.uow, "DefaultBinding", "HTTP" );
        }

        final VdbManifest manifest = this.vdb.createManifest( this.uow, new Properties() );
        assertThat( manifest, is( notNullValue() ) );
        assertThat( manifest.asDocument(), is( notNullValue() ) );
    }

    @Test
    public void shouldExportEmptyVdb() throws Exception {
        final String manifest = this.vdb.export( this.uow, new Properties() );
        assertThat( manifest, is( notNullValue() ) );
        assertThat( manifest.isEmpty(), is( false ) );
    }

    @Test
    public void shouldExportVdb() throws Exception {
        { // setup
            this.vdb.setVdbName( this.uow, "twitter" );
            this.vdb.setVersion( this.uow, 1 );
            this.vdb.setDescription( this.uow, "Shows how to call Web Services" );
            this.vdb.setProperty( this.uow, "UseConnectorMetadata", "cached" );

            final Model twitter = this.vdb.addModel( this.uow, "twitter" );
            twitter.setModelType( this.uow, Model.Type.PHYSICAL );

            final Model twitterview = this.vdb.addModel( this.uow, "twitterview" );
            twitterview.setModelType( this.uow, Model.Type.VIRTUAL );

            final Translator translator = this.vdb.addTranslator( this.uow, "rest", "ws" );
            translator.setProperty( this.uow, "DefaultBinding", "HTTP" );
        }

        // test
        final String manifest = this.vdb.export( this.uow, new Properties() );
        assertThat( manifest, is( notNullValue() ) );
        assertThat( manifest.isEmpty(), is( false ) );
    }

    @Test
    public void shouldFailConstructionIfNotVdb() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new VdbImpl( this.uow, _repo, _repo.komodoLibrary( this.uow ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailRenameWhenNewNameIsEmpty() throws Exception {
        this.vdb.rename( this.uow, EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailRenameWhenNewNameIsNull() throws Exception {
        this.vdb.rename( this.uow, null );
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
        assertThat( this.vdb.getName( this.uow ), is( VDB_NAME ) );
    }

    @Test
    public void shouldHaveCorrectOriginalFilePathAfterConstruction() throws Exception {
        assertThat( this.vdb.getOriginalFilePath( this.uow ), is( PATH ) );
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.vdb.getPrimaryType( this.uow ).getName(), is( VdbLexicon.Vdb.VIRTUAL_DATABASE ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.vdb.getTypeIdentifier( this.uow ), is(KomodoType.VDB));
    }

    @Test
    public void shouldHaveDefaultPreviewValueAfterConstruction() throws Exception {
        assertThat( this.vdb.isPreview( this.uow ), is( Vdb.DEFAULT_PREVIEW ) );
    }

    @Test
    public void shouldHaveDefaultVersionAfterConstruction() throws Exception {
        assertThat( this.vdb.getVersion( this.uow ), is( Vdb.DEFAULT_VERSION ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.vdb.getPropertyNames( this.uow );
        final String[] rawProps = this.vdb.getRawPropertyNames( this.uow );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveStrongTypedChildren() throws Exception {
        this.vdb.addDataRole( this.uow, "dataRole" );
        this.vdb.addEntry( this.uow, "entry", "path" );
        this.vdb.addImport( this.uow, "vdbImport" );
        this.vdb.addModel( this.uow, "model" );
        assertThat( this.vdb.getChildren( this.uow ).length, is( 4 ) );
        assertThat( this.vdb.getChildren( this.uow )[0], is( instanceOf( DataRole.class ) ) );
        assertThat( this.vdb.getChildren( this.uow )[1], is( instanceOf( Entry.class ) ) );
        assertThat( this.vdb.getChildren( this.uow )[2], is( instanceOf( VdbImport.class ) ) );
        assertThat( this.vdb.getChildren( this.uow )[3], is( instanceOf( Model.class ) ) );
    }

    @Test
    public void shouldIncludeSpecialPropertiesInPrimaryTypePropertyDescriptors() throws Exception {
        final PropertyDescriptor[] descriptors = this.vdb.getPrimaryType( this.uow ).getPropertyDescriptors( this.uow );
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
        this.vdb.addDataRole( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddEmptyEntry() throws Exception {
        this.vdb.addEntry( this.uow, StringConstants.EMPTY_STRING, "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddEmptyImport() throws Exception {
        this.vdb.addImport( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddEmptyModel() throws Exception {
        this.vdb.addModel( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddEmptyTranslator() throws Exception {
        this.vdb.addTranslator( this.uow, StringConstants.EMPTY_STRING, "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddNullDataRole() throws Exception {
        this.vdb.addDataRole( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddNullEntry() throws Exception {
        this.vdb.addEntry( this.uow, null, "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddNullImport() throws Exception {
        this.vdb.addImport( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddNullModel() throws Exception {
        this.vdb.addModel( this.uow, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToAddNullTranslator() throws Exception {
        this.vdb.addTranslator( this.uow, null, "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToSetEmptyOriginalFilePath() throws Exception {
        this.vdb.setOriginalFilePath( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToSetNullOriginalFilePath() throws Exception {
        this.vdb.setOriginalFilePath( this.uow, null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.vdb.getPropertyNames( this.uow );
        final Filter[] filters = this.vdb.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotHaveConnectionTypeAfterConstruction() throws Exception {
        assertThat( this.vdb.getConnectionType( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldNotHaveDataRolesAfterConstruction() throws Exception {
        assertThat( this.vdb.getDataRoles( this.uow ), is( notNullValue() ) );
        assertThat( this.vdb.getDataRoles( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveEntriesAfterConstruction() throws Exception {
        assertThat( this.vdb.getEntries( this.uow ), is( notNullValue() ) );
        assertThat( this.vdb.getEntries( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveModelsAfterConstruction() throws Exception {
        assertThat( this.vdb.getModels( this.uow ), is( notNullValue() ) );
        assertThat( this.vdb.getModels( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveTranslatorsAfterConstruction() throws Exception {
        assertThat( this.vdb.getTranslators( this.uow ), is( notNullValue() ) );
        assertThat( this.vdb.getTranslators( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldNotHaveVdbImportsAfterConstruction() throws Exception {
        assertThat( this.vdb.getImports( this.uow ), is( notNullValue() ) );
        assertThat( this.vdb.getImports( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveAllowedLanguages() throws Exception {
        final String newValue = "newAllowedLanguages";
        this.vdb.setAllowedLanguages( this.uow, newValue );
        this.vdb.setAllowedLanguages( this.uow, null );
        assertThat( this.vdb.getAllowedLanguages( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveAuthenticationType() throws Exception {
        final String newValue = "newAuthenticationType";
        this.vdb.setAuthenticationType( this.uow, newValue );
        this.vdb.setAuthenticationType( this.uow, null );
        assertThat( this.vdb.getAuthenticationType( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveDataRole() throws Exception {
        final String name = "dataRole";
        this.vdb.addDataRole( this.uow, name );
        assertThat( this.vdb.getDataRoles( this.uow ).length, is( 1 ) );

        this.vdb.removeDataRole( this.uow, name );
        assertThat( this.vdb.getDataRoles( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveEntry() throws Exception {
        final String name = "entry";
        this.vdb.addEntry( this.uow, name, "path" );
        assertThat( this.vdb.getEntries( this.uow ).length, is( 1 ) );

        this.vdb.removeEntry( this.uow, name );
        assertThat( this.vdb.getEntries( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveGssPattern() throws Exception {
        final String newValue = "newGssPattern";
        this.vdb.setGssPattern( this.uow, newValue );
        this.vdb.setGssPattern( this.uow, null );
        assertThat( this.vdb.getGssPattern( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveModel() throws Exception {
        final String name = "model";
        this.vdb.addModel( this.uow, name );
        assertThat( this.vdb.getModels( this.uow ).length, is( 1 ) );

        this.vdb.removeModel( this.uow, name );
        assertThat( this.vdb.getModels( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldRemovePasswordPattern() throws Exception {
        final String newValue = "newPasswordPattern";
        this.vdb.setPasswordPattern( this.uow, newValue );
        this.vdb.setPasswordPattern( this.uow, null );
        assertThat( this.vdb.getPasswordPattern( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveQueryTimeout() throws Exception {
        final int newValue = 10;
        this.vdb.setQueryTimeout( this.uow, newValue );
        this.vdb.setQueryTimeout( this.uow, -100 );
        assertThat( this.vdb.getQueryTimeout( this.uow ), is( -1 ) );
    }

    @Test
    public void shouldRemoveSecurityDomain() throws Exception {
        final String newValue = "newSecurityDomain";
        this.vdb.setSecurityDomain( this.uow, newValue );
        this.vdb.setSecurityDomain( this.uow, null );
        assertThat( this.vdb.getSecurityDomain( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldRemoveTranslator() throws Exception {
        final String name = "translator";
        this.vdb.addTranslator( this.uow, name, "oracle" );
        assertThat( this.vdb.getTranslators( this.uow ).length, is( 1 ) );

        this.vdb.removeTranslator( this.uow, name );
        assertThat( this.vdb.getTranslators( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveVdbImport() throws Exception {
        final String name = "vdbImport";
        this.vdb.addImport( this.uow, name );
        assertThat( this.vdb.getImports( this.uow ).length, is( 1 ) );

        this.vdb.removeImport( this.uow, name );
        assertThat( this.vdb.getImports( this.uow ).length, is( 0 ) );
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "newVdbName";
        this.vdb.rename( this.uow, newName );
        assertThat( this.vdb.getName( this.uow ), is( newName ) );
        assertThat( this.vdb.getVdbName( this.uow ), is( newName ) );
    }

    @Test
    public void shouldRoundTripVdb() throws Exception {
        final File vdbFile = new File( "resources/AzureService-vdb.xml" );
        final InputStream vdbStream = new FileInputStream( vdbFile );
        assertThat( vdbStream, is( notNullValue() ) );

        final String name = "AzureService";
        final VdbImporter importer = new VdbImporter( _repo );
        final ImportOptions importOptions = new ImportOptions();
        importOptions.setOption( OptionKeys.NAME, name );
        KomodoObject workspace = _repo.komodoWorkspace(uow);
        importer.importVdb( this.uow, vdbStream, workspace, importOptions, new ImportMessages() );

        commit(); // commit the import

        final Vdb[] vdbs = WorkspaceManager.getInstance( _repo ).findVdbs( this.uow );
        assertThat( vdbs.length, is( 2 ) );

        // find the imported VDB
        Vdb importedVdb = null;

        if ( name.equals( vdbs[ 0 ].getName( this.uow ) ) ) {
            importedVdb = vdbs[ 0 ];
        } else if ( name.equals( vdbs[ 1 ].getName( this.uow ) ) ) {
            importedVdb = vdbs[ 1 ];
        } else {
            fail();
        }

        final Vdb.VdbManifest manifest = importedVdb.createManifest( this.uow, null );
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
                    final String export = importedVdb.export( this.uow, null );
                    assertThat( export.contains( "<![CDATA[" ), is( true ) );
                    assertThat( export.contains( "]]>" ), is( true ) );
                }
            }
        }
    }

    @Test
    public void shouldSetAllowedLanguages() throws Exception {
        final String newValue = "newAllowedLanguages";
        this.vdb.setAllowedLanguages( this.uow, newValue );
        assertThat( this.vdb.getAllowedLanguages( this.uow ), is( newValue ) );
    }

    @Test
    public void shouldSetAuthenticationType() throws Exception {
        final String newValue = "newAuthenticationType";
        this.vdb.setAuthenticationType( this.uow, newValue );
        assertThat( this.vdb.getAuthenticationType( this.uow ), is( newValue ) );
    }

    @Test
    public void shouldSetConnectionType() throws Exception {
        final String newValue = "newConnectionType";
        this.vdb.setConnectionType( this.uow, newValue );
        assertThat( this.vdb.getConnectionType( this.uow ), is( newValue ) );
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String newValue = "newDescription";
        this.vdb.setDescription( this.uow, newValue );
        assertThat( this.vdb.getDescription( this.uow ), is( newValue ) );
    }

    @Test
    public void shouldSetGssPattern() throws Exception {
        final String newValue = "newGssPattern";
        this.vdb.setGssPattern( this.uow, newValue );
        assertThat( this.vdb.getGssPattern( this.uow ), is( newValue ) );
    }

    @Test
    public void shouldSetOriginalFilePath() throws Exception {
        final String newValue = "newOriginalFilePath";
        this.vdb.setOriginalFilePath( this.uow, newValue );
        assertThat( this.vdb.getOriginalFilePath( this.uow ), is( newValue ) );
    }

    @Test
    public void shouldSetPasswordPattern() throws Exception {
        final String newValue = "newPasswordPattern";
        this.vdb.setPasswordPattern( this.uow, newValue );
        assertThat( this.vdb.getPasswordPattern( this.uow ), is( newValue ) );
    }

    @Test
    public void shouldSetPreviewValue() throws Exception {
        final boolean newValue = !Vdb.DEFAULT_PREVIEW;
        this.vdb.setPreview( this.uow, newValue );
        assertThat( this.vdb.isPreview( this.uow ), is( newValue ) );
    }

    @Test
    public void shouldSetPreviewValueWithStringValue() throws Exception {
        this.vdb.setProperty( this.uow, VdbLexicon.Vdb.PREVIEW, "blah" );
        assertThat( this.vdb.isPreview( this.uow ), is( false ) );
    }

    @Test
    public void shouldSetQueryTimeout() throws Exception {
        final int newValue = 10;
        this.vdb.setQueryTimeout( this.uow, newValue );
        assertThat( this.vdb.getQueryTimeout( this.uow ), is( newValue ) );
    }

    @Test
    public void shouldSetSecurityDomain() throws Exception {
        final String newValue = "newSecurityDomain";
        this.vdb.setSecurityDomain( this.uow, newValue );
        assertThat( this.vdb.getSecurityDomain( this.uow ), is( newValue ) );
    }

    @Test
    public void shouldSetVdbName() throws Exception {
        final String newValue = "newName";
        this.vdb.setVdbName( this.uow, newValue );
        assertThat( this.vdb.getVdbName( this.uow ), is( newValue ) );
    }

    @Test
    public void shouldSetVersion() throws Exception {
        final int newValue = ( Vdb.DEFAULT_VERSION + 10 );
        this.vdb.setVersion( this.uow, newValue );
        assertThat( this.vdb.getVersion( this.uow ), is( newValue ) );
    }

    /*
     * ********************************************************************
     * *****                  Resolver Tests                          *****
     * ********************************************************************
     */

    @Test
    public void shouldCreateUsingResolver() throws Exception {
        final String name = "blah";

        final RelationalProperties props = new RelationalProperties();
        props.add( new RelationalProperty( VdbLexicon.Vdb.ORIGINAL_FILE, "/my/path/vdb.vdb" ) );

        final KomodoObject kobject = VdbImpl.RESOLVER.create( this.uow, _repo, null, name, props );
        assertThat( kobject, is( notNullValue() ) );
        assertThat( kobject, is( instanceOf( Vdb.class ) ) );
        assertThat( kobject.getName( this.uow ), is( name ) );
    }

}
