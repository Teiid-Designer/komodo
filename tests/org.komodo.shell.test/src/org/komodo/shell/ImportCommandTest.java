package org.komodo.shell;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

import org.junit.Test;
import org.komodo.relational.model.Model;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.commands.core.ImportCommand;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * Test the VDB import functionality
 *
 */
@SuppressWarnings( {"nls", "javadoc"} )
public class ImportCommandTest extends AbstractCommandTest {

	private static final String IMPORT_VDB1 = "importVdb1.txt"; 
	private static final String IMPORT_VDB2 = "importVdb2.txt"; 
	private static final String IMPORT_VDB3 = "importVdb3.txt"; 
	private static final String IMPORT_VDB4 = "importVdb4.txt"; 
	private static final String IMPORT_VDB5 = "importVdb5.txt"; 

	/**
	 * Test for CreateCommand
	 */
	public ImportCommandTest( ) {
		super();
	}

    /**
     * Import VDB1 - AzureService-vdb.xml
     * @throws Exception
     */
    @Test
    public void testImportVdb1() throws Exception {
    	setup(IMPORT_VDB1, ImportCommand.class);

    	execute();

    	assertEquals("/workspace", wsStatus.getCurrentContext().getFullName());

    	WorkspaceContext vdbContext = ContextUtils.getContextForPath(wsStatus, "/workspace/AzureService");
    	assertNotNull(vdbContext);

    	KomodoObject ko = vdbContext.getKomodoObj();
    	UnitOfWork trans = wsStatus.getTransaction();

    	// Verify the komodo class is a Vdb and is VDB type
    	assertEquals(KomodoType.VDB.name(), ko.getTypeIdentifier(trans).name());

    	Vdb vdb = (Vdb)resolveType(trans, ko, Vdb.class);

        // Check VDB version
        assertThat(vdb.getVersion(trans), is(1));

    	// Check VDB description
    	String description = vdb.getDescription(trans);
    	assertEquals("VDB for: AzureService, Version: 1", description);

    	// Check VDB connectionType property
    	Property prop = vdb.getProperty(trans, "vdb:connectionType");
    	String connectionValue = prop.getStringValue(trans);
        assertEquals("BY_VERSION", connectionValue);

    	// Check VDB data-service-view property
    	prop = vdb.getProperty(trans, "data-service-view");
    	String dsViewValue = prop.getStringValue(trans);
        assertEquals("SvcView", dsViewValue);

    	// Check VDB auto-generate property
    	prop = vdb.getProperty(trans, "{http://teiid.org/rest}auto-generate");
    	boolean autoGenValue = prop.getBooleanValue(trans);
        assertEquals(true, autoGenValue);

        { // Check import VDB
            final VdbImport[] vdbImports = vdb.getImports(trans);
            assertThat(vdbImports.length, is(1));
            assertThat(vdbImports[0].getName(trans), is("SvcSourceVdb_AzurePricesDS"));
            assertThat(vdbImports[0].getVersion(trans), is(1));
            assertThat(vdbImports[0].isImportDataPolicies(trans), is(true));
        }

        { // Check model
            final Model[] models = vdb.getModels(trans);
            assertThat(models.length, is(1));
            assertThat(models[0].getName(trans), is("AzureService"));
            assertThat(models[0].getModelType(trans), is(Model.Type.VIRTUAL));
            assertThat(models[0].isVisible( trans ), is(true));
            assertThat(models[0].getMetadataType( trans ), is("DDL"));
            assertThat(models[0].getModelDefinition(trans).startsWith("CREATE VIEW SvcView"), is(true));
            assertThat(models[0].getModelDefinition(trans).endsWith("END;"), is(true));
        }
    }

    /**
     * Import VDB2 - myService-vdb.xml
     * @throws Exception
     */
    @Test
    public void testImportVdb2() throws Exception {
    	setup(IMPORT_VDB2, ImportCommand.class);

    	execute();

    	assertEquals("/workspace", wsStatus.getCurrentContext().getFullName()); 

    	WorkspaceContext vdbContext = ContextUtils.getContextForPath(wsStatus, "/workspace/myService"); 
    	assertNotNull(vdbContext);

    	KomodoObject ko = vdbContext.getKomodoObj();
    	UnitOfWork trans = wsStatus.getTransaction();
    	
    	// Verify the komodo class is a Vdb and is VDB type
    	assertEquals(KomodoType.VDB.name(), ko.getTypeIdentifier(trans).name());

    	Vdb vdb = (Vdb)resolveType(trans, ko, Vdb.class);
    	
        // Check VDB version
        assertThat(vdb.getVersion(trans), is(1));

    	// Check VDB description
    	String description = vdb.getDescription(trans);
    	assertEquals("VDB for: myService, Version: 1", description); 

    	// Check VDB connectionType property
    	Property prop = vdb.getProperty(trans, "vdb:connectionType");
    	String connectionValue = prop.getStringValue(trans);
        assertEquals("BY_VERSION", connectionValue);

    	// Check VDB data-service-view property
    	prop = vdb.getProperty(trans, "data-service-view");
    	String dsViewValue = prop.getStringValue(trans);
        assertEquals("SvcView", dsViewValue);

    	// Check VDB auto-generate property
    	prop = vdb.getProperty(trans, "{http://teiid.org/rest}auto-generate");
    	boolean autoGenValue = prop.getBooleanValue(trans);
        assertEquals(true, autoGenValue);
    	
		{ // Check import VDB
			final VdbImport[] vdbImports = vdb.getImports(trans);
			assertThat(vdbImports.length, is(2));
			assertThat(vdbImports[0].getName(trans), is("SvcSourceVdb_AzurePricesDS"));
			assertThat(vdbImports[0].getVersion(trans), is(1));
			assertThat(vdbImports[0].isImportDataPolicies(trans), is(true));
			assertThat(vdbImports[1].getName(trans), is("SvcSourceVdb_SalesforceDS"));
			assertThat(vdbImports[1].getVersion(trans), is(1));
			assertThat(vdbImports[1].isImportDataPolicies(trans), is(true));
		}

		{ // Check model
			final Model[] models = vdb.getModels(trans);
			assertThat(models.length, is(1));
			assertThat(models[0].getName(trans), is("myService"));
			assertThat(models[0].getModelType(trans), is(Model.Type.VIRTUAL));
			assertThat(models[0].isVisible(trans), is(true));
			assertThat(models[0].getMetadataType(trans), is("DDL"));
			assertThat(models[0].getModelDefinition(trans).startsWith("CREATE VIEW SvcView"), is(true));
			assertThat(models[0].getModelDefinition(trans).endsWith("END;"), is(true));
		}
    }

    /**
     * Import VDB3 - SvcSourceVdb_ODataNorthwind-vdb.xml
     * @throws Exception
     */
    @Test
    public void testImportVdb3() throws Exception {
    	setup(IMPORT_VDB3, ImportCommand.class);

    	execute();

    	assertEquals("/workspace", wsStatus.getCurrentContext().getFullName()); 

    	WorkspaceContext vdbContext = ContextUtils.getContextForPath(wsStatus, "/workspace/SvcSourceVdb_ODataNorthwind"); 
    	assertNotNull(vdbContext);

    	KomodoObject ko = vdbContext.getKomodoObj();
    	UnitOfWork trans = wsStatus.getTransaction();
    	
    	// Verify the komodo class is a Vdb and is VDB type
    	assertEquals(KomodoType.VDB.name(), ko.getTypeIdentifier(trans).name());

    	Vdb vdb = (Vdb)resolveType(trans, ko, Vdb.class);
    	
        // Check VDB version
        assertThat(vdb.getVersion(trans), is(1));

    	// Check VDB description
    	String description = vdb.getDescription(trans);
    	assertEquals("VDB for: SvcSourceVdb_ODataNorthwind, Version: 1", description); 

    	// Check VDB connectionType property
    	Property prop = vdb.getProperty(trans, "vdb:connectionType");
    	String connectionValue = prop.getStringValue(trans);
        assertEquals("BY_VERSION", connectionValue);

		{ // Check model
			final Model[] models = vdb.getModels(trans);
			assertThat(models.length, is(1));
			
			final Model theModel = models[0];
			assertThat(theModel.getName(trans), is("ODataNorthwind"));
			assertThat(theModel.getModelType(trans), is(Model.Type.PHYSICAL));
			
			// Check model source
			ModelSource[] modelSources = theModel.getSources(trans);
			assertThat(modelSources.length, is(1));
			ModelSource theModelSource = modelSources[0];
			assertThat(theModelSource.getTranslatorName(trans), is("odata"));
			assertThat(theModelSource.getJndiName(trans), is("java:/ODataNorthwind"));
		}
    }

    /**
     * Import VDB4 - SvcSourceVdb_SalesforceDS-vdb.xml
     * @throws Exception
     */
    @Test
    public void testImportVdb4() throws Exception {
    	setup(IMPORT_VDB4, ImportCommand.class);

    	execute();

    	assertEquals("/workspace", wsStatus.getCurrentContext().getFullName()); 

    	WorkspaceContext vdbContext = ContextUtils.getContextForPath(wsStatus, "/workspace/SvcSourceVdb_SalesforceDS"); 
    	assertNotNull(vdbContext);

    	KomodoObject ko = vdbContext.getKomodoObj();
    	UnitOfWork trans = wsStatus.getTransaction();
    	
    	// Verify the komodo class is a Vdb and is VDB type
    	assertEquals(KomodoType.VDB.name(), ko.getTypeIdentifier(trans).name());

    	Vdb vdb = (Vdb)resolveType(trans, ko, Vdb.class);
    	
        // Check VDB version
        assertThat(vdb.getVersion(trans), is(1));

    	// Check VDB description
    	String description = vdb.getDescription(trans);
    	assertEquals("VDB for: SvcSourceVdb_SalesforceDS, Version: 1", description); 

    	// Check VDB connectionType property
    	Property prop = vdb.getProperty(trans, "vdb:connectionType");
    	String connectionValue = prop.getStringValue(trans);
        assertEquals("BY_VERSION", connectionValue);

		{ // Check model
			final Model[] models = vdb.getModels(trans);
			assertThat(models.length, is(1));
			
			final Model theModel = models[0];
			assertThat(theModel.getName(trans), is("SalesforceDS"));
			assertThat(theModel.getModelType(trans), is(Model.Type.PHYSICAL));
			
			// Check model source
			ModelSource[] modelSources = theModel.getSources(trans);
			assertThat(modelSources.length, is(1));
			ModelSource theModelSource = modelSources[0];
			assertThat(theModelSource.getTranslatorName(trans), is("salesforce"));
			assertThat(theModelSource.getJndiName(trans), is("java:/SalesforceDS"));
		}
    }

    /**
     * Import VDB5 - s-vdb.xml
     * @throws Exception
     */
    @Test
    public void testImportVdb5() throws Exception {
    	setup(IMPORT_VDB5, ImportCommand.class);

//    	execute();
//
//    	assertEquals("/workspace", wsStatus.getCurrentContext().getFullName()); 
//
//    	WorkspaceContext vdbContext = ContextUtils.getContextForPath(wsStatus, "/workspace/tpch"); 
//    	assertNotNull(vdbContext);
//
//    	KomodoObject ko = vdbContext.getKomodoObj();
//    	UnitOfWork trans = wsStatus.getTransaction();
//    	
//    	// Verify the komodo class is a Vdb and is VDB type
//    	assertEquals(KomodoType.VDB.name(), ko.getTypeIdentifier(trans).name());
//
//    	Vdb vdb = (Vdb)resolveType(trans, ko, Vdb.class);
//    	
//        // Check VDB version
//        assertThat(vdb.getVersion(trans), is(4));
//
//    	// Check VDB connectionType property
//    	Property prop = vdb.getProperty(trans, "vdb:connectionType");
//    	String connectionValue = prop.getStringValue(trans);
//        assertEquals("BY_VERSION", connectionValue);
//
//		{ // Check model
//			final Model[] models = vdb.getModels(trans);
//			assertThat(models.length, is(1));
//			
//			final Model theModel = models[0];
//			assertThat(theModel.getName(trans), is("SalesforceDS"));
//			assertThat(theModel.getModelType(trans), is(Model.Type.PHYSICAL));
//			
//			// Check model source
//			ModelSource[] modelSources = theModel.getSources(trans);
//			assertThat(modelSources.length, is(1));
//			ModelSource theModelSource = modelSources[0];
//			assertThat(theModelSource.getTranslatorName(trans), is("salesforce"));
//			assertThat(theModelSource.getJndiName(trans), is("java:/SalesforceDS"));
//		}
    }
}
