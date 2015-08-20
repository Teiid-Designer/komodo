package org.komodo.shell;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import org.junit.Test;
import org.komodo.importer.commands.ImportCommand;
import org.komodo.relational.model.Model;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.shell.api.WorkspaceContext;
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
	private static final String IMPORT_VDB6 = "importVdb6.txt"; 
	private static final String IMPORT_VDB7 = "importVdb7.txt"; 

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
            //
            // model definition is filtered by default
            //
            models[0].setFilters(null);

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
            //
            // model definition is filtered by default
            //
            models[0].setFilters(null);

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
     * Import VDB5 - importProps-vdb.xml
     * @throws Exception
     */
    @Test
    public void testImportVdb5() throws Exception {
    	setup(IMPORT_VDB5, ImportCommand.class);

    	execute();

    	assertEquals("/workspace", wsStatus.getCurrentContext().getFullName()); 

    	WorkspaceContext vdbContext = ContextUtils.getContextForPath(wsStatus, "/workspace/importProps"); 
    	assertNotNull(vdbContext);

    	KomodoObject ko = vdbContext.getKomodoObj();
    	UnitOfWork trans = wsStatus.getTransaction();
    	
    	// Verify the komodo class is a Vdb and is VDB type
    	assertEquals(KomodoType.VDB.name(), ko.getTypeIdentifier(trans).name());

    	Vdb vdb = (Vdb)resolveType(trans, ko, Vdb.class);
    	
        // Check VDB version
        assertThat(vdb.getVersion(trans), is(4));

    	// Check VDB connectionType property
    	Property prop = vdb.getProperty(trans, "vdb:connectionType");
    	String connectionValue = prop.getStringValue(trans);
        assertEquals("BY_VERSION", connectionValue);

		{ // Check model
			final Model[] models = vdb.getModels(trans);
			assertThat(models.length, is(1));
			
			final Model theModel = models[0];
			assertThat(theModel.getName(trans), is("postgresql"));
			assertThat(theModel.getModelType(trans), is(Model.Type.PHYSICAL));
			
			// Check model source
			ModelSource[] modelSources = theModel.getSources(trans);
			assertThat(modelSources.length, is(1));
			ModelSource theModelSource = modelSources[0];
			assertThat(theModelSource.getTranslatorName(trans), is("postgresql-override"));
			assertThat(theModelSource.getJndiName(trans), is("java:/postgresql"));
		}
		
		{ // Check translator
			final Translator[] translators = vdb.getTranslators(trans);
			assertThat(translators.length, is(1));
			
			final Translator theTranslator = translators[0];
			assertThat(theTranslator.getName(trans), is("postgresql-override"));
			assertThat(theTranslator.getType(trans), is("postgresql"));

			String propNames[] = theTranslator.getPropertyNames(trans);
			assertThat(propNames.length, is(3));
			
	    	Property transProp = theTranslator.getProperty(trans, "MaxInCriteriaSize");
	    	Long maxInCriteriaSize = transProp.getLongValue(trans);
	    	assertThat(maxInCriteriaSize, is(Long.valueOf(100)));

	    	transProp = theTranslator.getProperty(trans, "MaxDependentInPredicates");
	    	Long maxDependentInPredicates = transProp.getLongValue(trans);
	    	assertThat(maxDependentInPredicates, is(Long.valueOf(100)));
		}
    }
    
    /**
     * Import VDB6 - s-vdb.xml
     * @throws Exception
     */
    @Test
    public void testImportVdb6() throws Exception {
    	setup(IMPORT_VDB6, ImportCommand.class);

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
    
    /**
     * Import VDB7 - BQT2_ALL_TYPES_TABLE-vdb.xml
     * @throws Exception
     */
    @Test
    public void testImportVdb7() throws Exception {
    	setup(IMPORT_VDB7, ImportCommand.class);

//    	execute();
//
//    	assertEquals("/workspace", wsStatus.getCurrentContext().getFullName()); 
//
//    	WorkspaceContext vdbContext = ContextUtils.getContextForPath(wsStatus, "/workspace/BQT2_ALL_TYPES_TABLE"); 
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
//        assertThat(vdb.getVersion(trans), is(1));
//
//    	// Check VDB connectionType property
//    	Property prop = vdb.getProperty(trans, "vdb:connectionType");
//    	String connectionValue = prop.getStringValue(trans);
//        assertEquals("BY_VERSION", connectionValue);
//    	
//        // Check validationDateTime property
//    	prop = vdb.getProperty(trans, "validationDateTime");
//    	String dtValue = prop.getStringValue(trans);
//        assertEquals("Mon Jul 06 10:05:02 CDT 2015", dtValue);
//
//        // Check validationVersion property
//    	prop = vdb.getProperty(trans, "validationVersion");
//    	String validationVersionValue = prop.getStringValue(trans);
//        assertEquals("8.7.1", validationVersionValue);
//        
//        
//		{ // Check model
//			final Model[] models = vdb.getModels(trans);
//			assertThat(models.length, is(1));
//			
//			final Model theModel = models[0];
//			assertThat(theModel.getName(trans), is("BQT2_SQLSS2000"));
//			assertThat(theModel.getModelType(trans), is(Model.Type.PHYSICAL));
//			
//			// Check model source
//			ModelSource[] modelSources = theModel.getSources(trans);
//			assertThat(modelSources.length, is(1));
//			ModelSource theModelSource = modelSources[0];
//			assertThat(theModelSource.getTranslatorName(trans), is("sqlserver"));
//			assertThat(theModelSource.getJndiName(trans), is("BQT2_SQLSS2000"));
//			
//			Table[] tables = theModel.getTables(trans);
//			assertThat(tables.length, is(1));
//			Table theTable = tables[0];
//			assertThat(theTable.getName(trans), is("ALL_TYPES"));
//			String nis = theTable.getNameInSource(trans);
//			assertThat(nis, is("\"bqt2\".\"BQT2\".\"ALL_TYPES\""));
//			
//			Column[] columns = theTable.getColumns(trans);
//			assertThat(columns.length, is(33));
//
//			// check column names
//			assertThat(columns[0].getName(trans),is("type_int"));  //type_int integer OPTIONS(NAMEINSOURCE '"type_int"', NATIVE_TYPE 'int', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[1].getName(trans),is("type_integer"));  //type_integer integer OPTIONS(NAMEINSOURCE '"type_integer"', NATIVE_TYPE 'int', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[2].getName(trans),is("type_smallint"));  //type_smallint short OPTIONS(NAMEINSOURCE '"type_smallint"', NATIVE_TYPE 'smallint', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[3].getName(trans),is("type_tinyint"));  //type_tinyint byte OPTIONS(NAMEINSOURCE '"type_tinyint"', NATIVE_TYPE 'tinyint', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[4].getName(trans),is("type_decimal"));  //type_decimal bigdecimal OPTIONS(NAMEINSOURCE '"type_decimal"', NATIVE_TYPE 'decimal', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[5].getName(trans),is("type_decimal_5"));  //type_decimal_5 bigdecimal OPTIONS(NAMEINSOURCE '"type_decimal_5"', NATIVE_TYPE 'decimal', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[6].getName(trans),is("type_decimal_5_5"));  //type_decimal_5_5 bigdecimal OPTIONS(NAMEINSOURCE '"type_decimal_5_5"', NATIVE_TYPE 'decimal', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[7].getName(trans),is("type_double_precision"));  //type_double_precision float OPTIONS(NAMEINSOURCE '"type_double_precision"', NATIVE_TYPE 'float', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[8].getName(trans),is("type_float"));  //type_float float OPTIONS(NAMEINSOURCE '"type_float"', NATIVE_TYPE 'float', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[9].getName(trans),is("type_float_10"));  //type_float_10 float OPTIONS(NAMEINSOURCE '"type_float_10"', NATIVE_TYPE 'real', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[10].getName(trans),is("type_numeric"));  //type_numeric bigdecimal OPTIONS(NAMEINSOURCE '"type_numeric"', NATIVE_TYPE 'numeric', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[11].getName(trans),is("type_numeric_5"));  //type_numeric_5 bigdecimal OPTIONS(NAMEINSOURCE '"type_numeric_5"', NATIVE_TYPE 'numeric', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[12].getName(trans),is("type_numeric_5_5"));  //type_numeric_5_5 bigdecimal OPTIONS(NAMEINSOURCE '"type_numeric_5_5"', NATIVE_TYPE 'numeric', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[13].getName(trans),is("type_real"));  //type_real float OPTIONS(NAMEINSOURCE '"type_real"', NATIVE_TYPE 'real', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[14].getName(trans),is("type_bit"));  //type_bit boolean OPTIONS(NAMEINSOURCE '"type_bit"', NATIVE_TYPE 'bit', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[15].getName(trans),is("type_character"));  //type_character char(1) OPTIONS(NAMEINSOURCE '"type_character"', NATIVE_TYPE 'char', FIXED_LENGTH true),
//			assertThat(columns[16].getName(trans),is("type_character_10"));  //type_character_10 string(10) OPTIONS(NAMEINSOURCE '"type_character_10"', NATIVE_TYPE 'char', FIXED_LENGTH true),
//			assertThat(columns[17].getName(trans),is("type_char"));  //type_char char(1) OPTIONS(NAMEINSOURCE '"type_char"', NATIVE_TYPE 'char', FIXED_LENGTH true),
//			assertThat(columns[18].getName(trans),is("type_char_10"));  //type_char_10 string(10) OPTIONS(NAMEINSOURCE '"type_char_10"', NATIVE_TYPE 'char', FIXED_LENGTH true),
//			assertThat(columns[19].getName(trans),is("type_nchar"));  //type_nchar string(1) OPTIONS(NAMEINSOURCE '"type_nchar"', NATIVE_TYPE 'nchar', FIXED_LENGTH true),
//			assertThat(columns[20].getName(trans),is("type_nchar_10"));  //type_nchar_10 string(10) OPTIONS(NAMEINSOURCE '"type_nchar_10"', NATIVE_TYPE 'nchar', FIXED_LENGTH true),
//			assertThat(columns[21].getName(trans),is("type_varchar"));  //type_varchar string(1) OPTIONS(NAMEINSOURCE '"type_varchar"', NATIVE_TYPE 'varchar'),
//			assertThat(columns[22].getName(trans),is("type_varchar_10"));  //type_varchar_10 string(10) OPTIONS(NAMEINSOURCE '"type_varchar_10"', NATIVE_TYPE 'varchar'),
//			assertThat(columns[23].getName(trans),is("type_long_nvarchar"));  //type_long_nvarchar string(1) OPTIONS(NAMEINSOURCE '"type_long_nvarchar"', NATIVE_TYPE 'nvarchar', FIXED_LENGTH true),
//			assertThat(columns[24].getName(trans),is("type_long_nvarchar_10"));  //type_long_nvarchar_10 string(10) OPTIONS(NAMEINSOURCE '"type_long_nvarchar_10"', NATIVE_TYPE 'nvarchar', FIXED_LENGTH true),
//			assertThat(columns[25].getName(trans),is("type_text"));  //type_text clob(2147483647) OPTIONS(NAMEINSOURCE '"type_text"', NATIVE_TYPE 'text', CASE_SENSITIVE false, SEARCHABLE 'LIKE_ONLY'),
//			assertThat(columns[26].getName(trans),is("type_money"));  //type_money bigdecimal OPTIONS(NAMEINSOURCE '"type_money"', NATIVE_TYPE 'money', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[27].getName(trans),is("type_smallmoney"));  //type_smallmoney bigdecimal OPTIONS(NAMEINSOURCE '"type_smallmoney"', NATIVE_TYPE 'smallmoney', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[28].getName(trans),is("type_datetime"));  //type_datetime timestamp OPTIONS(NAMEINSOURCE '"type_datetime"', NATIVE_TYPE 'datetime', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[29].getName(trans),is("type_binary"));  //type_binary object(1) OPTIONS(NAMEINSOURCE '"type_binary"', NATIVE_TYPE 'binary', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'UNSEARCHABLE'),
//			assertThat(columns[30].getName(trans),is("type_binary_2"));  //type_binary_2 object(2) OPTIONS(NAMEINSOURCE '"type_binary_2"', NATIVE_TYPE 'binary', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'UNSEARCHABLE'),
//			assertThat(columns[31].getName(trans),is("type_image"));  //type_image blob(2147483647) OPTIONS(NAMEINSOURCE '"type_image"', NATIVE_TYPE 'image', CASE_SENSITIVE false, SEARCHABLE 'UNSEARCHABLE'),
//			assertThat(columns[32].getName(trans),is("type_varbinary"));  //type_varbinary string(0) OPTIONS(NAMEINSOURCE '"type_varbinary"', NATIVE_TYPE 'varbinary', CASE_SENSITIVE false, SEARCHABLE 'ALL_EXCEPT_LIKE')
//			
//			// check column name in source
//			assertThat(columns[0].getNameInSource(trans),is("\"type_int\""));  //type_int integer OPTIONS(NAMEINSOURCE '"type_int"', NATIVE_TYPE 'int', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[1].getNameInSource(trans),is("\"type_integer\""));  //type_integer integer OPTIONS(NAMEINSOURCE '"type_integer"', NATIVE_TYPE 'int', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[2].getNameInSource(trans),is("\"type_smallint\""));  //type_smallint short OPTIONS(NAMEINSOURCE '"type_smallint"', NATIVE_TYPE 'smallint', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[3].getNameInSource(trans),is("\"type_tinyint\""));  //type_tinyint byte OPTIONS(NAMEINSOURCE '"type_tinyint"', NATIVE_TYPE 'tinyint', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[4].getNameInSource(trans),is("\"type_decimal\""));  //type_decimal bigdecimal OPTIONS(NAMEINSOURCE '"type_decimal"', NATIVE_TYPE 'decimal', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[5].getNameInSource(trans),is("\"type_decimal_5\""));  //type_decimal_5 bigdecimal OPTIONS(NAMEINSOURCE '"type_decimal_5"', NATIVE_TYPE 'decimal', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[6].getNameInSource(trans),is("\"type_decimal_5_5\""));  //type_decimal_5_5 bigdecimal OPTIONS(NAMEINSOURCE '"type_decimal_5_5"', NATIVE_TYPE 'decimal', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[7].getNameInSource(trans),is("\"type_double_precision\""));  //type_double_precision float OPTIONS(NAMEINSOURCE '"type_double_precision"', NATIVE_TYPE 'float', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[8].getNameInSource(trans),is("\"type_float\""));  //type_float float OPTIONS(NAMEINSOURCE '"type_float"', NATIVE_TYPE 'float', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[9].getNameInSource(trans),is("\"type_float_10\""));  //type_float_10 float OPTIONS(NAMEINSOURCE '"type_float_10"', NATIVE_TYPE 'real', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[10].getNameInSource(trans),is("\"type_numeric\""));  //type_numeric bigdecimal OPTIONS(NAMEINSOURCE '"type_numeric"', NATIVE_TYPE 'numeric', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[11].getNameInSource(trans),is("\"type_numeric_5\""));  //type_numeric_5 bigdecimal OPTIONS(NAMEINSOURCE '"type_numeric_5"', NATIVE_TYPE 'numeric', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[12].getNameInSource(trans),is("\"type_numeric_5_5\""));  //type_numeric_5_5 bigdecimal OPTIONS(NAMEINSOURCE '"type_numeric_5_5"', NATIVE_TYPE 'numeric', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[13].getNameInSource(trans),is("\"type_real\""));  //type_real float OPTIONS(NAMEINSOURCE '"type_real"', NATIVE_TYPE 'real', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[14].getNameInSource(trans),is("\"type_bit\""));  //type_bit boolean OPTIONS(NAMEINSOURCE '"type_bit"', NATIVE_TYPE 'bit', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[15].getNameInSource(trans),is("\"type_character\""));  //type_character char(1) OPTIONS(NAMEINSOURCE '"type_character"', NATIVE_TYPE 'char', FIXED_LENGTH true),
//			assertThat(columns[16].getNameInSource(trans),is("\"type_character_10\""));  //type_character_10 string(10) OPTIONS(NAMEINSOURCE '"type_character_10"', NATIVE_TYPE 'char', FIXED_LENGTH true),
//			assertThat(columns[17].getNameInSource(trans),is("\"type_char\""));  //type_char char(1) OPTIONS(NAMEINSOURCE '"type_char"', NATIVE_TYPE 'char', FIXED_LENGTH true),
//			assertThat(columns[18].getNameInSource(trans),is("\"type_char_10\""));  //type_char_10 string(10) OPTIONS(NAMEINSOURCE '"type_char_10"', NATIVE_TYPE 'char', FIXED_LENGTH true),
//			assertThat(columns[19].getNameInSource(trans),is("\"type_nchar\""));  //type_nchar string(1) OPTIONS(NAMEINSOURCE '"type_nchar"', NATIVE_TYPE 'nchar', FIXED_LENGTH true),
//			assertThat(columns[20].getNameInSource(trans),is("\"type_nchar_10\""));  //type_nchar_10 string(10) OPTIONS(NAMEINSOURCE '"type_nchar_10"', NATIVE_TYPE 'nchar', FIXED_LENGTH true),
//			assertThat(columns[21].getNameInSource(trans),is("\"type_varchar\""));  //type_varchar string(1) OPTIONS(NAMEINSOURCE '"type_varchar"', NATIVE_TYPE 'varchar'),
//			assertThat(columns[22].getNameInSource(trans),is("\"type_varchar_10\""));  //type_varchar_10 string(10) OPTIONS(NAMEINSOURCE '"type_varchar_10"', NATIVE_TYPE 'varchar'),
//			assertThat(columns[23].getNameInSource(trans),is("\"type_long_nvarchar\""));  //type_long_nvarchar string(1) OPTIONS(NAMEINSOURCE '"type_long_nvarchar"', NATIVE_TYPE 'nvarchar', FIXED_LENGTH true),
//			assertThat(columns[24].getNameInSource(trans),is("\"type_long_nvarchar_10\""));  //type_long_nvarchar_10 string(10) OPTIONS(NAMEINSOURCE '"type_long_nvarchar_10"', NATIVE_TYPE 'nvarchar', FIXED_LENGTH true),
//			assertThat(columns[25].getNameInSource(trans),is("\"type_text\""));  //type_text clob(2147483647) OPTIONS(NAMEINSOURCE '"type_text"', NATIVE_TYPE 'text', CASE_SENSITIVE false, SEARCHABLE 'LIKE_ONLY'),
//			assertThat(columns[26].getNameInSource(trans),is("\"type_money\""));  //type_money bigdecimal OPTIONS(NAMEINSOURCE '"type_money"', NATIVE_TYPE 'money', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[27].getNameInSource(trans),is("\"type_smallmoney\""));  //type_smallmoney bigdecimal OPTIONS(NAMEINSOURCE '"type_smallmoney"', NATIVE_TYPE 'smallmoney', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[28].getNameInSource(trans),is("\"type_datetime\""));  //type_datetime timestamp OPTIONS(NAMEINSOURCE '"type_datetime"', NATIVE_TYPE 'datetime', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[29].getNameInSource(trans),is("\"type_binary\""));  //type_binary object(1) OPTIONS(NAMEINSOURCE '"type_binary"', NATIVE_TYPE 'binary', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'UNSEARCHABLE'),
//			assertThat(columns[30].getNameInSource(trans),is("\"type_binary_2\""));  //type_binary_2 object(2) OPTIONS(NAMEINSOURCE '"type_binary_2"', NATIVE_TYPE 'binary', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'UNSEARCHABLE'),
//			assertThat(columns[31].getNameInSource(trans),is("\"type_image\""));  //type_image blob(2147483647) OPTIONS(NAMEINSOURCE '"type_image"', NATIVE_TYPE 'image', CASE_SENSITIVE false, SEARCHABLE 'UNSEARCHABLE'),
//			assertThat(columns[32].getNameInSource(trans),is("\"type_varbinary\""));  //type_varbinary string(0) OPTIONS(NAMEINSOURCE '"type_varbinary"', NATIVE_TYPE 'varbinary', CASE_SENSITIVE false, SEARCHABLE 'ALL_EXCEPT_LIKE')
//
//			// check datatype names
//			assertThat(columns[0].getDatatypeName(trans).toLowerCase(), is("integer"));  //type_int integer OPTIONS(NAMEINSOURCE '"type_int"', NATIVE_TYPE 'int', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[1].getDatatypeName(trans).toLowerCase(), is("integer"));  //type_integer integer OPTIONS(NAMEINSOURCE '"type_integer"', NATIVE_TYPE 'int', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[2].getDatatypeName(trans).toLowerCase(), is("short"));  //type_smallint short OPTIONS(NAMEINSOURCE '"type_smallint"', NATIVE_TYPE 'smallint', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[3].getDatatypeName(trans).toLowerCase(), is("byte"));  //type_tinyint byte OPTIONS(NAMEINSOURCE '"type_tinyint"', NATIVE_TYPE 'tinyint', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[4].getDatatypeName(trans).toLowerCase(), is("bigdecimal"));  //type_decimal bigdecimal OPTIONS(NAMEINSOURCE '"type_decimal"', NATIVE_TYPE 'decimal', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[5].getDatatypeName(trans).toLowerCase(), is("bigdecimal"));  //type_decimal_5 bigdecimal OPTIONS(NAMEINSOURCE '"type_decimal_5"', NATIVE_TYPE 'decimal', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[6].getDatatypeName(trans).toLowerCase(), is("bigdecimal"));  //type_decimal_5_5 bigdecimal OPTIONS(NAMEINSOURCE '"type_decimal_5_5"', NATIVE_TYPE 'decimal', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[7].getDatatypeName(trans).toLowerCase(), is("float"));  //type_double_precision float OPTIONS(NAMEINSOURCE '"type_double_precision"', NATIVE_TYPE 'float', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[8].getDatatypeName(trans).toLowerCase(), is("float"));  //type_float float OPTIONS(NAMEINSOURCE '"type_float"', NATIVE_TYPE 'float', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[9].getDatatypeName(trans).toLowerCase(), is("float"));  //type_float_10 float OPTIONS(NAMEINSOURCE '"type_float_10"', NATIVE_TYPE 'real', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[10].getDatatypeName(trans).toLowerCase(), is("bigdecimal"));  //type_numeric bigdecimal OPTIONS(NAMEINSOURCE '"type_numeric"', NATIVE_TYPE 'numeric', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[11].getDatatypeName(trans).toLowerCase(), is("bigdecimal"));  //type_numeric_5 bigdecimal OPTIONS(NAMEINSOURCE '"type_numeric_5"', NATIVE_TYPE 'numeric', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[12].getDatatypeName(trans).toLowerCase(), is("bigdecimal"));  //type_numeric_5_5 bigdecimal OPTIONS(NAMEINSOURCE '"type_numeric_5_5"', NATIVE_TYPE 'numeric', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[13].getDatatypeName(trans).toLowerCase(), is("float"));  //type_real float OPTIONS(NAMEINSOURCE '"type_real"', NATIVE_TYPE 'real', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[14].getDatatypeName(trans).toLowerCase(), is("boolean"));  //type_bit boolean OPTIONS(NAMEINSOURCE '"type_bit"', NATIVE_TYPE 'bit', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[15].getDatatypeName(trans).toLowerCase(), is("char"));  //type_character char(1) OPTIONS(NAMEINSOURCE '"type_character"', NATIVE_TYPE 'char', FIXED_LENGTH true),
//			assertThat(columns[16].getDatatypeName(trans).toLowerCase(), is("string"));  //type_character_10 string(10) OPTIONS(NAMEINSOURCE '"type_character_10"', NATIVE_TYPE 'char', FIXED_LENGTH true),
//			assertThat(columns[17].getDatatypeName(trans).toLowerCase(), is("char"));  //type_char char(1) OPTIONS(NAMEINSOURCE '"type_char"', NATIVE_TYPE 'char', FIXED_LENGTH true),
//			assertThat(columns[18].getDatatypeName(trans).toLowerCase(), is("string"));  //type_char_10 string(10) OPTIONS(NAMEINSOURCE '"type_char_10"', NATIVE_TYPE 'char', FIXED_LENGTH true),
//			assertThat(columns[19].getDatatypeName(trans).toLowerCase(), is("string"));  //type_nchar string(1) OPTIONS(NAMEINSOURCE '"type_nchar"', NATIVE_TYPE 'nchar', FIXED_LENGTH true),
//			assertThat(columns[20].getDatatypeName(trans).toLowerCase(), is("string"));  //type_nchar_10 string(10) OPTIONS(NAMEINSOURCE '"type_nchar_10"', NATIVE_TYPE 'nchar', FIXED_LENGTH true),
//			assertThat(columns[21].getDatatypeName(trans).toLowerCase(), is("string"));  //type_varchar string(1) OPTIONS(NAMEINSOURCE '"type_varchar"', NATIVE_TYPE 'varchar'),
//			assertThat(columns[22].getDatatypeName(trans).toLowerCase(), is("string"));  //type_varchar_10 string(10) OPTIONS(NAMEINSOURCE '"type_varchar_10"', NATIVE_TYPE 'varchar'),
//			assertThat(columns[23].getDatatypeName(trans).toLowerCase(), is("string"));  //type_long_nvarchar string(1) OPTIONS(NAMEINSOURCE '"type_long_nvarchar"', NATIVE_TYPE 'nvarchar', FIXED_LENGTH true),
//			assertThat(columns[24].getDatatypeName(trans).toLowerCase(), is("string"));  //type_long_nvarchar_10 string(10) OPTIONS(NAMEINSOURCE '"type_long_nvarchar_10"', NATIVE_TYPE 'nvarchar', FIXED_LENGTH true),
//			assertThat(columns[25].getDatatypeName(trans).toLowerCase(), is("clob"));  //type_text clob(2147483647) OPTIONS(NAMEINSOURCE '"type_text"', NATIVE_TYPE 'text', CASE_SENSITIVE false, SEARCHABLE 'LIKE_ONLY'),
//			assertThat(columns[26].getDatatypeName(trans).toLowerCase(), is("bigdecimal"));  //type_money bigdecimal OPTIONS(NAMEINSOURCE '"type_money"', NATIVE_TYPE 'money', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[27].getDatatypeName(trans).toLowerCase(), is("bigdecimal"));  //type_smallmoney bigdecimal OPTIONS(NAMEINSOURCE '"type_smallmoney"', NATIVE_TYPE 'smallmoney', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[28].getDatatypeName(trans).toLowerCase(), is("timestamp"));  //type_datetime timestamp OPTIONS(NAMEINSOURCE '"type_datetime"', NATIVE_TYPE 'datetime', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[29].getDatatypeName(trans).toLowerCase(), is("object"));  //type_binary object(1) OPTIONS(NAMEINSOURCE '"type_binary"', NATIVE_TYPE 'binary', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'UNSEARCHABLE'),
//			assertThat(columns[30].getDatatypeName(trans).toLowerCase(), is("object"));  //type_binary_2 object(2) OPTIONS(NAMEINSOURCE '"type_binary_2"', NATIVE_TYPE 'binary', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'UNSEARCHABLE'),
//			assertThat(columns[31].getDatatypeName(trans).toLowerCase(), is("blob"));  //type_image blob(2147483647) OPTIONS(NAMEINSOURCE '"type_image"', NATIVE_TYPE 'image', CASE_SENSITIVE false, SEARCHABLE 'UNSEARCHABLE'),
//			assertThat(columns[32].getDatatypeName(trans).toLowerCase(), is("string"));  //type_varbinary string(0) OPTIONS(NAMEINSOURCE '"type_varbinary"', NATIVE_TYPE 'varbinary', CASE_SENSITIVE false, SEARCHABLE 'ALL_EXCEPT_LIKE')
//
//			// check native types
//			assertThat(columns[0].getNativeType(trans).toLowerCase(), is("int"));  //type_int integer OPTIONS(NAMEINSOURCE '"type_int"', NATIVE_TYPE 'int', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[1].getNativeType(trans).toLowerCase(), is("int"));  //type_integer integer OPTIONS(NAMEINSOURCE '"type_integer"', NATIVE_TYPE 'int', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[2].getNativeType(trans).toLowerCase(), is("smallint"));  //type_smallint short OPTIONS(NAMEINSOURCE '"type_smallint"', NATIVE_TYPE 'smallint', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[3].getNativeType(trans).toLowerCase(), is("tinyint"));  //type_tinyint byte OPTIONS(NAMEINSOURCE '"type_tinyint"', NATIVE_TYPE 'tinyint', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[4].getNativeType(trans).toLowerCase(), is("decimal"));  //type_decimal bigdecimal OPTIONS(NAMEINSOURCE '"type_decimal"', NATIVE_TYPE 'decimal', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[5].getNativeType(trans).toLowerCase(), is("decimal"));  //type_decimal_5 bigdecimal OPTIONS(NAMEINSOURCE '"type_decimal_5"', NATIVE_TYPE 'decimal', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[6].getNativeType(trans).toLowerCase(), is("decimal"));  //type_decimal_5_5 bigdecimal OPTIONS(NAMEINSOURCE '"type_decimal_5_5"', NATIVE_TYPE 'decimal', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[7].getNativeType(trans).toLowerCase(), is("float"));  //type_double_precision float OPTIONS(NAMEINSOURCE '"type_double_precision"', NATIVE_TYPE 'float', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[8].getNativeType(trans).toLowerCase(), is("float"));  //type_float float OPTIONS(NAMEINSOURCE '"type_float"', NATIVE_TYPE 'float', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[9].getNativeType(trans).toLowerCase(), is("real"));  //type_float_10 float OPTIONS(NAMEINSOURCE '"type_float_10"', NATIVE_TYPE 'real', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[10].getNativeType(trans).toLowerCase(), is("numeric"));  //type_numeric bigdecimal OPTIONS(NAMEINSOURCE '"type_numeric"', NATIVE_TYPE 'numeric', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[11].getNativeType(trans).toLowerCase(), is("numeric"));  //type_numeric_5 bigdecimal OPTIONS(NAMEINSOURCE '"type_numeric_5"', NATIVE_TYPE 'numeric', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[12].getNativeType(trans).toLowerCase(), is("numeric"));  //type_numeric_5_5 bigdecimal OPTIONS(NAMEINSOURCE '"type_numeric_5_5"', NATIVE_TYPE 'numeric', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[13].getNativeType(trans).toLowerCase(), is("real"));  //type_real float OPTIONS(NAMEINSOURCE '"type_real"', NATIVE_TYPE 'real', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[14].getNativeType(trans).toLowerCase(), is("bit"));  //type_bit boolean OPTIONS(NAMEINSOURCE '"type_bit"', NATIVE_TYPE 'bit', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[15].getNativeType(trans).toLowerCase(), is("char"));  //type_character char(1) OPTIONS(NAMEINSOURCE '"type_character"', NATIVE_TYPE 'char', FIXED_LENGTH true),
//			assertThat(columns[16].getNativeType(trans).toLowerCase(), is("char"));  //type_character_10 string(10) OPTIONS(NAMEINSOURCE '"type_character_10"', NATIVE_TYPE 'char', FIXED_LENGTH true),
//			assertThat(columns[17].getNativeType(trans).toLowerCase(), is("char"));  //type_char char(1) OPTIONS(NAMEINSOURCE '"type_char"', NATIVE_TYPE 'char', FIXED_LENGTH true),
//			assertThat(columns[18].getNativeType(trans).toLowerCase(), is("char"));  //type_char_10 string(10) OPTIONS(NAMEINSOURCE '"type_char_10"', NATIVE_TYPE 'char', FIXED_LENGTH true),
//			assertThat(columns[19].getNativeType(trans).toLowerCase(), is("nchar"));  //type_nchar string(1) OPTIONS(NAMEINSOURCE '"type_nchar"', NATIVE_TYPE 'nchar', FIXED_LENGTH true),
//			assertThat(columns[20].getNativeType(trans).toLowerCase(), is("nchar"));  //type_nchar_10 string(10) OPTIONS(NAMEINSOURCE '"type_nchar_10"', NATIVE_TYPE 'nchar', FIXED_LENGTH true),
//			assertThat(columns[21].getNativeType(trans).toLowerCase(), is("varchar"));  //type_varchar string(1) OPTIONS(NAMEINSOURCE '"type_varchar"', NATIVE_TYPE 'varchar'),
//			assertThat(columns[22].getNativeType(trans).toLowerCase(), is("varchar"));  //type_varchar_10 string(10) OPTIONS(NAMEINSOURCE '"type_varchar_10"', NATIVE_TYPE 'varchar'),
//			assertThat(columns[23].getNativeType(trans).toLowerCase(), is("nvarchar"));  //type_long_nvarchar string(1) OPTIONS(NAMEINSOURCE '"type_long_nvarchar"', NATIVE_TYPE 'nvarchar', FIXED_LENGTH true),
//			assertThat(columns[24].getNativeType(trans).toLowerCase(), is("nvarchar"));  //type_long_nvarchar_10 string(10) OPTIONS(NAMEINSOURCE '"type_long_nvarchar_10"', NATIVE_TYPE 'nvarchar', FIXED_LENGTH true),
//			assertThat(columns[25].getNativeType(trans).toLowerCase(), is("text"));  //type_text clob(2147483647) OPTIONS(NAMEINSOURCE '"type_text"', NATIVE_TYPE 'text', CASE_SENSITIVE false, SEARCHABLE 'LIKE_ONLY'),
//			assertThat(columns[26].getNativeType(trans).toLowerCase(), is("money"));  //type_money bigdecimal OPTIONS(NAMEINSOURCE '"type_money"', NATIVE_TYPE 'money', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[27].getNativeType(trans).toLowerCase(), is("smallmoney"));  //type_smallmoney bigdecimal OPTIONS(NAMEINSOURCE '"type_smallmoney"', NATIVE_TYPE 'smallmoney', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[28].getNativeType(trans).toLowerCase(), is("datetime"));  //type_datetime timestamp OPTIONS(NAMEINSOURCE '"type_datetime"', NATIVE_TYPE 'datetime', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[29].getNativeType(trans).toLowerCase(), is("binary"));  //type_binary object(1) OPTIONS(NAMEINSOURCE '"type_binary"', NATIVE_TYPE 'binary', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'UNSEARCHABLE'),
//			assertThat(columns[30].getNativeType(trans).toLowerCase(), is("binary"));  //type_binary_2 object(2) OPTIONS(NAMEINSOURCE '"type_binary_2"', NATIVE_TYPE 'binary', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'UNSEARCHABLE'),
//			assertThat(columns[31].getNativeType(trans).toLowerCase(), is("image"));  //type_image blob(2147483647) OPTIONS(NAMEINSOURCE '"type_image"', NATIVE_TYPE 'image', CASE_SENSITIVE false, SEARCHABLE 'UNSEARCHABLE'),
//			assertThat(columns[32].getNativeType(trans).toLowerCase(), is("varbinary"));  //type_varbinary string(0) OPTIONS(NAMEINSOURCE '"type_varbinary"', NATIVE_TYPE 'varbinary', CASE_SENSITIVE false, SEARCHABLE 'ALL_EXCEPT_LIKE')
//
//			// check lengths
//			assertThat(columns[0].getLength(trans), is(Long.valueOf(0)));  //type_int integer OPTIONS(NAMEINSOURCE '"type_int"', NATIVE_TYPE 'int', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[1].getLength(trans), is(Long.valueOf(0)));  //type_integer integer OPTIONS(NAMEINSOURCE '"type_integer"', NATIVE_TYPE 'int', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[2].getLength(trans), is(Long.valueOf(0)));  //type_smallint short OPTIONS(NAMEINSOURCE '"type_smallint"', NATIVE_TYPE 'smallint', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[3].getLength(trans), is(Long.valueOf(0)));  //type_tinyint byte OPTIONS(NAMEINSOURCE '"type_tinyint"', NATIVE_TYPE 'tinyint', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[4].getLength(trans), is(Long.valueOf(0)));  //type_decimal bigdecimal OPTIONS(NAMEINSOURCE '"type_decimal"', NATIVE_TYPE 'decimal', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[5].getLength(trans), is(Long.valueOf(0)));  //type_decimal_5 bigdecimal OPTIONS(NAMEINSOURCE '"type_decimal_5"', NATIVE_TYPE 'decimal', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[6].getLength(trans), is(Long.valueOf(0)));  //type_decimal_5_5 bigdecimal OPTIONS(NAMEINSOURCE '"type_decimal_5_5"', NATIVE_TYPE 'decimal', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[7].getLength(trans), is(Long.valueOf(0)));  //type_double_precision float OPTIONS(NAMEINSOURCE '"type_double_precision"', NATIVE_TYPE 'float', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[8].getLength(trans), is(Long.valueOf(0)));  //type_float float OPTIONS(NAMEINSOURCE '"type_float"', NATIVE_TYPE 'float', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[9].getLength(trans), is(Long.valueOf(0)));  //type_float_10 float OPTIONS(NAMEINSOURCE '"type_float_10"', NATIVE_TYPE 'real', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[10].getLength(trans), is(Long.valueOf(0)));  //type_numeric bigdecimal OPTIONS(NAMEINSOURCE '"type_numeric"', NATIVE_TYPE 'numeric', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[11].getLength(trans), is(Long.valueOf(0)));  //type_numeric_5 bigdecimal OPTIONS(NAMEINSOURCE '"type_numeric_5"', NATIVE_TYPE 'numeric', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[12].getLength(trans), is(Long.valueOf(0)));  //type_numeric_5_5 bigdecimal OPTIONS(NAMEINSOURCE '"type_numeric_5_5"', NATIVE_TYPE 'numeric', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[13].getLength(trans), is(Long.valueOf(0)));  //type_real float OPTIONS(NAMEINSOURCE '"type_real"', NATIVE_TYPE 'real', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[14].getLength(trans), is(Long.valueOf(0)));  //type_bit boolean OPTIONS(NAMEINSOURCE '"type_bit"', NATIVE_TYPE 'bit', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[15].getLength(trans), is(Long.valueOf(1)));  //type_character char(1) OPTIONS(NAMEINSOURCE '"type_character"', NATIVE_TYPE 'char', FIXED_LENGTH true),
//			assertThat(columns[16].getLength(trans), is(Long.valueOf(10)));  //type_character_10 string(10) OPTIONS(NAMEINSOURCE '"type_character_10"', NATIVE_TYPE 'char', FIXED_LENGTH true),
//			assertThat(columns[17].getLength(trans), is(Long.valueOf(1)));  //type_char char(1) OPTIONS(NAMEINSOURCE '"type_char"', NATIVE_TYPE 'char', FIXED_LENGTH true),
//			assertThat(columns[18].getLength(trans), is(Long.valueOf(10)));  //type_char_10 string(10) OPTIONS(NAMEINSOURCE '"type_char_10"', NATIVE_TYPE 'char', FIXED_LENGTH true),
//			assertThat(columns[19].getLength(trans), is(Long.valueOf(1)));  //type_nchar string(1) OPTIONS(NAMEINSOURCE '"type_nchar"', NATIVE_TYPE 'nchar', FIXED_LENGTH true),
//			assertThat(columns[20].getLength(trans), is(Long.valueOf(10)));  //type_nchar_10 string(10) OPTIONS(NAMEINSOURCE '"type_nchar_10"', NATIVE_TYPE 'nchar', FIXED_LENGTH true),
//			assertThat(columns[21].getLength(trans), is(Long.valueOf(1)));  //type_varchar string(1) OPTIONS(NAMEINSOURCE '"type_varchar"', NATIVE_TYPE 'varchar'),
//			assertThat(columns[22].getLength(trans), is(Long.valueOf(10)));  //type_varchar_10 string(10) OPTIONS(NAMEINSOURCE '"type_varchar_10"', NATIVE_TYPE 'varchar'),
//			assertThat(columns[23].getLength(trans), is(Long.valueOf(1)));  //type_long_nvarchar string(1) OPTIONS(NAMEINSOURCE '"type_long_nvarchar"', NATIVE_TYPE 'nvarchar', FIXED_LENGTH true),
//			assertThat(columns[24].getLength(trans), is(Long.valueOf(10)));  //type_long_nvarchar_10 string(10) OPTIONS(NAMEINSOURCE '"type_long_nvarchar_10"', NATIVE_TYPE 'nvarchar', FIXED_LENGTH true),
//			assertThat(columns[25].getLength(trans), is(Long.valueOf(2147483647)));  //type_text clob(2147483647) OPTIONS(NAMEINSOURCE '"type_text"', NATIVE_TYPE 'text', CASE_SENSITIVE false, SEARCHABLE 'LIKE_ONLY'),
//			assertThat(columns[26].getLength(trans), is(Long.valueOf(0)));  //type_money bigdecimal OPTIONS(NAMEINSOURCE '"type_money"', NATIVE_TYPE 'money', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[27].getLength(trans), is(Long.valueOf(0)));  //type_smallmoney bigdecimal OPTIONS(NAMEINSOURCE '"type_smallmoney"', NATIVE_TYPE 'smallmoney', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[28].getLength(trans), is(Long.valueOf(0)));  //type_datetime timestamp OPTIONS(NAMEINSOURCE '"type_datetime"', NATIVE_TYPE 'datetime', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'ALL_EXCEPT_LIKE'),
//			assertThat(columns[29].getLength(trans), is(Long.valueOf(1)));  //type_binary object(1) OPTIONS(NAMEINSOURCE '"type_binary"', NATIVE_TYPE 'binary', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'UNSEARCHABLE'),
//			assertThat(columns[30].getLength(trans), is(Long.valueOf(2)));  //type_binary_2 object(2) OPTIONS(NAMEINSOURCE '"type_binary_2"', NATIVE_TYPE 'binary', CASE_SENSITIVE false, FIXED_LENGTH true, SEARCHABLE 'UNSEARCHABLE'),
//			assertThat(columns[31].getLength(trans), is(Long.valueOf(2147483647)));  //type_image blob(2147483647) OPTIONS(NAMEINSOURCE '"type_image"', NATIVE_TYPE 'image', CASE_SENSITIVE false, SEARCHABLE 'UNSEARCHABLE'),
//			assertThat(columns[32].getLength(trans), is(Long.valueOf(0)));  //type_varbinary string(0) OPTIONS(NAMEINSOURCE '"type_varbinary"', NATIVE_TYPE 'varbinary', CASE_SENSITIVE false, SEARCHABLE 'ALL_EXCEPT_LIKE')
//		}
		
    }
}
