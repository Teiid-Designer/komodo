package org.komodo.shell;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.Test;
import org.komodo.relational.vdb.Vdb;
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
    	
    }

}
