package org.komodo.shell;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import org.junit.Test;
import org.komodo.relational.model.Table;
import org.komodo.relational.vdb.Vdb;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.commands.core.CreateCommand;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;

/**
 *
 * @author blafond
 *
 */
@SuppressWarnings("javadoc")
public class CreateCommandTest extends AbstractCommandTest {

	private static final String CREATE_COMMAND_1 = "createCommand1.txt"; //$NON-NLS-1$
	private static final String CREATE_COMMAND_2 = "createCommand2.txt"; //$NON-NLS-1$
	private static final String CREATE_COMMAND_3 = "createCommand3.txt"; //$NON-NLS-1$
	private static final String CREATE_COMMAND_4 = "createCommand4.txt"; //$NON-NLS-1$
	private static final String CREATE_COMMAND_5 = "createCommand5.txt"; //$NON-NLS-1$
	private static final String CREATE_COMMAND_6 = "createCommand6.txt"; //$NON-NLS-1$
	private static final String CREATE_COMMAND_7 = "createCommand7.txt"; //$NON-NLS-1$
	private static final String CREATE_COMMAND_8 = "createCommand8.txt"; //$NON-NLS-1$
	
	/**
	 * Test for CreateCommand
	 */
	public CreateCommandTest( ) {
		super();
	}

    @Test
    public void testCreateVdb() throws Exception {
    	setup(CREATE_COMMAND_1, CreateCommand.class);

    	execute();

    	assertEquals("/tko:workspace/vdb_test_1", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

    @Test
    public void testCreateModel() throws Exception {
    	setup(CREATE_COMMAND_2, CreateCommand.class);

    	execute();

    	assertEquals("/tko:workspace/vdb_test_1/model_1", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

    @Test
    public void testCreateTable() throws Exception {
    	setup(CREATE_COMMAND_3, CreateCommand.class);

    	execute();

    	assertEquals("/tko:workspace/vdb_test_1/model_1/table_1", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

    @Test
    public void testCreateThreeColumns() throws Exception {
    	setup(CREATE_COMMAND_4, CreateCommand.class);

    	execute();

    	assertEquals("/tko:workspace/vdb_test_1/model_1/table_1", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$

    	KomodoObject ko = wsStatus.getCurrentContext().getKomodoObj();
    	// Verify the komodo class is a Table and is TABLE type
    	assertEquals(KomodoType.TABLE.name(), ko.getTypeIdentifier(this.uow).name());

    	Table table = (Table)resolveType(this.uow, ko, Table.class);

    	// verify 3 columns
    	assertEquals(3, table.getColumns(this.uow).length);

    	// verify PK
    	assertNotNull(table.getPrimaryKey(this.uow));
    	assertEquals("pk_1", table.getPrimaryKey(this.uow).getName(this.uow)); //$NON-NLS-1$

    	// verify 2 access patterns
    	assertEquals(2, table.getAccessPatterns(this.uow).length);
    }
    
    @Test
    public void testCreatesFromWorkspace() throws Exception {
    	setup(CREATE_COMMAND_5, CreateCommand.class);

    	execute();
    	
    	assertEquals("/tko:workspace", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    	
    	WorkspaceContext tableContext = ContextUtils.getContextForPath(wsStatus, "/tko:workspace/vdb_test_1/model_1/table_1"); //$NON-NLS-1$

    	KomodoObject ko = tableContext.getKomodoObj();
    	// Verify the komodo class is a Table and is TABLE type
    	assertEquals(KomodoType.TABLE.name(), ko.getTypeIdentifier(this.uow).name());
    	
    	Table table = (Table)resolveType(this.uow, ko, Table.class);
    	
    	// verify 3 columns
    	assertEquals(3, table.getColumns(this.uow).length);
    	
    	// verify PK
    	assertNotNull(table.getPrimaryKey(this.uow));
    	assertEquals("pk_1", table.getPrimaryKey(this.uow).getName(this.uow)); //$NON-NLS-1$
    	
    	// verify 2 access patterns
    	assertEquals(2, table.getAccessPatterns(this.uow).length);

    	// verify 1 foreign key
    	//assertEquals(1, table.getForeignKeys(null).length);

    }
    
    @Test
    /**
     * Creates at least one of each type of relational object within a Model
     * @throws Exception
     */
    public void testCreatesWithAbsolutePaths() throws Exception {
    	setup(CREATE_COMMAND_6, CreateCommand.class);

    	execute();
    	
    	assertEquals("/tko:workspace", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    	
    	WorkspaceContext tableContext = ContextUtils.getContextForPath(wsStatus, "/tko:workspace/vdb_test_1/model_1/table_1"); //$NON-NLS-1$

    	KomodoObject ko = tableContext.getKomodoObj();
    	// Verify the komodo class is a Table and is TABLE type
    	assertEquals(KomodoType.TABLE.name(), ko.getTypeIdentifier(this.uow).name());
    	
    	Table table = (Table)resolveType(this.uow, ko, Table.class);
    	
    	// verify 3 columns
    	assertEquals(3, table.getColumns(this.uow).length);
    	
    	// verify PK
    	assertNotNull(table.getPrimaryKey(this.uow));
    	assertEquals("pk_1", table.getPrimaryKey(this.uow).getName(this.uow)); //$NON-NLS-1$
    	
    	// verify 2 access patterns
    	assertEquals(2, table.getAccessPatterns(this.uow).length);

    	// verify 1 foreign key
    	//assertEquals(1, table.getForeignKeys(null).length);

    }
    
    @Test
    /**
     * Creates various types within a relational model
     * @throws Exception
     */
    public void testCreateAllRelational() throws Exception {
    	setup(CREATE_COMMAND_7, CreateCommand.class);

    	execute();
    	
    	assertEquals("/tko:workspace", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    	
    	WorkspaceContext modelContext = ContextUtils.getContextForPath(wsStatus, "/tko:workspace/MyModel"); //$NON-NLS-1$
    	KomodoObject ko = modelContext.getKomodoObj();
    	// Verify the komodo class is a Model and is MODEL type
    	assertEquals(KomodoType.MODEL.name(), ko.getTypeIdentifier(this.uow).name());

    	WorkspaceContext table1Context = ContextUtils.getContextForPath(wsStatus, "/tko:workspace/MyModel/MyTable1"); //$NON-NLS-1$
    	ko = table1Context.getKomodoObj();
    	// Verify the komodo class is a Model and is MODEL type
    	assertEquals(KomodoType.TABLE.name(), ko.getTypeIdentifier(this.uow).name());
    	
    	Table table1 = (Table)resolveType(this.uow, ko, Table.class);
    	
    	// verify 3 columns
    	assertEquals(3, table1.getColumns(this.uow).length);
    	
    	// verify PK
    	assertNotNull(table1.getPrimaryKey(this.uow));
    	assertEquals("MyPk", table1.getPrimaryKey(this.uow).getName(this.uow)); //$NON-NLS-1$

    	// verify 1 FK
    	assertEquals(1, table1.getForeignKeys(this.uow).length);
    	
    	// verify 2 access patterns
    	assertEquals(1, table1.getAccessPatterns(this.uow).length);

    	// verify 1 unique constrain
    	assertEquals(1, table1.getUniqueConstraints(this.uow).length);

    }
    
    @Test
    /**
     * Creates various types within a VDB
     * @throws Exception
     */
    public void testCreateAllVdb() throws Exception {
    	setup(CREATE_COMMAND_8, CreateCommand.class);

    	execute();
    	
    	assertEquals("/tko:workspace", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    	
    	WorkspaceContext vdbContext = ContextUtils.getContextForPath(wsStatus, "/tko:workspace/MyVdb1"); //$NON-NLS-1$
    	KomodoObject ko = vdbContext.getKomodoObj();
    	// Verify the komodo class is a Vdb and is VDB type
    	assertEquals(KomodoType.VDB.name(), ko.getTypeIdentifier(this.uow).name());
    	
    	Vdb vdb = (Vdb)resolveType(this.uow, ko, Vdb.class);
    	
    	// Verify one translator
    	assertEquals(1, vdb.getTranslators(this.uow).length);
    	
    	// Verify one DataRole
    	assertEquals(1, vdb.getDataRoles(this.uow).length);
    	
    	// Verify one Import
    	assertEquals(1, vdb.getImports(this.uow).length);
    	
    	// Verify one Entry
    	assertEquals(1, vdb.getEntries(this.uow).length);
    	
    }

}
