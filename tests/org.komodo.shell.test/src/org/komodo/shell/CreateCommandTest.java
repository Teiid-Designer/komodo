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
import org.komodo.spi.repository.Repository.UnitOfWork;

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

    	assertEquals("/workspace/vdb_test_1", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

    @Test
    public void testCreateModel() throws Exception {
    	setup(CREATE_COMMAND_2, CreateCommand.class);

    	execute();

    	assertEquals("/workspace/vdb_test_1/model_1", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

    @Test
    public void testCreateTable() throws Exception {
    	setup(CREATE_COMMAND_3, CreateCommand.class);

    	execute();

    	assertEquals("/workspace/vdb_test_1/model_1/table_1", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

    @Test
    public void testCreateThreeColumns() throws Exception {
    	setup(CREATE_COMMAND_4, CreateCommand.class);

    	execute();

    	assertEquals("/workspace/vdb_test_1/model_1/table_1", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$

    	KomodoObject ko = wsStatus.getCurrentContext().getKomodoObj();
    	UnitOfWork trans = wsStatus.getTransaction();
    	// Verify the komodo class is a Table and is TABLE type
    	assertEquals(KomodoType.TABLE.name(), ko.getTypeIdentifier(trans).name());

    	Table table = (Table)resolveType(trans, ko, Table.class);

    	// verify 3 columns
    	assertEquals(3, table.getColumns(trans).length);

    	// verify PK
    	assertNotNull(table.getPrimaryKey(trans));
    	assertEquals("pk_1", table.getPrimaryKey(trans).getName(trans)); //$NON-NLS-1$

    	// verify 2 access patterns
    	assertEquals(2, table.getAccessPatterns(trans).length);
    }

    @Test
    public void testCreatesFromWorkspace() throws Exception {
    	setup(CREATE_COMMAND_5, CreateCommand.class);

    	execute();

    	assertEquals("/workspace", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$

    	WorkspaceContext tableContext = ContextUtils.getContextForPath(wsStatus, "/workspace/vdb_test_1/model_1/table_1"); //$NON-NLS-1$

    	KomodoObject ko = tableContext.getKomodoObj();
    	UnitOfWork trans = wsStatus.getTransaction();
    	// Verify the komodo class is a Table and is TABLE type
    	assertEquals(KomodoType.TABLE.name(), ko.getTypeIdentifier(trans).name());

    	Table table = (Table)resolveType(trans, ko, Table.class);

    	// verify 3 columns
    	assertEquals(3, table.getColumns(trans).length);

    	// verify PK
    	assertNotNull(table.getPrimaryKey(trans));
    	assertEquals("pk_1", table.getPrimaryKey(trans).getName(trans)); //$NON-NLS-1$

    	// verify 2 access patterns
    	assertEquals(2, table.getAccessPatterns(trans).length);

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

    	assertEquals("/workspace", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$

    	WorkspaceContext tableContext = ContextUtils.getContextForPath(wsStatus, "/workspace/vdb_test_1/model_1/table_1"); //$NON-NLS-1$

    	KomodoObject ko = tableContext.getKomodoObj();
    	UnitOfWork trans = wsStatus.getTransaction();
    	// Verify the komodo class is a Table and is TABLE type
    	assertEquals(KomodoType.TABLE.name(), ko.getTypeIdentifier(trans).name());

    	Table table = (Table)resolveType(trans, ko, Table.class);

    	// verify 3 columns
    	assertEquals(3, table.getColumns(trans).length);

    	// verify PK
    	assertNotNull(table.getPrimaryKey(trans));
    	assertEquals("pk_1", table.getPrimaryKey(trans).getName(trans)); //$NON-NLS-1$

    	// verify 2 access patterns
    	assertEquals(2, table.getAccessPatterns(trans).length);

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

    	assertEquals("/workspace", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$

    	WorkspaceContext modelContext = ContextUtils.getContextForPath(wsStatus, "/workspace/MyVdb/MyModel"); //$NON-NLS-1$
    	KomodoObject ko = modelContext.getKomodoObj();
    	UnitOfWork trans = wsStatus.getTransaction();
    	// Verify the komodo class is a Model and is MODEL type
    	assertEquals(KomodoType.MODEL.name(), ko.getTypeIdentifier(trans).name());

    	WorkspaceContext table1Context = ContextUtils.getContextForPath(wsStatus, "/workspace/MyVdb/MyModel/MyTable1"); //$NON-NLS-1$
    	ko = table1Context.getKomodoObj();
    	// Verify the komodo class is a Model and is MODEL type
    	assertEquals(KomodoType.TABLE.name(), ko.getTypeIdentifier(trans).name());

    	Table table1 = (Table)resolveType(trans, ko, Table.class);

    	// verify 3 columns
    	assertEquals(3, table1.getColumns(trans).length);

    	// verify PK
    	assertNotNull(table1.getPrimaryKey(trans));
    	assertEquals("MyPk", table1.getPrimaryKey(trans).getName(trans)); //$NON-NLS-1$

    	// verify 1 FK
    	assertEquals(1, table1.getForeignKeys(trans).length);

    	// verify 2 access patterns
    	assertEquals(1, table1.getAccessPatterns(trans).length);

    	// verify 1 unique constrain
    	assertEquals(1, table1.getUniqueConstraints(trans).length);

    }

    @Test
    /**
     * Creates various types within a VDB
     * @throws Exception
     */
    public void testCreateAllVdb() throws Exception {
    	setup(CREATE_COMMAND_8, CreateCommand.class);

    	execute();

    	assertEquals("/workspace", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$

    	WorkspaceContext vdbContext = ContextUtils.getContextForPath(wsStatus, "/workspace/MyVdb1"); //$NON-NLS-1$
    	KomodoObject ko = vdbContext.getKomodoObj();
    	UnitOfWork trans = wsStatus.getTransaction();
    	// Verify the komodo class is a Vdb and is VDB type
    	assertEquals(KomodoType.VDB.name(), ko.getTypeIdentifier(trans).name());

    	Vdb vdb = (Vdb)resolveType(trans, ko, Vdb.class);

    	// Verify one translator
    	assertEquals(1, vdb.getTranslators(trans).length);

    	// Verify one DataRole
    	assertEquals(1, vdb.getDataRoles(trans).length);

    	// Verify one Import
    	assertEquals(1, vdb.getImports(trans).length);

    	// Verify one Entry
    	assertEquals(1, vdb.getEntries(trans).length);

    }

}
