package org.komodo.shell;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.Test;
import org.komodo.relational.model.Table;
import org.komodo.shell.commands.core.CreateCommand;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;

public class CreateCommandTest extends AbstractCommandTest {

	private static final String CREATE_COMMAND_1 = "createCommand1.txt"; //$NON-NLS-1$
	private static final String CREATE_COMMAND_2 = "createCommand2.txt"; //$NON-NLS-1$
	private static final String CREATE_COMMAND_3 = "createCommand3.txt"; //$NON-NLS-1$
	private static final String CREATE_COMMAND_4 = "createCommand4.txt"; //$NON-NLS-1$
	
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

    	assertEquals("tko:komodo/tko:workspace/vdb_test_1", wsStatus.getCurrentContext().getFullName());
    }
    
    @Test
    public void testCreateModel() throws Exception {
    	setup(CREATE_COMMAND_2, CreateCommand.class);

    	execute();

    	assertEquals("tko:komodo/tko:workspace/vdb_test_1/model_1", wsStatus.getCurrentContext().getFullName());
    }
    
    @Test
    public void testCreateTable() throws Exception {
    	setup(CREATE_COMMAND_3, CreateCommand.class);

    	execute();

    	assertEquals("tko:komodo/tko:workspace/vdb_test_1/model_1/table_1", wsStatus.getCurrentContext().getFullName());
    }
    
    @Test
    public void testCreateThreeColumns() throws Exception {
    	setup(CREATE_COMMAND_4, CreateCommand.class);

    	execute();
    	
    	assertEquals("tko:komodo/tko:workspace/vdb_test_1/model_1/table_1", wsStatus.getCurrentContext().getFullName());
    	
    	KomodoObject ko = wsStatus.getCurrentContext().getKomodoObj();
    	// Verify the komodo class is a Table and is TABLE type
    	assertEquals(KomodoType.TABLE.name(), ko.getTypeIdentifier(null).name());
    	
    	Table table = (Table)resolveType(ko, Table.class);
    	
    	// verify 3 columns
    	assertEquals(3, table.getColumns(null).length);
    	
    	// verify PK
    	assertNotNull(table.getPrimaryKey(null));
    	assertEquals("pk_1", table.getPrimaryKey(null).getName(null));
    	
    	// verify 2 access patterns
    	assertEquals(2, table.getAccessPatterns(null).length);

    	// verify 1 foreign key
    	assertEquals(1, table.getForeignKeys(null).length);

    }
}
