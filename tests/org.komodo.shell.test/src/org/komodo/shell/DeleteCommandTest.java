package org.komodo.shell;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.komodo.shell.commands.core.DeleteCommand;

public class DeleteCommandTest  extends AbstractCommandTest {

	private static final String DELETE_COMMAND_1 = "deleteCommand1.txt"; //$NON-NLS-1$
	private static final String DELETE_COMMAND_2 = "deleteCommand2.txt"; //$NON-NLS-1$
	private static final String DELETE_COMMAND_3 = "deleteCommand3.txt"; //$NON-NLS-1$
	
	/**
	 * Test for CreateCommand
	 */
	public DeleteCommandTest( ) {
		super();
	}
	
    @Test
    public void testDeleteVdb() throws Exception {
    	setup(DELETE_COMMAND_1, DeleteCommand.class);

    	execute();

    	assertEquals("tko:komodo/tko:workspace", wsStatus.getCurrentContext().getFullName());
    	assertEquals(0, wsStatus.getCurrentContext().getKomodoObj().getChildren(null).length);
    }
    
    @Test
    public void testDeleteModel() throws Exception {
    	setup(DELETE_COMMAND_2, DeleteCommand.class);

    	execute();

    	assertEquals("tko:komodo/tko:workspace/vdb_test_1", wsStatus.getCurrentContext().getFullName());
    	assertEquals(0, wsStatus.getCurrentContext().getKomodoObj().getChildren(null).length);
    }
    
    
    @Test
    public void testDeleteTable() throws Exception {
    	setup(DELETE_COMMAND_3, DeleteCommand.class);

    	execute();

    	assertEquals("tko:komodo/tko:workspace/vdb_test_1/model_1", wsStatus.getCurrentContext().getFullName());
    	assertEquals(0, wsStatus.getCurrentContext().getKomodoObj().getChildren(null).length);
    }

}
