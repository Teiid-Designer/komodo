package org.komodo.shell;

import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.commands.core.DeleteCommand;
import org.komodo.shell.util.ContextUtils;

@SuppressWarnings("javadoc")
public class DeleteCommandTest  extends AbstractCommandTest {

	private static final String DELETE_COMMAND_1 = "deleteCommand1.txt"; //$NON-NLS-1$
	private static final String DELETE_COMMAND_2 = "deleteCommand2.txt"; //$NON-NLS-1$
	private static final String DELETE_COMMAND_3 = "deleteCommand3.txt"; //$NON-NLS-1$
	private static final String DELETE_COMMAND_4 = "deleteCommand4.txt"; //$NON-NLS-1$
	private static final String DELETE_COMMAND_5 = "deleteCommand5.txt"; //$NON-NLS-1$

    @Test
    public void testDeleteVdb() throws Exception {
    	setup(DELETE_COMMAND_1, DeleteCommand.class);

    	execute();

    	assertEquals("/tko:workspace", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    	assertEquals(0, wsStatus.getCurrentContext().getKomodoObj().getChildren(this.uow).length);
    }

    @Test
    public void testDeleteModel() throws Exception {
    	setup(DELETE_COMMAND_2, DeleteCommand.class);

    	execute();

    	assertEquals("/tko:workspace/vdb_test_1", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    	assertEquals(0, wsStatus.getCurrentContext().getKomodoObj().getChildren(this.uow).length);
    }


    @Test
    public void testDeleteTable() throws Exception {
    	setup(DELETE_COMMAND_3, DeleteCommand.class);

    	execute();

    	assertEquals("/tko:workspace/vdb_test_1/model_1", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    	assertEquals(0, wsStatus.getCurrentContext().getKomodoObj().getChildren(this.uow).length);
    }
    
    @Test
    public void testDeleteAbsolutePath1() throws Exception {
    	setup(DELETE_COMMAND_4, DeleteCommand.class);

    	execute();

    	assertEquals("/tko:workspace", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    	
    	// Model child table was deleted
    	WorkspaceContext context = ContextUtils.getContextForPath(wsStatus, "/tko:workspace/vdb_test_1/model_1"); //$NON-NLS-1$
    	assertEquals(0, context.getKomodoObj().getChildren(uow).length);
    }
    
    @Test
    public void testDeleteAbsolutePath2() throws Exception {
    	setup(DELETE_COMMAND_5, DeleteCommand.class);

    	execute();

    	assertEquals("/tko:workspace", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    	
    	// VDB child model was deleted
    	WorkspaceContext context = ContextUtils.getContextForPath(wsStatus, "/tko:workspace/vdb_test_1"); //$NON-NLS-1$
    	assertEquals(0, context.getKomodoObj().getChildren(uow).length);
    }

}
