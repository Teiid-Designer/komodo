package org.komodo.shell;

import static org.junit.Assert.assertEquals;

import java.io.File;

import org.junit.Test;
import org.komodo.relational.model.Table;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.commands.core.SetCommand;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;

/**
 * SetCommand - allows setting of properties, global properties, recording state, etc.
 * 
 */
@SuppressWarnings("javadoc")
public class SetCommandTest extends AbstractCommandTest {

	private static final String SET_COMMAND_1 = "setCommand1.txt"; //$NON-NLS-1$
	private static final String SET_COMMAND_2 = "setCommand2.txt"; //$NON-NLS-1$
	private static final String SET_COMMAND_3 = "setCommand3.txt"; //$NON-NLS-1$
	private static final String SET_COMMAND_4 = "setCommand4.txt"; //$NON-NLS-1$
	private static final String SET_COMMAND_5 = "setCommand5.txt"; //$NON-NLS-1$
	
	/**
	 * Test for SetCommand
	 */
	public SetCommandTest( ) {
		super();
	}
	
    @Test
    public void testSetProperty1() throws Exception {
    	setup(SET_COMMAND_1, SetCommand.class);

    	execute();

    	assertEquals("tko:komodo/tko:workspace/MyModel/MyTable", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    	
    	KomodoObject ko = wsStatus.getCurrentContext().getKomodoObj();
    	// Verify the komodo class is a Table and is TABLE type
    	assertEquals(KomodoType.TABLE.name(), ko.getTypeIdentifier(null).name());
    	
    	Table table = (Table)resolveType(ko, Table.class);
    	Property prop = table.getProperty(null, "ddl:length"); //$NON-NLS-1$
    	
    	assertEquals(99, prop.getValue(null));
    }
    
    @Test
    public void testSetProperty2() throws Exception {
    	setup(SET_COMMAND_2, SetCommand.class);

    	execute();

    	assertEquals("tko:komodo/tko:workspace", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    	
    	WorkspaceContext tableContext = ContextUtils.getContextForPath(wsStatus, "tko:komodo/tko:workspace/MyModel/MyTable"); //$NON-NLS-1$
    	KomodoObject ko = tableContext.getKomodoObj();
    	// Verify the komodo class is a Table and is TABLE type
    	assertEquals(KomodoType.TABLE.name(), ko.getTypeIdentifier(null).name());
    	
    	Table table = (Table)resolveType(ko, Table.class);
    	Property prop = table.getProperty(null, "ddl:length"); //$NON-NLS-1$
    	
    	assertEquals(99, prop.getValue(null));
    }
    
    @Test
    public void testSetProperty3() throws Exception {
    	setup(SET_COMMAND_3, SetCommand.class);

    	execute();

    	assertEquals("tko:komodo/tko:workspace", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    	
    	WorkspaceContext tableContext = ContextUtils.getContextForPath(wsStatus, "tko:komodo/tko:workspace/MyModel/MyTable"); //$NON-NLS-1$
    	KomodoObject ko = tableContext.getKomodoObj();
    	// Verify the komodo class is a Table and is TABLE type
    	assertEquals(KomodoType.TABLE.name(), ko.getTypeIdentifier(null).name());
    	
    	Table table = (Table)resolveType(ko, Table.class);
    	Property prop = table.getProperty(null, "ddl:length"); //$NON-NLS-1$
    	
    	assertEquals(99, prop.getValue(null));
    }
    
    @Test
    public void testSetGlobal() throws Exception {
    	setup(SET_COMMAND_4, SetCommand.class);

    	execute();

    	File recordFile = wsStatus.getRecordingOutputFile();
    	assertEquals("/home/me/BogusFile.txt", recordFile.getCanonicalPath()); //$NON-NLS-1$
    }
    
    @Test
    public void testSetRecord() throws Exception {
    	setup(SET_COMMAND_5, SetCommand.class);

    	execute();

    	wsStatus.getRecordingStatus();
    	assertEquals(true, wsStatus.getRecordingStatus());
    }
    
}
