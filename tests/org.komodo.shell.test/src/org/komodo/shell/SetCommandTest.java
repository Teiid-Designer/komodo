package org.komodo.shell;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import java.io.File;
import org.junit.Test;
import org.komodo.relational.model.Table;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.core.SetCommand;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * SetCommand - allows setting of properties, global properties, recording state, etc.
 *
 */
@SuppressWarnings({"javadoc", "nls"})
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

    	assertEquals("/workspace/MyVdb/MyModel/MyTable", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$

    	KomodoObject ko = wsStatus.getCurrentContext().getKomodoObj();
    	UnitOfWork trans = wsStatus.getTransaction();
     	// Verify the komodo class is a Table and is TABLE type
    	assertEquals(KomodoType.TABLE.name(), ko.getTypeIdentifier(trans).name());

    	Table table = (Table)resolveType(trans, ko, Table.class);
    	assertEquals("mydescription", table.getDescription( trans ));
    }

    @Test
    public void testSetProperty2() throws Exception {
    	setup(SET_COMMAND_2, SetCommand.class);

    	execute();

    	assertEquals("/workspace", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$

    	WorkspaceContext tableContext = ContextUtils.getContextForPath(wsStatus, "/workspace/MyVdb/MyModel/MyTable"); //$NON-NLS-1$
    	assertNotNull(tableContext);

    	KomodoObject ko = tableContext.getKomodoObj();
    	UnitOfWork trans = wsStatus.getTransaction();
    	// Verify the komodo class is a Table and is TABLE type
    	assertEquals(KomodoType.TABLE.name(), ko.getTypeIdentifier(trans).name());

    	Table table = (Table)resolveType(trans, ko, Table.class);
    	assertEquals("mydescription", table.getDescription( trans ));
    }

    @Test
    public void testSetProperty3() throws Exception {
    	setup(SET_COMMAND_3, SetCommand.class);

    	execute();

    	assertEquals("/workspace", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$

    	WorkspaceContext tableContext = ContextUtils.getContextForPath(wsStatus, "/workspace/MyVdb/MyModel/MyTable"); //$NON-NLS-1$
        assertNotNull(tableContext);

    	KomodoObject ko = tableContext.getKomodoObj();
    	UnitOfWork trans = wsStatus.getTransaction();
    	// Verify the komodo class is a Table and is TABLE type
    	assertEquals(KomodoType.TABLE.name(), ko.getTypeIdentifier(trans).name());

    	Table table = (Table)resolveType(trans, ko, Table.class);
    	assertEquals(5, table.getCardinality(trans));
    }

    @Test
    public void testSetGlobal() throws Exception {
    	setup(SET_COMMAND_4, SetCommand.class);

    	execute();

    	File recordFile = wsStatus.getRecordingOutputFile();
    	assertEquals("BogusFile.txt", recordFile.getName()); //$NON-NLS-1$
    }

    @Test
    public void testSetRecord() throws Exception {
        setup(SET_COMMAND_5, SetCommand.class);
        wsStatus.setProperty(WorkspaceStatus.RECORDING_FILE_KEY, "/tmp/recordingFile"); //$NON-NLS-1$

        execute();

        assertEquals(true, wsStatus.getRecordingStatus());
    }

    @Test
    public void testResetGlobalProperties() throws Exception {
        setup( "setCommandResetGlobalProps.txt", SetCommand.class );
        execute();

        for ( final String propName : WorkspaceStatus.GLOBAL_PROPS.keySet() ) {
            assertThat( this.wsStatus.getProperties().getProperty( propName ), is( WorkspaceStatus.GLOBAL_PROPS.get( propName ) ) );
        }
    }

    @Test
    public void testSetGlobalProperties() throws Exception {
        setup( "setCommandGlobalProps.txt", SetCommand.class );
        execute();
        assertThat( this.wsStatus.getProperties().getProperty( WorkspaceStatus.EXPORT_DEFAULT_DIR_KEY ), is( "/export/directory" ) );
        assertThat( this.wsStatus.getProperties().getProperty( WorkspaceStatus.IMPORT_DEFAULT_DIR_KEY ), is( "/import/directory" ) );
        assertThat( this.wsStatus.getRecordingOutputFile().getAbsolutePath(), is( "/recording/file.txt" ) );
        assertThat( this.wsStatus.isShowingFullPathInPrompt(), is( true ) );
        assertThat( this.wsStatus.isShowingFullPathInPrompt(), is( true ) );
        assertThat( this.wsStatus.isShowingHiddenProperties(), is( true ) );
        assertThat( this.wsStatus.isShowingPropertyNamePrefixes(), is( true ) );
        assertThat( this.wsStatus.isShowingTypeInPrompt(), is( false ) );
    }


}
