package org.komodo.shell;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsCollectionContaining.hasItems;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import java.io.File;
import java.util.Arrays;
import org.junit.Test;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.PrimaryKey;
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
    private static final String SET_CONSTRAINT_TYPE = "setConstraintTypeProperty.txt"; //$NON-NLS-1$
    private static final String SET_PRIMARY_KEY_COLS = "setPrimaryKeyColumns.txt"; //$NON-NLS-1$
    private static final String REPLACE_PRIMARY_KEY_COLS = "replacePrimaryKeyColumns.txt"; //$NON-NLS-1$
    private static final String SET_FOREIGN_KEY = "setForeignKeyReferenceTableAndColumns.txt"; //$NON-NLS-1$
    private static final String REMOVE_FOREIGN_KEY_REF_COLS = "removeForeignKeyReferenceTableColumns.txt"; //$NON-NLS-1$

    @Test
    public void shouldNotBeAbleToSetTableConstraintType() throws Exception {
        setup( SET_CONSTRAINT_TYPE, SetCommand.class );

        try {
            execute();
            fail();
        } catch ( final Throwable e ) {
            assertThat( getCommandOutput().endsWith( "The property \"teiidddl:constraintType\" is not valid or cannot be modified\n" ),
                        is( true ) );
        }
    }

    @Test
    public void shouldRemoveExistingColumnsWhenChangingForeignKeyTableReference() throws Exception {
        // see https://github.com/Teiid-Designer/komodo/issues/149
        setup( REMOVE_FOREIGN_KEY_REF_COLS, SetCommand.class );
        execute();
        assertThat( this.wsStatus.getCurrentContext().getFullName(), is( "/workspace/PartsVDB/PartsOracle/PARTS/fk" ) ); //$NON-NLS-1$

        final UnitOfWork transaction = this.wsStatus.getTransaction();
        final KomodoObject kobject = this.wsStatus.getCurrentContext().getKomodoObj();
        assertThat( kobject.getTypeIdentifier( transaction ).name(), is( KomodoType.FOREIGN_KEY.name() ) );

        final ForeignKey fk = ( ForeignKey )resolveType( transaction, kobject, ForeignKey.class );
        final Table refTable = fk.getReferencesTable( transaction );
        assertThat( ContextUtils.convertPathToDisplayPath( refTable.getAbsolutePath() ),
                    is( "/workspace/PartsVDB/PartsOracle/STATUS" ) );

        final Column[] refColumns = fk.getReferencesColumns( transaction );
        assertThat( refColumns.length, is( 0 ) );
    }

    @Test
    public void shouldReplacePrimaryKeyColumns() throws Exception {
        // see https://github.com/Teiid-Designer/komodo/issues/211
        setup( REPLACE_PRIMARY_KEY_COLS, SetCommand.class );
        execute();
        assertThat( this.wsStatus.getCurrentContext().getFullName(), is( "/workspace/MyVdb/MyModel/MyTable/pk" ) ); //$NON-NLS-1$

        final UnitOfWork transaction = this.wsStatus.getTransaction();
        final KomodoObject kobject = this.wsStatus.getCurrentContext().getKomodoObj();
        assertThat( kobject.getTypeIdentifier( transaction ).name(), is( KomodoType.PRIMARY_KEY.name() ) );

        final PrimaryKey pk = ( PrimaryKey )resolveType( transaction, kobject, PrimaryKey.class );
        assertThat( pk.getColumns( transaction ).length, is( 1 ) );

        final Column column = pk.getColumns( transaction )[ 0 ];
        assertThat( ContextUtils.convertPathToDisplayPath( column.getAbsolutePath() ),
                    is( "/workspace/MyVdb/MyModel/MyTable/LastName" ) );
    }

    @Test
    public void shouldSetForeignKey() throws Exception {
        // see https://github.com/Teiid-Designer/komodo/issues/149
        setup( SET_FOREIGN_KEY, SetCommand.class );
        execute();
        assertThat( this.wsStatus.getCurrentContext().getFullName(), is( "/workspace/PartsVDB/PartsOracle/PARTS/fk" ) ); //$NON-NLS-1$

        final UnitOfWork transaction = this.wsStatus.getTransaction();
        final KomodoObject kobject = this.wsStatus.getCurrentContext().getKomodoObj();
        assertThat( kobject.getTypeIdentifier( transaction ).name(), is( KomodoType.FOREIGN_KEY.name() ) );

        final ForeignKey fk = ( ForeignKey )resolveType( transaction, kobject, ForeignKey.class );
        final Table refTable = fk.getReferencesTable( transaction );
        assertThat( ContextUtils.convertPathToDisplayPath( refTable.getAbsolutePath() ),
                    is( "/workspace/PartsVDB/PartsOracle/SHIP_VIA" ) );

        final Column[] refColumns = fk.getReferencesColumns( transaction );
        assertThat( refColumns.length, is( 2 ) );

        final String[] paths = new String[] { ContextUtils.convertPathToDisplayPath( refColumns[ 0 ].getAbsolutePath() ),
                                              ContextUtils.convertPathToDisplayPath( refColumns[ 1 ].getAbsolutePath() ) };
        assertThat( Arrays.asList( paths ),
                    hasItems( "/workspace/PartsVDB/PartsOracle/SHIP_VIA/SHIPPER_ID",
                              "/workspace/PartsVDB/PartsOracle/SHIP_VIA/SHIPPER_NAME" ) );
    }

    @Test
    public void shouldSetPrimaryKeyColumns() throws Exception {
        // see https://github.com/Teiid-Designer/komodo/issues/211
        setup( SET_PRIMARY_KEY_COLS, SetCommand.class );
        execute();
        assertThat( this.wsStatus.getCurrentContext().getFullName(), is( "/workspace/MyVdb/MyModel/MyTable/pk" ) ); //$NON-NLS-1$

        final UnitOfWork transaction = this.wsStatus.getTransaction();
        final KomodoObject kobject = this.wsStatus.getCurrentContext().getKomodoObj();
        assertThat( kobject.getTypeIdentifier( transaction ).name(), is( KomodoType.PRIMARY_KEY.name() ) );

        final PrimaryKey pk = ( PrimaryKey )resolveType( transaction, kobject, PrimaryKey.class );
        assertThat( pk.getColumns( transaction ).length, is( 2 ) );

        final Column[] columns = pk.getColumns( transaction );
        final String[] paths = new String[] { ContextUtils.convertPathToDisplayPath( columns[ 0 ].getAbsolutePath() ),
                                              ContextUtils.convertPathToDisplayPath( columns[ 1 ].getAbsolutePath() ) };
        assertThat( Arrays.asList( paths ),
                    hasItems( "/workspace/MyVdb/MyModel/MyTable/FirstName", "/workspace/MyVdb/MyModel/MyTable/LastName" ) );
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
