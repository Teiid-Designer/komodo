/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.workspace;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsCollectionContaining.hasItems;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import java.util.Arrays;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalProperty;
import org.komodo.relational.model.AccessPattern;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Function;
import org.komodo.relational.model.Index;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.ProcedureResultSet;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.relational.model.ResultSetColumn;
import org.komodo.relational.model.Schema;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.relational.model.UserDefinedFunction;
import org.komodo.relational.model.View;
import org.komodo.relational.model.VirtualProcedure;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Entry;
import org.komodo.relational.vdb.Mask;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class WorkspaceManagerTest extends RelationalModelTest {

    private WorkspaceManager wsMgr;

    @Before
    public void obtainWorkspaceManager() throws Exception {
        wsMgr = WorkspaceManager.getInstance(_repo);
    }

    @After
    public void uncacheWorkspaceManager() {
        WorkspaceManager.uncacheInstance(_repo);
        wsMgr = null;
    }

    @Test
    public void shouldCreateModel() throws Exception {
        final Vdb vdb = createVdb();
        final Model model = this.wsMgr.createModel( this.uow, vdb, "model" );
        assertThat( model, is( notNullValue() ) );
        assertThat( _repo.getFromWorkspace( this.uow, model.getAbsolutePath() ), is( ( KomodoObject )model ) );
    }

    @Test
    public void shouldCreateSchema() throws Exception {
        final Schema schema = this.wsMgr.createSchema( this.uow, null, "schema" );
        assertThat( schema, is( notNullValue() ) );
        assertThat( _repo.getFromWorkspace( this.uow, schema.getAbsolutePath() ), is( ( KomodoObject )schema ) );
    }

    @Test
    public void shouldCreateVdb() throws Exception {
        final KomodoObject parent = _repo.add(this.uow, _repo.komodoWorkspace(this.uow).getAbsolutePath(), "parent", null);
        final Vdb vdb = this.wsMgr.createVdb(this.uow, parent, "vdb", "externalFilePath");
        assertThat(vdb, is(notNullValue()));
        assertThat(_repo.getFromWorkspace(this.uow, vdb.getAbsolutePath()), is((KomodoObject)vdb));
    }

    @Test
    public void shouldCreateVdbWithNullParent() throws Exception {
        final Vdb vdb = this.wsMgr.createVdb(this.uow, null, this.name.getMethodName(), "externalFilePath");
        assertThat(vdb, is(notNullValue()));
        assertThat(vdb.getParent(this.uow), is(_repo.komodoWorkspace(this.uow)));
    }

    @Test
    public void shouldDeleteModel() throws Exception {
        final Model model = createModel();
        this.wsMgr.delete(this.uow, model);
        assertThat(this.wsMgr.findModels(this.uow).length, is(0));
    }

    @Test
    public void shouldDeleteVdb() throws Exception {
        final Vdb vdb = createVdb();
        this.wsMgr.delete(this.uow, vdb);
        assertThat(this.wsMgr.findVdbs(this.uow).length, is(0));
    }

    @Test
    public void shouldFindAllObjectsOfTypeInWorkspaceWhenPassingInEmptyParentPath() throws Exception {
        final String prefix = this.name.getMethodName();
        int suffix = 0;

        // create at root
        for ( int i = 0; i < 5; ++i ) {
            createVdb( ( prefix + ++suffix ), ( VDB_PATH + i ) );
        }

        // create under a folder
        final KomodoObject parent = _repo.add( this.uow, null, "blah", null );

        for ( int i = 0; i < 7; ++i ) {
            createVdb( ( prefix + ++suffix ), parent, ( VDB_PATH + i ) );
        }

        commit(); // must save before running a query

        assertThat( this.wsMgr.findByType( this.uow, VdbLexicon.Vdb.VIRTUAL_DATABASE, StringConstants.EMPTY_STRING ).length,
                    is( suffix ) );
    }

    @Test
    public void shouldFindAllObjectsOfTypeUnderASpecificParentPath() throws Exception {
        final String prefix = this.name.getMethodName();
        final int numAtRoot = 3;

        // create at root
        for ( int i = 0; i < numAtRoot; ++i ) {
            createVdb( ( prefix + i ), ( VDB_PATH + i ) );
        }

        // create under a folder
        final KomodoObject parent = _repo.add( this.uow, null, "blah", null );
        final int expected = 3;

        for ( int i = numAtRoot; i < ( numAtRoot + expected ); ++i ) {
            createVdb( ( prefix + i ), parent, ( VDB_PATH + i ) );
        }

        commit(); // must save before running a query

        assertThat( this.wsMgr.findByType( this.uow, VdbLexicon.Vdb.VIRTUAL_DATABASE, parent.getAbsolutePath() ).length,
                    is( expected ) );
    }

    @Test
    public void shouldFindModels() throws Exception {
        Vdb parent = createVdb();
        final String prefix = this.name.getMethodName();
        int suffix = 0;

        // create at root
        for (int i = 0; i < 5; ++i) {
            parent.addModel(this.uow, (prefix + ++suffix));
        }

        // create under a folder
        final KomodoObject folder = _repo.add(this.uow, parent.getAbsolutePath(), "folder", null);
        assertNotNull(_repo.getFromWorkspace(this.uow, folder.getAbsolutePath()));

        for (int i = 0; i < 7; ++i) {
            suffix++;
            parent = createVdb( "vdb" + suffix, folder, (VDB_PATH + i) );
            parent.addModel(this.uow, (prefix + suffix));
        }

        commit(); // must save before running a query

        assertThat(this.wsMgr.findModels(this.uow).length, is(suffix));
    }

    @Test
    public void shouldFindVdbs() throws Exception {
        final String prefix = this.name.getMethodName();
        int suffix = 0;

        // create at root
        for (int i = 0; i < 5; ++i) {
            createVdb((prefix + ++suffix), (VDB_PATH + i));
        }

        // create under a folder
        final KomodoObject parent = _repo.add(this.uow, null, "blah", null);

        for (int i = 0; i < 7; ++i) {
            createVdb( ( prefix + ++suffix ), parent, ( VDB_PATH + i ) );
        }

        commit(); // must save before running a query

        assertThat(this.wsMgr.findVdbs(this.uow).length, is(suffix));
    }

    @Test
    public void shouldHaveCorrectChildTypes() {
        assertThat( Arrays.asList( this.wsMgr.getChildTypes() ), hasItems( Vdb.IDENTIFIER, Schema.IDENTIFIER ) );
        assertThat( this.wsMgr.getChildTypes().length, is( 2 ) );
    }

    @Test( expected = Exception.class )
    public void shouldNotAllowEmptyExternalFilePath() throws Exception {
        this.wsMgr.createVdb(this.uow, null, "vdbName", StringConstants.EMPTY_STRING);
    }

    @Test( expected = Exception.class )
    public void shouldNotAllowEmptyTypeWhenFindingObjects() throws Exception {
        this.wsMgr.findByType(this.uow, StringConstants.EMPTY_STRING, "/my/path");
    }

    @Test( expected = Exception.class )
    public void shouldNotAllowEmptyVdbName() throws Exception {
        this.wsMgr.createVdb(this.uow, null, StringConstants.EMPTY_STRING, "externalFilePath");
    }

    @Test( expected = Exception.class )
    public void shouldNotAllowNullExternalFilePath() throws Exception {
        this.wsMgr.createVdb(this.uow, null, "vdbName", null);
    }

    @Test( expected = Exception.class )
    public void shouldNotAllowNullTypeWhenFindingObjects() throws Exception {
        this.wsMgr.findByType(this.uow, null, "/my/path");
    }

    @Test( expected = Exception.class )
    public void shouldNotAllowNullVdbName() throws Exception {
        this.wsMgr.createVdb(this.uow, null, null, "externalFilePath");
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowRemove() throws Exception {
        this.wsMgr.remove( this.uow );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowRename() throws Exception {
        this.wsMgr.rename( this.uow, "newName" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotAllowCreateModelWithNullParent() throws Exception {
        this.wsMgr.createModel( this.uow, null, "model" );
    }

    @Test
    public void shouldNotFindModelsWhenWorkspaceIsEmpty() throws Exception {
        assertThat(this.wsMgr.findModels(this.uow).length, is(0));
    }

    @Test
    public void shouldNotFindSchemasWhenWorkspaceIsEmpty() throws Exception {
        assertThat(this.wsMgr.findSchemas(this.uow).length, is(0));
    }

    @Test
    public void shouldNotFindTeiidsWhenWorkspaceIsEmpty() throws Exception {
        assertThat(this.wsMgr.findTeiids(this.uow).isEmpty(), is(true));
    }

    @Test
    public void shouldNotFindVdbsWhenWorkspaceIsEmpty() throws Exception {
        assertThat(this.wsMgr.findVdbs(this.uow).length, is(0));
    }

    @Test
    public void shouldNotResolveAsVdb() throws Exception {
        final Model model = createModel();
        final KomodoObject kobject = new ObjectImpl(_repo, model.getAbsolutePath(), model.getIndex());
        assertNull(this.wsMgr.resolve(this.uow, kobject, Vdb.class));
    }

    @Test
    public void shouldResolveAccessPattern() throws Exception {
        final Table table = createTable();
        final AccessPattern accessPattern = table.addAccessPattern(this.uow, "accessPattern");
        final KomodoObject kobject = new ObjectImpl(_repo, accessPattern.getAbsolutePath(), accessPattern.getIndex());
        assertThat(this.wsMgr.resolve(this.uow, kobject, AccessPattern.class), is(instanceOf(AccessPattern.class)));
    }

    @Test
    public void shouldResolveCondition() throws Exception {
        final Vdb vdb = createVdb();
        final DataRole dataRole = vdb.addDataRole(this.uow, "dataRole");
        final Permission permission = dataRole.addPermission(this.uow, "permission");
        final Condition condition = permission.addCondition(this.uow, "condition");
        final KomodoObject kobject = new ObjectImpl(_repo, condition.getAbsolutePath(), condition.getIndex());
        assertThat(this.wsMgr.resolve(this.uow, kobject, Condition.class), is(instanceOf(Condition.class)));
    }

    @Test
    public void shouldResolveDataRole() throws Exception {
        final Vdb vdb = createVdb();
        final DataRole dataRole = vdb.addDataRole(this.uow, "dataRole");
        final KomodoObject kobject = new ObjectImpl(_repo, dataRole.getAbsolutePath(), dataRole.getIndex());
        assertThat(this.wsMgr.resolve(this.uow, kobject, DataRole.class), is(instanceOf(DataRole.class)));
    }

    @Test
    public void shouldResolveDataTypeResultSet() throws Exception {
        final Model model = createModel();
        final StoredProcedure procedure = model.addStoredProcedure(uow, "procedure");
        final ProcedureResultSet resultSet = procedure.setResultSet(uow, DataTypeResultSet.class);
        final KomodoObject kobject = new ObjectImpl(_repo, resultSet.getAbsolutePath(), resultSet.getIndex());
        assertThat(this.wsMgr.resolve(uow, kobject, DataTypeResultSet.class), is(instanceOf(DataTypeResultSet.class)));
    }

    @Test
    public void shouldResolveEntry() throws Exception {
        final Vdb vdb = createVdb();
        final Entry entry = vdb.addEntry(this.uow, "entry", "path");
        final KomodoObject kobject = new ObjectImpl(_repo, entry.getAbsolutePath(), entry.getIndex());
        assertThat(this.wsMgr.resolve(this.uow, kobject, Entry.class), is(instanceOf(Entry.class)));
    }

    @Test
    public void shouldResolveForeignKey() throws Exception {
        final Table refTable = createTable(getDefaultVdbName(), VDB_PATH, "mymodel", "refTable");
        final Table table = createTable();
        final ForeignKey foreignKey = table.addForeignKey(uow, "foreignKey", refTable);
        final KomodoObject kobject = new ObjectImpl(_repo, foreignKey.getAbsolutePath(), foreignKey.getIndex());
        assertThat(this.wsMgr.resolve(uow, kobject, ForeignKey.class), is(instanceOf(ForeignKey.class)));
    }

    @Test
    public void shouldResolveIndex() throws Exception {
        final Table table = createTable();
        final Index index = table.addIndex(this.uow, "index");
        final KomodoObject kobject = new ObjectImpl(_repo, index.getAbsolutePath(), index.getIndex());
        assertThat(this.wsMgr.resolve(this.uow, kobject, Index.class), is(instanceOf(Index.class)));
    }

    @Test
    public void shouldResolveMask() throws Exception {
        final Vdb vdb = createVdb();
        final DataRole dataRole = vdb.addDataRole(this.uow, "dataRole");
        final Permission permission = dataRole.addPermission(this.uow, "permission");
        final Mask mask = permission.addMask(this.uow, "mask");
        final KomodoObject kobject = new ObjectImpl(_repo, mask.getAbsolutePath(), mask.getIndex());
        assertThat(this.wsMgr.resolve(this.uow, kobject, Mask.class), is(instanceOf(Mask.class)));
    }

    @Test
    public void shouldResolveModel() throws Exception {
        final Model model = createModel();
        final KomodoObject kobject = new ObjectImpl(_repo, model.getAbsolutePath(), model.getIndex());
        assertThat(this.wsMgr.resolve(this.uow, kobject, Model.class), is(instanceOf(Model.class)));
    }

    @Test
    public void shouldResolveModelSource() throws Exception {
        final Model model = createModel();
        final ModelSource source = model.addSource(this.uow, "source");
        final KomodoObject kobject = new ObjectImpl(_repo, source.getAbsolutePath(), source.getIndex());
        assertThat(this.wsMgr.resolve(this.uow, kobject, ModelSource.class), is(instanceOf(ModelSource.class)));
    }

    @Test
    public void shouldResolveParameter() throws Exception {
        final Model model = createModel();
        final Procedure procedure = model.addVirtualProcedure(this.uow, "procedure");
        final Parameter param = procedure.addParameter(this.uow, "param");
        final KomodoObject kobject = new ObjectImpl(_repo, param.getAbsolutePath(), param.getIndex());
        assertThat(this.wsMgr.resolve(this.uow, kobject, Parameter.class), is(instanceOf(Parameter.class)));
    }

    @Test
    public void shouldResolvePermission() throws Exception {
        final Vdb vdb = createVdb();
        final DataRole dataRole = vdb.addDataRole(this.uow, "dataRole");
        final Permission permission = dataRole.addPermission(this.uow, "permission");
        final KomodoObject kobject = new ObjectImpl(_repo, permission.getAbsolutePath(), permission.getIndex());
        assertThat(this.wsMgr.resolve(this.uow, kobject, Permission.class), is(instanceOf(Permission.class)));
    }

    @Test
    public void shouldResolvePrimaryKey() throws Exception {
        final Table table = createTable();
        final PrimaryKey pk = table.setPrimaryKey(this.uow, "pk");
        final KomodoObject kobject = new ObjectImpl(_repo, pk.getAbsolutePath(), pk.getIndex());
        assertThat(this.wsMgr.resolve(this.uow, kobject, PrimaryKey.class), is(instanceOf(PrimaryKey.class)));
    }

    @Test
    public void shouldResolvePushdownFunction() throws Exception {
        final Model model = createModel();
        final Function function = model.addPushdownFunction(this.uow, "function");
        final KomodoObject kobject = new ObjectImpl(_repo, function.getAbsolutePath(), function.getIndex());
        assertThat(this.wsMgr.resolve(this.uow, kobject, PushdownFunction.class), is(instanceOf(PushdownFunction.class)));
    }

    @Test
    public void shouldResolveStoredProcedure() throws Exception {
        final Model model = createModel();
        final Procedure procedure = model.addStoredProcedure(this.uow, "procedure");
        final KomodoObject kobject = new ObjectImpl(_repo, procedure.getAbsolutePath(), procedure.getIndex());
        assertThat(this.wsMgr.resolve(this.uow, kobject, StoredProcedure.class), is(instanceOf(StoredProcedure.class)));
    }

    @Test
    public void shouldResolveTabularResultSet() throws Exception {
        final Model model = createModel();
        final StoredProcedure procedure = model.addStoredProcedure(uow, "procedure");
        final ProcedureResultSet resultSet = procedure.setResultSet(uow, TabularResultSet.class);
        final KomodoObject kobject = new ObjectImpl(_repo, resultSet.getAbsolutePath(), resultSet.getIndex());
        assertThat(this.wsMgr.resolve(uow, kobject, TabularResultSet.class), is(instanceOf(TabularResultSet.class)));
    }

    @Test
    public void shouldResolveTable() throws Exception {
        final Table table = createTable();
        final KomodoObject kobject = new ObjectImpl(_repo, table.getAbsolutePath(), table.getIndex());
        assertThat(this.wsMgr.resolve(this.uow, kobject, Table.class), is(instanceOf(Table.class)));
    }

    @Test
    public void shouldResolveTeiid() throws Exception {
        final Teiid teiid = this.wsMgr.createTeiid(this.uow, null, "teiid");
        final KomodoObject kobject = new ObjectImpl(_repo, teiid.getAbsolutePath(), teiid.getIndex());
        assertThat(this.wsMgr.resolve(this.uow, kobject, Teiid.class), is(instanceOf(Teiid.class)));
    }

    @Test
    public void shouldResolveTranslator() throws Exception {
        final Vdb vdb = createVdb();
        final Translator translator = vdb.addTranslator(this.uow, "translator", "oracle");
        final KomodoObject kobject = new ObjectImpl(_repo, translator.getAbsolutePath(), translator.getIndex());
        assertThat(this.wsMgr.resolve(this.uow, kobject, Translator.class), is(instanceOf(Translator.class)));
    }

    @Test
    public void shouldResolveUniqueConstraint() throws Exception {
        final Table table = createTable();
        final UniqueConstraint uniqueConstraint = table.addUniqueConstraint(this.uow, "uniqueConstraint");
        final KomodoObject kobject = new ObjectImpl(_repo, uniqueConstraint.getAbsolutePath(), uniqueConstraint.getIndex());
        assertThat(this.wsMgr.resolve(this.uow, kobject, UniqueConstraint.class), is(instanceOf(UniqueConstraint.class)));
    }

    @Test
    public void shouldResolveUserDefinedFunction() throws Exception {
        final Model model = createModel();
        final Function function = model.addUserDefinedFunction(this.uow, "function");
        final KomodoObject kobject = new ObjectImpl(_repo, function.getAbsolutePath(), function.getIndex());
        assertThat(this.wsMgr.resolve(this.uow, kobject, UserDefinedFunction.class), is(instanceOf(UserDefinedFunction.class)));
    }

    @Test
    public void shouldResolveVdb() throws Exception {
        final Vdb vdb = createVdb();
        final KomodoObject kobject = new ObjectImpl(_repo, vdb.getAbsolutePath(), vdb.getIndex());
        assertThat(this.wsMgr.resolve(this.uow, kobject, Vdb.class), is(instanceOf(Vdb.class)));
    }

    @Test
    public void shouldResolveVdbImport() throws Exception {
        final Vdb vdb = createVdb();
        final VdbImport vdbImport = vdb.addImport(this.uow, "vdbImport");
        final KomodoObject kobject = new ObjectImpl(_repo, vdbImport.getAbsolutePath(), vdbImport.getIndex());
        assertThat(this.wsMgr.resolve(this.uow, kobject, VdbImport.class), is(instanceOf(VdbImport.class)));
    }

    @Test
    public void shouldResolveView() throws Exception {
        final Model model = createModel();
        final View view = model.addView(this.uow, "view");
        final KomodoObject kobject = new ObjectImpl(_repo, view.getAbsolutePath(), view.getIndex());
        assertThat(this.wsMgr.resolve(this.uow, kobject, View.class), is(instanceOf(View.class)));
    }

    @Test
    public void shouldResolveVirtualProcedure() throws Exception {
        final Model model = createModel();
        final Procedure procedure = model.addVirtualProcedure(this.uow, "procedure");
        final KomodoObject kobject = new ObjectImpl(_repo, procedure.getAbsolutePath(), procedure.getIndex());
        assertThat(this.wsMgr.resolve(this.uow, kobject, VirtualProcedure.class), is(instanceOf(VirtualProcedure.class)));
    }

    @Test
    public void shouldAdaptAllRelationalObjects() throws Exception {
        Vdb vdb = createVdb();
        Model model = vdb.addModel(this.uow, "testControlModel");
        Table table = model.addTable(this.uow, "testControlTable");
        KomodoObject o = null;

        // Null object should safely return null
        assertNull(wsMgr.resolve(this.uow, o, Vdb.class));

        // Vdb should always equal vdb
        o = vdb;
        Vdb vdb1 = wsMgr.resolve(this.uow, o, Vdb.class);
        assertNotNull(vdb1);

        // ObjectImpl referencing a vdb should be able to be adapted to a Vdb
        ObjectImpl vdbObj = new ObjectImpl(_repo, vdb.getAbsolutePath(), 0);
        Vdb vdb2 = wsMgr.resolve(this.uow, vdbObj, Vdb.class);
        assertNotNull(vdb2);

        // Model should always equal model
        o = model;
        Model model1 = wsMgr.resolve(this.uow, o, Model.class);
        assertNotNull(model1);

        // ObjectImpl referencing a model should be able to be adapted to a Model
        ObjectImpl modelObj = new ObjectImpl(_repo, model.getAbsolutePath(), 0);
        Model model2 = wsMgr.resolve(this.uow, modelObj, Model.class);
        assertNotNull(model2);

        // Table should always equal table
        o = table;
        Table table1 = wsMgr.resolve(this.uow, o, Table.class);
        assertNotNull(table1);

        // ObjectImpl referencing a table should be able to be adapted to a Table
        ObjectImpl tableObj = new ObjectImpl(_repo, table.getAbsolutePath(), 0);
        Table table2 = wsMgr.resolve(this.uow, tableObj, Table.class);
        assertNotNull(table2);
    }

    @Test
    public void shouldCreateDataTypeResultSet() throws Exception {
        final Model model = createModel();
        final StoredProcedure procedure = model.addStoredProcedure(this.uow, "procedure");
        final KomodoObject result = wsMgr.create(this.uow, procedure, "resultSet", KomodoType.DATA_TYPE_RESULT_SET);
        assertNotNull(result);
        assertThat(result, is(instanceOf(DataTypeResultSet.class)));
    }

    @Test
    public void shouldCreateResultSetColumn() throws Exception {
        final Model model = createModel();
        final PushdownFunction function = model.addPushdownFunction( this.uow, "function");
        final TabularResultSet resultSet = function.setResultSet( this.uow, TabularResultSet.class );
        final KomodoObject result = wsMgr.create(this.uow, resultSet, "resultSetColumn", KomodoType.RESULT_SET_COLUMN);
        assertThat(result, is(instanceOf(ResultSetColumn.class)));
    }

    @Test
    public void shouldCreateAllRelationalObjects() throws Exception {
        KomodoObject workspace = _repo.komodoWorkspace(this.uow);
        Vdb vdb = createVdb();
        Model model = vdb.addModel(this.uow, "testControlModel");
        Table table = model.addTable(this.uow, "testControlTable");
        StoredProcedure procedure = model.addStoredProcedure(this.uow, "testControlProcedure");
        DataRole dataRole = vdb.addDataRole(this.uow, "testControlDataRole");
        Permission permission = dataRole.addPermission(this.uow, "testControlPermission");

        for (KomodoType type : KomodoType.values()) {
            String id = "test" + type.getType();
            switch (type) {
                case FOREIGN_KEY: {
                    Table refTable = model.addTable(this.uow, "testRefTable");
                    KomodoObject result = wsMgr.create(this.uow, table, id, type, new RelationalProperty(TeiidDdlLexicon.Constraint.FOREIGN_KEY_CONSTRAINT, refTable));
                    assertNotNull(result);
                    break;
                }
                case STATEMENT_OPTION: {
                    String optionValue = "testOptionValue";
                    KomodoObject result = wsMgr.create(this.uow, table, id, type, new RelationalProperty(StandardDdlLexicon.VALUE, optionValue));
                    assertNotNull(result);
                    break;
                }
                case VDB_ENTRY: {
                    String entryPath = "testEntryPath";
                    KomodoObject result = wsMgr.create(this.uow, vdb, id, type, new RelationalProperty(VdbLexicon.Entry.PATH, entryPath));
                    assertNotNull(result);
                    break;
                }
                case VDB_TRANSLATOR: {
                    String transType = "testTranslatorType";
                    KomodoObject result = wsMgr.create(this.uow, vdb, id, type, new RelationalProperty(VdbLexicon.Translator.TYPE, transType));
                    assertNotNull(result);
                    break;
                }
                case VDB: {
                    String filePath = "/test2";
                    KomodoObject result = wsMgr.create(this.uow, workspace, "test" + id, type, new RelationalProperty(VdbLexicon.Vdb.ORIGINAL_FILE, filePath));
                    assertNotNull(result);
                    break;
                }
                case ACCESS_PATTERN: {
                    KomodoObject result = wsMgr.create(this.uow, table, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case COLUMN: {
                    KomodoObject result = wsMgr.create(this.uow, table, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case PUSHDOWN_FUNCTION: {
                    KomodoObject result = wsMgr.create(this.uow, model, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case USER_DEFINED_FUNCTION: {
                    KomodoObject result = wsMgr.create(this.uow, model, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case INDEX: {
                    KomodoObject result = wsMgr.create(this.uow, table, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case MODEL: {
                    KomodoObject result = wsMgr.create(this.uow, vdb, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case PARAMETER: {
                    KomodoObject result = wsMgr.create(this.uow, procedure, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case PRIMARY_KEY: {
                    KomodoObject result = wsMgr.create(this.uow, table, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case STORED_PROCEDURE: {
                    KomodoObject result = wsMgr.create(this.uow, model, "test" + id, type);
                    assertNotNull(result);
                    assertThat(result, is(instanceOf(StoredProcedure.class)));
                    break;
                }
                case VIRTUAL_PROCEDURE: {
                    KomodoObject result = wsMgr.create(this.uow, model, "test" + id, type);
                    assertNotNull(result);
                    assertThat(result, is(instanceOf(VirtualProcedure.class)));
                    break;
                }
                case DATA_TYPE_RESULT_SET: {
                    // since this and tabular result set expect the same name must run in different test
                    break;
                }
                case TABULAR_RESULT_SET: {
                    KomodoObject result = wsMgr.create(this.uow, procedure, "test" + id, type);
                    assertNotNull(result);
                    assertThat(result, is(instanceOf(TabularResultSet.class)));
                    break;
                }
                case RESULT_SET_COLUMN: {
                    // see separate test
                    break;
                }
                case SCHEMA: {
                    KomodoObject result = wsMgr.create(this.uow, workspace, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case TABLE: {
                    KomodoObject result = wsMgr.create(this.uow, model, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case TEIID: {
                    KomodoObject result = wsMgr.create(this.uow, workspace, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case UNIQUE_CONSTRAINT: {
                    KomodoObject result = wsMgr.create(this.uow, table, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case VDB_CONDITION: {
                    KomodoObject result = wsMgr.create(this.uow, permission, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case VDB_DATA_ROLE: {
                    KomodoObject result = wsMgr.create(this.uow, vdb, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case VDB_IMPORT: {
                    KomodoObject result = wsMgr.create(this.uow, vdb, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case VDB_MASK: {
                    KomodoObject result = wsMgr.create(this.uow, permission, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case VDB_MODEL_SOURCE: {
                    KomodoObject result = wsMgr.create(this.uow, model, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case VDB_PERMISSION: {
                    KomodoObject result = wsMgr.create(this.uow, dataRole, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case VIEW: {
                    KomodoObject result = wsMgr.create(this.uow, model, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case UNKNOWN:
                default: {
                    KomodoObject result = wsMgr.create(this.uow, workspace, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
            }
        }
    }

    @Test
    public void shouldFilterChildren() throws Exception {
    	final WorkspaceManager workspaceMgr = WorkspaceManager.getInstance(_repo );
    	final Vdb vdb = createVdb();
    	final KomodoObject kobject = _repo.add( this.uow, workspaceMgr.getAbsolutePath(), "kobject", null );
    	assertThat( workspaceMgr.getChildren( this.uow ).length, is( 1 ) );
    	assertThat( workspaceMgr.getChildren( this.uow )[0], is( instanceOf( Vdb.class ) ) );
    	assertThat( workspaceMgr.getRawChildren( this.uow ).length, is( 2 ) );
    	assertThat( Arrays.asList( workspaceMgr.getRawChildren( this.uow ) ), hasItems( vdb, kobject ) );
    }

}
