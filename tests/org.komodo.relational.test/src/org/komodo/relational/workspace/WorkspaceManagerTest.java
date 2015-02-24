/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.workspace;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalProperty;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.model.AccessPattern;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Function;
import org.komodo.relational.model.Index;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.ProcedureResultSet;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.relational.model.View;
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
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class WorkspaceManagerTest extends RelationalModelTest {

    private WorkspaceManager wsMgr;

    @Before
    public void obtainWorkspaceManager() {
        wsMgr = WorkspaceManager.getInstance(_repo);
    }

    @After
    public void cleanup() {
        WorkspaceManager.uncacheInstance(_repo);
        wsMgr = null;
    }

    private Model createModel(final UnitOfWork uow, final KomodoObject parent, final String modelName) throws Exception {
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = _repo.createTransaction(this.name.getMethodName(), false, null);
        }

        final Model model = this.wsMgr.createModel(transaction, parent, modelName);

        assertThat(model.getPrimaryType(transaction).getName(), is(VdbLexicon.Vdb.DECLARATIVE_MODEL));
        assertThat(model.getName(transaction), is(modelName));

        if (uow == null) {
            transaction.commit();
        }

        return model;
    }

    private Table createTable( final UnitOfWork uow,
                               final String tableName ) throws Exception {
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = _repo.createTransaction(this.name.getMethodName(), false, null);
        }

        final Vdb vdb = createVdb(transaction, null, "vdb");
        final Model model = createModel(transaction, vdb, "model");
        final Table table = model.addTable(transaction, tableName);

        if (uow == null) {
            transaction.commit();
        }

        return table;
    }

    private Vdb createVdb( final UnitOfWork uow,
                           final KomodoObject parent,
                           final String vdbName ) throws Exception {
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = _repo.createTransaction(this.name.getMethodName(), false, null);
        }

        final String externalFilePath = "extPath";
        final Vdb vdb = this.wsMgr.createVdb(transaction, parent, vdbName, externalFilePath);

        assertThat(vdb.getPrimaryType(transaction).getName(), is(VdbLexicon.Vdb.VIRTUAL_DATABASE));
        assertThat(vdb.getName(transaction), is(vdbName));
        assertThat(vdb.getOriginalFilePath(transaction), is(externalFilePath));

        if (uow == null) {
            transaction.commit();
        }

        return vdb;
    }

    @Test
    public void shouldCreateVdb() throws Exception {
        final KomodoObject parent = _repo.add(null, _repo.komodoWorkspace(null).getAbsolutePath(), "parent", null);
        final Vdb vdb = createVdb(null, parent, this.name.getMethodName());
        assertThat(vdb, is(notNullValue()));
        assertThat(_repo.getFromWorkspace(null, vdb.getAbsolutePath()), is((KomodoObject)vdb));
    }

    @Test
    public void shouldCreateVdbWithNullParent() throws Exception {
        final Vdb vdb = createVdb(null, null, this.name.getMethodName());
        assertThat(vdb, is(notNullValue()));
        assertThat(vdb.getParent(null), is(_repo.komodoWorkspace(null)));
    }

    @Test
    public void shouldDeleteModel() throws Exception {
        final Vdb parent = createVdb(null, null, this.name.getMethodName() + "Vdb");
        final Model model = createModel(null, parent, this.name.getMethodName() + "Model");
        this.wsMgr.delete(null, model);
        assertThat(this.wsMgr.findModels(null).length, is(0));
    }

    @Test
    public void shouldDeleteVdb() throws Exception {
        final Vdb vdb = createVdb(null, null, "vdb");
        this.wsMgr.delete(null, vdb);
        assertThat(this.wsMgr.findVdbs(null).length, is(0));
    }

    @Test
    public void shouldFindModels() throws Exception {
        final Vdb parent = createVdb(null, null, "vdb");
        final String prefix = this.name.getMethodName();
        int suffix = 0;

        // create at root
        for (int i = 0; i < 5; ++i) {
            createModel(null, parent, (prefix + ++suffix));
        }

        // create under a folder
        final KomodoObject folder = _repo.add(null, parent.getAbsolutePath(), "folder", null);

        for (int i = 0; i < 7; ++i) {
            createModel(null, folder, (prefix + ++suffix));
        }

        assertThat(this.wsMgr.findModels(null).length, is(suffix));
    }

    @Test
    public void shouldFindVdbs() throws Exception {
        final String prefix = this.name.getMethodName();
        int suffix = 0;

        // create at root
        for (int i = 0; i < 5; ++i) {
            createVdb(null, null, (prefix + ++suffix));
        }

        // create under a folder
        final KomodoObject parent = _repo.add(null, null, "blah", null);

        for (int i = 0; i < 7; ++i) {
            createVdb(null, parent, (prefix + ++suffix));
        }

        assertThat(this.wsMgr.findVdbs(null).length, is(suffix));
    }

    @Test( expected = KException.class )
    public void shouldNotAllowEmptyExternalFilePath() throws Exception {
        this.wsMgr.createVdb(null, null, "vdbName", StringConstants.EMPTY_STRING);
    }

    @Test( expected = KException.class )
    public void shouldNotAllowEmptyVdbName() throws Exception {
        this.wsMgr.createVdb(null, null, StringConstants.EMPTY_STRING, "externalFilePath");
    }

    @Test( expected = KException.class )
    public void shouldNotAllowNullExternalFilePath() throws Exception {
        this.wsMgr.createVdb(null, null, "vdbName", null);
    }

    @Test( expected = KException.class )
    public void shouldNotAllowNullVdbName() throws Exception {
        this.wsMgr.createVdb(null, null, null, "externalFilePath");
    }

    @Test
    public void shouldNotFindModelsWhenWorkspaceIsEmpty() throws Exception {
        assertThat(this.wsMgr.findModels(null).length, is(0));
    }

    @Test
    public void shouldNotFindSchemasWhenWorkspaceIsEmpty() throws Exception {
        assertThat(this.wsMgr.findSchemas(null).length, is(0));
    }

    @Test
    public void shouldNotFindTeiidsWhenWorkspaceIsEmpty() throws Exception {
        assertThat(this.wsMgr.findTeiids(null).isEmpty(), is(true));
    }

    @Test
    public void shouldNotFindVdbsWhenWorkspaceIsEmpty() throws Exception {
        assertThat(this.wsMgr.findVdbs(null).length, is(0));
    }

    @Test
    public void shouldNotResolveAsVdb() throws Exception {
        final Model model = createModel(null, null, "model");
        final KomodoObject kobject = new ObjectImpl(_repo, model.getAbsolutePath(), model.getIndex());
        assertNull(this.wsMgr.resolve(null, kobject, Vdb.class));
    }

    @Test
    public void shouldResolveAccessPattern() throws Exception {
        final Table table = createTable(null, "table");
        final AccessPattern accessPattern = table.addAccessPattern(null, "accessPattern");
        final KomodoObject kobject = new ObjectImpl(_repo, accessPattern.getAbsolutePath(), accessPattern.getIndex());
        assertThat(this.wsMgr.resolve(null, kobject, AccessPattern.class), is(instanceOf(AccessPattern.class)));
    }

    @Test
    public void shouldResolveCondition() throws Exception {
        final Vdb vdb = createVdb(null, null, "vdb");
        final DataRole dataRole = vdb.addDataRole(null, "dataRole");
        final Permission permission = dataRole.addPermission(null, "permission");
        final Condition condition = permission.addCondition(null, "condition");
        final KomodoObject kobject = new ObjectImpl(_repo, condition.getAbsolutePath(), condition.getIndex());
        assertThat(this.wsMgr.resolve(null, kobject, Condition.class), is(instanceOf(Condition.class)));
    }

    @Test
    public void shouldResolveDataRole() throws Exception {
        final Vdb vdb = createVdb(null, null, "vdb");
        final DataRole dataRole = vdb.addDataRole(null, "dataRole");
        final KomodoObject kobject = new ObjectImpl(_repo, dataRole.getAbsolutePath(), dataRole.getIndex());
        assertThat(this.wsMgr.resolve(null, kobject, DataRole.class), is(instanceOf(DataRole.class)));
    }

    @Test
    public void shouldResolveEntry() throws Exception {
        final Vdb vdb = createVdb(null, null, "vdb");
        final Entry entry = vdb.addEntry(null, "entry", "path");
        final KomodoObject kobject = new ObjectImpl(_repo, entry.getAbsolutePath(), entry.getIndex());
        assertThat(this.wsMgr.resolve(null, kobject, Entry.class), is(instanceOf(Entry.class)));
    }

    @Test
    public void shouldResolveForeignKey() throws Exception {
        final Repository.UnitOfWork uow = _repo.createTransaction(this.name.getMethodName(), false, null);
        final Table refTable = createTable(uow, "refTable");
        final Table table = createTable(uow, "table");
        final ForeignKey foreignKey = table.addForeignKey(uow, "foreignKey", refTable);
        final KomodoObject kobject = new ObjectImpl(_repo, foreignKey.getAbsolutePath(), foreignKey.getIndex());
        assertThat(this.wsMgr.resolve(uow, kobject, ForeignKey.class), is(instanceOf(ForeignKey.class)));
        uow.commit();
    }

    @Test
    public void shouldResolveFunction() throws Exception {
        final Model model = createModel(null, null, "model");
        final Function function = model.addFunction(null, "function");
        final KomodoObject kobject = new ObjectImpl(_repo, function.getAbsolutePath(), function.getIndex());
        assertThat(this.wsMgr.resolve(null, kobject, Function.class), is(instanceOf(Function.class)));
    }

    @Test
    public void shouldResolveIndex() throws Exception {
        final Table table = createTable(null, "table");
        final Index index = table.addIndex(null, "index");
        final KomodoObject kobject = new ObjectImpl(_repo, index.getAbsolutePath(), index.getIndex());
        assertThat(this.wsMgr.resolve(null, kobject, Index.class), is(instanceOf(Index.class)));
    }

    @Test
    public void shouldResolveMask() throws Exception {
        final Vdb vdb = createVdb(null, null, "vdb");
        final DataRole dataRole = vdb.addDataRole(null, "dataRole");
        final Permission permission = dataRole.addPermission(null, "permission");
        final Mask mask = permission.addMask(null, "mask");
        final KomodoObject kobject = new ObjectImpl(_repo, mask.getAbsolutePath(), mask.getIndex());
        assertThat(this.wsMgr.resolve(null, kobject, Mask.class), is(instanceOf(Mask.class)));
    }

    @Test
    public void shouldResolveModel() throws Exception {
        final Model model = createModel(null, null, "model");
        final KomodoObject kobject = new ObjectImpl(_repo, model.getAbsolutePath(), model.getIndex());
        assertThat(this.wsMgr.resolve(null, kobject, Model.class), is(instanceOf(Model.class)));
    }

    @Test
    public void shouldResolveModelSource() throws Exception {
        final Model model = createModel(null, null, "model");
        final ModelSource source = model.addSource(null, "source");
        final KomodoObject kobject = new ObjectImpl(_repo, source.getAbsolutePath(), source.getIndex());
        assertThat(this.wsMgr.resolve(null, kobject, ModelSource.class), is(instanceOf(ModelSource.class)));
    }

    @Test
    public void shouldResolveParameter() throws Exception {
        final Model model = createModel(null, null, "model");
        final Procedure procedure = model.addProcedure(null, "procedure");
        final Parameter param = procedure.addParameter(null, "param");
        final KomodoObject kobject = new ObjectImpl(_repo, param.getAbsolutePath(), param.getIndex());
        assertThat(this.wsMgr.resolve(null, kobject, Parameter.class), is(instanceOf(Parameter.class)));
    }

    @Test
    public void shouldResolvePermission() throws Exception {
        final Vdb vdb = createVdb(null, null, "vdb");
        final DataRole dataRole = vdb.addDataRole(null, "dataRole");
        final Permission permission = dataRole.addPermission(null, "permission");
        final KomodoObject kobject = new ObjectImpl(_repo, permission.getAbsolutePath(), permission.getIndex());
        assertThat(this.wsMgr.resolve(null, kobject, Permission.class), is(instanceOf(Permission.class)));
    }

    @Test
    public void shouldResolvePrimaryKey() throws Exception {
        final Table table = createTable(null, "table");
        final PrimaryKey pk = table.setPrimaryKey(null, "pk");
        final KomodoObject kobject = new ObjectImpl(_repo, pk.getAbsolutePath(), pk.getIndex());
        assertThat(this.wsMgr.resolve(null, kobject, PrimaryKey.class), is(instanceOf(PrimaryKey.class)));
    }

    @Test
    public void shouldResolveProcedure() throws Exception {
        final Model model = createModel(null, null, "model");
        final Procedure procedure = model.addProcedure(null, "procedure");
        final KomodoObject kobject = new ObjectImpl(_repo, procedure.getAbsolutePath(), procedure.getIndex());
        assertThat(this.wsMgr.resolve(null, kobject, Procedure.class), is(instanceOf(Procedure.class)));
    }

    @Test
    public void shouldResolveProcedureResultSet() throws Exception {
        final Repository.UnitOfWork uow = _repo.createTransaction(this.name.getMethodName(), false, null);
        final Model model = createModel(uow, null, "model");
        final Procedure procedure = model.addProcedure(uow, "procedure");
        final ProcedureResultSet resultSet = procedure.getResultSet(uow, true);
        final KomodoObject kobject = new ObjectImpl(_repo, resultSet.getAbsolutePath(), resultSet.getIndex());
        assertThat(this.wsMgr.resolve(uow, kobject, ProcedureResultSet.class), is(instanceOf(ProcedureResultSet.class)));
        uow.commit();
    }

    @Test
    public void shouldResolveTable() throws Exception {
        final Table table = createTable(null, "table");
        final KomodoObject kobject = new ObjectImpl(_repo, table.getAbsolutePath(), table.getIndex());
        assertThat(this.wsMgr.resolve(null, kobject, Table.class), is(instanceOf(Table.class)));
    }

    @Test
    public void shouldResolveTeiid() throws Exception {
        final Teiid teiid = this.wsMgr.createTeiid(null, null, "teiid");
        final KomodoObject kobject = new ObjectImpl(_repo, teiid.getAbsolutePath(), teiid.getIndex());
        assertThat(this.wsMgr.resolve(null, kobject, Teiid.class), is(instanceOf(Teiid.class)));
    }

    @Test
    public void shouldResolveTranslator() throws Exception {
        final Vdb vdb = createVdb(null, null, "vdb");
        final Translator translator = vdb.addTranslator(null, "translator", "oracle");
        final KomodoObject kobject = new ObjectImpl(_repo, translator.getAbsolutePath(), translator.getIndex());
        assertThat(this.wsMgr.resolve(null, kobject, Translator.class), is(instanceOf(Translator.class)));
    }

    @Test
    public void shouldResolveUniqueConstraint() throws Exception {
        final Table table = createTable(null, "table");
        final UniqueConstraint uniqueConstraint = table.addUniqueConstraint(null, "uniqueConstraint");
        final KomodoObject kobject = new ObjectImpl(_repo, uniqueConstraint.getAbsolutePath(), uniqueConstraint.getIndex());
        assertThat(this.wsMgr.resolve(null, kobject, UniqueConstraint.class), is(instanceOf(UniqueConstraint.class)));
    }

    @Test
    public void shouldResolveVdb() throws Exception {
        final Vdb vdb = createVdb(null, null, "vdb");
        final KomodoObject kobject = new ObjectImpl(_repo, vdb.getAbsolutePath(), vdb.getIndex());
        assertThat(this.wsMgr.resolve(null, kobject, Vdb.class), is(instanceOf(Vdb.class)));
    }

    @Test
    public void shouldResolveVdbImport() throws Exception {
        final Vdb vdb = createVdb(null, null, "vdb");
        final VdbImport vdbImport = vdb.addImport(null, "vdbImport");
        final KomodoObject kobject = new ObjectImpl(_repo, vdbImport.getAbsolutePath(), vdbImport.getIndex());
        assertThat(this.wsMgr.resolve(null, kobject, VdbImport.class), is(instanceOf(VdbImport.class)));
    }

    @Test
    public void shouldResolveView() throws Exception {
        final Model model = createModel(null, null, "model");
        final View view = model.addView(null, "view");
        final KomodoObject kobject = new ObjectImpl(_repo, view.getAbsolutePath(), view.getIndex());
        assertThat(this.wsMgr.resolve(null, kobject, View.class), is(instanceOf(View.class)));
    }

    @Test
    public void shouldAdaptAllRelationalObjects() throws Exception {
        KomodoObject workspace = _repo.komodoWorkspace(null);
        Vdb vdb = RelationalModelFactory.createVdb(null, _repo, workspace.getAbsolutePath(), "testControlVdb", "/test1");
        Model model = RelationalModelFactory.createModel(null, _repo, vdb, "testControlModel");
        Table table = RelationalModelFactory.createTable(null, _repo, model, "testControlTable");
        KomodoObject o = null;

        // Null object should safely return null
        assertNull(wsMgr.resolve(null, o, Vdb.class));

        // Vdb should always equal vdb
        o = vdb;
        Vdb vdb1 = wsMgr.resolve(null, o, Vdb.class);
        assertNotNull(vdb1);

        // ObjectImpl referencing a vdb should be able to be adapted to a Vdb
        ObjectImpl vdbObj = new ObjectImpl(_repo, vdb.getAbsolutePath(), 0);
        Vdb vdb2 = wsMgr.resolve(null, vdbObj, Vdb.class);
        assertNotNull(vdb2);

        // Model should always equal model
        o = model;
        Model model1 = wsMgr.resolve(null, o, Model.class);
        assertNotNull(model1);

        // ObjectImpl referencing a model should be able to be adapted to a Model
        ObjectImpl modelObj = new ObjectImpl(_repo, model.getAbsolutePath(), 0);
        Model model2 = wsMgr.resolve(null, modelObj, Model.class);
        assertNotNull(model2);

        // Table should always equal table
        o = table;
        Table table1 = wsMgr.resolve(null, o, Table.class);
        assertNotNull(table1);

        // ObjectImpl referencing a table should be able to be adapted to a Table
        ObjectImpl tableObj = new ObjectImpl(_repo, table.getAbsolutePath(), 0);
        Table table2 = wsMgr.resolve(null, tableObj, Table.class);
        assertNotNull(table2);
    }

    @Test
    public void shouldCreateAllRelationalObjects() throws Exception {
        KomodoObject workspace = _repo.komodoWorkspace(null);
        Vdb vdb = RelationalModelFactory.createVdb(null, _repo, workspace.getAbsolutePath(), "testControlVdb", "/test1");
        Model model = RelationalModelFactory.createModel(null, _repo, vdb, "testControlModel");
        Table table = RelationalModelFactory.createTable(null, _repo, model, "testControlTable");
        Procedure procedure = RelationalModelFactory.createProcedure(null, _repo, model, "testControlProcedure");
        DataRole dataRole = RelationalModelFactory.createDataRole(null, _repo, vdb, "testControlDataRole");
        Permission permission = RelationalModelFactory.createPermission(null, _repo, dataRole, "testControlPermission");

        for (KomodoType type : KomodoType.values()) {
            String id = "test" + type.getType();
            switch (type) {
                case FOREIGN_KEY: {
                    Table refTable = RelationalModelFactory.createTable(null, _repo, model, "testRefTable");
                    KomodoObject result = wsMgr.create(null, table, id, type, new RelationalProperty(TeiidDdlLexicon.Constraint.FOREIGN_KEY_CONSTRAINT, refTable));
                    assertNotNull(result);
                    break;
                }
                case STATEMENT_OPTION: {
                    String optionValue = "testOptionValue";
                    KomodoObject result = wsMgr.create(null, table, id, type, new RelationalProperty(StandardDdlLexicon.VALUE, optionValue));
                    assertNotNull(result);
                    break;
                }
                case VDB_ENTRY: {
                    String entryPath = "testEntryPath";
                    KomodoObject result = wsMgr.create(null, vdb, id, type, new RelationalProperty(VdbLexicon.Entry.PATH, entryPath));
                    assertNotNull(result);
                    break;
                }
                case VDB_TRANSLATOR: {
                    String transType = "testTranslatorType";
                    KomodoObject result = wsMgr.create(null, vdb, id, type, new RelationalProperty(VdbLexicon.Translator.TYPE, transType));
                    assertNotNull(result);
                    break;
                }
                case VDB: {
                    String filePath = "/test2";
                    KomodoObject result = wsMgr.create(null, workspace, "test" + id, type, new RelationalProperty(VdbLexicon.Vdb.ORIGINAL_FILE, filePath));
                    assertNotNull(result);
                    break;
                }
                case ACCESS_PATTERN: {
                    KomodoObject result = wsMgr.create(null, table, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case COLUMN: {
                    KomodoObject result = wsMgr.create(null, table, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case FUNCTION: {
                    KomodoObject result = wsMgr.create(null, model, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case INDEX: {
                    KomodoObject result = wsMgr.create(null, table, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case MODEL: {
                    KomodoObject result = wsMgr.create(null, vdb, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case PARAMETER: {
                    KomodoObject result = wsMgr.create(null, procedure, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case PRIMARY_KEY: {
                    KomodoObject result = wsMgr.create(null, table, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case PROCEDURE: {
                    KomodoObject result = wsMgr.create(null, model, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case PROCEDURE_RESULT_SET: {
                    KomodoObject result = wsMgr.create(null, procedure, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case SCHEMA: {
                    KomodoObject result = wsMgr.create(null, workspace, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case TABLE: {
                    KomodoObject result = wsMgr.create(null, model, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case TEIID: {
                    KomodoObject result = wsMgr.create(null, workspace, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case UNIQUE_CONSTRAINT: {
                    KomodoObject result = wsMgr.create(null, table, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case VDB_CONDITION: {
                    KomodoObject result = wsMgr.create(null, permission, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case VDB_DATA_ROLE: {
                    KomodoObject result = wsMgr.create(null, vdb, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case VDB_IMPORT: {
                    KomodoObject result = wsMgr.create(null, vdb, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case VDB_MASK: {
                    KomodoObject result = wsMgr.create(null, permission, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case VDB_MODEL_SOURCE: {
                    KomodoObject result = wsMgr.create(null, model, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case VDB_PERMISSION: {
                    KomodoObject result = wsMgr.create(null, dataRole, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case VIEW: {
                    KomodoObject result = wsMgr.create(null, model, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
                case UNKNOWN:
                default: {
                    KomodoObject result = wsMgr.create(null, workspace, "test" + id, type);
                    assertNotNull(result);
                    break;
                }
            }
        }
    }
}
