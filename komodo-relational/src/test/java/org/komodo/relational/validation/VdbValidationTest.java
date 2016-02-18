/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.validation;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import org.junit.Test;
import org.komodo.relational.model.AccessPattern;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Index;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.Parameter.Direction;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.relational.model.ResultSetColumn;
import org.komodo.relational.model.SchemaElement;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.relational.model.UserDefinedFunction;
import org.komodo.relational.model.View;
import org.komodo.relational.model.VirtualProcedure;
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Entry;
import org.komodo.relational.vdb.Mask;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.spi.outcome.Outcome.Level;
import org.komodo.spi.repository.validation.Result;
import org.komodo.spi.repository.validation.Rule;

@SuppressWarnings( { "javadoc", "nls" } )
public final class VdbValidationTest extends RelationalValidationTest {

    @Test
    public void shouldGetAllRules() throws Exception {
        final Rule[] rules = _repo.getValidationManager().getAllRules(getTransaction());
        assertThat( rules.length, is( 42 ) );
    }

    // ==============================================================================================
    // Check valid rule counts for each type
    // ==============================================================================================

    @Test
    public void shouldGetValidRulesForVdb() throws Exception {
        Vdb vdb = createVdb("myVDB");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),vdb);
        assertThat( rules.length, is( 4 ) );
    }

    @Test
    public void shouldGetValidRulesForDataRole() throws Exception {
        Vdb vdb = createVdb("myVDB");
        DataRole dataRole = addDataRole(vdb,"1myDataRole");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),dataRole);
        assertThat( rules.length, is( 1 ) );
    }

    @Test
    public void shouldGetValidRulesForPermission() throws Exception {
        Vdb vdb = createVdb("myVDB");
        DataRole dataRole = addDataRole(vdb,"myDataRole");
        Permission permission = addPermission(dataRole,"myPermission");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),permission);
        assertThat( rules.length, is( 1 ) );
    }

    @Test
    public void shouldGetValidRulesForMask() throws Exception {
        Vdb vdb = createVdb("myVDB");
        DataRole dataRole = addDataRole(vdb,"myDataRole");
        Permission permission = addPermission(dataRole,"myPermission");
        Mask mask = addMask(permission,"myMask");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),mask);
        assertThat( rules.length, is( 1 ) );
    }

    @Test
    public void shouldGetValidRulesForCondition() throws Exception {
        Vdb vdb = createVdb("myVDB");
        DataRole dataRole = addDataRole(vdb,"myDataRole");
        Permission permission = addPermission(dataRole,"myPermission");
        Condition condition = addCondition(permission,"myCondition");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),condition);
        assertThat( rules.length, is( 1 ) );
    }

    @Test
    public void shouldGetValidRulesForTranslator() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Translator translator = addTranslator(vdb,"myTranslator","transType");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),translator);
        assertThat( rules.length, is( 1 ) );
    }

    @Test
    public void shouldGetValidRulesForEntry() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Entry entry = addEntry(vdb,"myEntry");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),entry);
        assertThat( rules.length, is( 1 ) );
    }

    @Test
    public void shouldGetValidRulesForVdbImport() throws Exception {
        Vdb vdb = createVdb("myVDB");
        VdbImport vdbImport = addVdbImport(vdb,"myImport");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),vdbImport);
        assertThat( rules.length, is( 1 ) );
    }

    @Test
    public void shouldGetValidRulesForModel() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),model);
        assertThat( rules.length, is( 1 ) );
    }

    @Test
    public void shouldGetValidRulesForTable() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),table);
        assertThat( rules.length, is( 4 ) );
    }

    @Test
    public void shouldGetValidRulesForView() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        View view = addView(model,"myView");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),view);
        assertThat( rules.length, is( 3 ) );
    }

    @Test
    public void shouldGetValidRulesForModelSource() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        ModelSource modelSource = addSource(model,"mySource");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),modelSource);
        assertThat( rules.length, is( 1 ) );
    }

    @Test
    public void shouldGetValidRulesForPushdownFunction() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        PushdownFunction pf = addPushdownFunction(model,"myPF");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),pf);
        assertThat( rules.length, is( 1 ) );
    }

    @Test
    public void shouldGetValidRulesForUserDefinedFunction() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        UserDefinedFunction udf = addUserDefinedFunction(model,"myUDF");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),udf);
        assertThat( rules.length, is( 5 ) );
    }

    @Test
    public void shouldGetValidRulesForVirtualProc() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        VirtualProcedure vp = addVirtualProcedure(model,"myVP");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),vp);
        assertThat( rules.length, is( 1 ) );
    }

    @Test
    public void shouldGetValidRulesForStoredProc() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        StoredProcedure sp = addStoredProcedure(model,"mySP");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),sp);
        assertThat( rules.length, is( 1 ) );
    }

    @Test
    public void shouldGetValidRulesForAccessPattern() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        AccessPattern ap = addAccessPattern(table,"myAP");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),ap);
        assertThat( rules.length, is( 1 ) );
    }

    @Test
    public void shouldGetValidRulesForIndex() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Index index = addIndex(table,"myIndex");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),index);
        assertThat( rules.length, is( 1 ) );
    }

    @Test
    public void shouldGetValidRulesForPrimaryKey() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        PrimaryKey pk = addPrimaryKey(table,"myPK");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),pk);
        assertThat( rules.length, is( 1 ) );
    }

    @Test
    public void shouldGetValidRulesForForeignKey() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Table refTable = addTable(model,"refTable");
        ForeignKey fk = addForeignKey(table,"myFK",refTable);

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),fk);
        assertThat( rules.length, is( 1 ) );
    }

    @Test
    public void shouldGetValidRulesForUniqueConstraint() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        UniqueConstraint uc = addUniqueConstraint(table,"myUC");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),uc);
        assertThat( rules.length, is( 1 ) );
    }

    @Test
    public void shouldGetValidRulesForColumn() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"myColumn");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),col);
        assertThat( rules.length, is( 2 ) );
    }

    @Test
    public void shouldGetValidRulesForStringColumn() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"myColumn");
        col.setDatatypeName(getTransaction(), "string");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),col);
        assertThat( rules.length, is( 3 ) );
    }

    @Test
    public void shouldGetValidRulesForNumericColumn() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"myColumn");
        col.setDatatypeName(getTransaction(), "decimal");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),col);
        assertThat( rules.length, is( 3 ) );
    }

    @Test
    public void shouldGetValidRulesForParameter() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        StoredProcedure sp = addStoredProcedure(model,"mySP");
        Parameter param = addParameter(sp,"myParam");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),param);
        assertThat( rules.length, is( 1 ) );
    }

    @Test
    public void shouldGetValidRulesForDataTypeResultSet() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        StoredProcedure sp = addStoredProcedure(model,"mySP");
        DataTypeResultSet rs = addDataTypeResultSet(sp);

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),rs);
        assertThat( rules.length, is( 1 ) );
    }

    @Test
    public void shouldGetValidRulesForTabularResultSet() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        StoredProcedure sp = addStoredProcedure(model,"mySP");
        TabularResultSet rs = addTabularResultSet(sp);

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),rs);
        assertThat( rules.length, is( 1 ) );
    }

    @Test
    public void shouldGetValidRulesForResultSetColumn() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        StoredProcedure sp = addStoredProcedure(model,"mySP");
        TabularResultSet rs = addTabularResultSet(sp);
        ResultSetColumn rsCol = addResultSetColumn(rs,"myRsCol");

        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),rsCol);
        assertThat( rules.length, is( 1 ) );
    }

    @Test
    public void shouldTestGetVdbNodeNameRuleById() throws Exception {
        final Rule rule = _repo.getValidationManager().getRule("default.vdb.nodeName", getTransaction());
        assertThat( rule.getName(getTransaction()), is( "default.vdb.nodeName" ) );
    }

    @Test
    public void shouldTestGetVdbConnectionTypePropertyRuleById() throws Exception {
        final Rule rule = _repo.getValidationManager().getRule("default.vdb.connectionType.propertyValue", getTransaction());
        assertThat( rule.getName(getTransaction()), is( "default.vdb.connectionType.propertyValue" ) );
    }

    @Test
    public void shouldTestGetRuleByIdFailure() throws Exception {
        final Rule rule = _repo.getValidationManager().getRule("default.vdb.connectionType.badOne", getTransaction());
        assertThat( (rule==null), is( true ) );
    }

    // ==============================================================================================
    // VDB Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestVdbNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), vdb, "default.vdb.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestVdbNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("1myVDB");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), vdb, "default.vdb.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The VDB name must match the specified pattern."));
    }

    @Test
    public void shouldTestVdbNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        vdb.setVdbName(getTransaction(), "vdbName");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), vdb, "default.vdb.name.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestVdbNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        vdb.setVdbName(getTransaction(), "1vdbName");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), vdb, "default.vdb.name.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The VDB 'vdb:name' property is required and must match the specified pattern."));
    }

    @Test
    public void shouldTestVdbVersionValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        vdb.setVersion(getTransaction(), 3);

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), vdb, "default.vdb.version.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestVdbConnectionTypeValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        vdb.setConnectionType(getTransaction(), "vdbName");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), vdb, "default.vdb.connectionType.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestVdbConnectionTypeValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), vdb, "default.vdb.connectionType.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The VDB 'vdb:connectionType' property is required and must match the specified pattern."));
    }

    // ==============================================================================================
    // DataRole Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestDataRoleNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        DataRole dataRole = addDataRole(vdb,"myDataRole");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), dataRole, "default.dataRole.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestDataRoleNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        DataRole dataRole = addDataRole(vdb,"1myDataRole");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), dataRole, "default.dataRole.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The DataRole name must match the specified pattern."));
    }

    // ==============================================================================================
    // Permission Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestPermissionNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        DataRole dataRole = addDataRole(vdb,"myDataRole");
        Permission permission = addPermission(dataRole,"myPermission");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), permission, "default.permission.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestPermissionNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        DataRole dataRole = addDataRole(vdb,"myDataRole");
        Permission permission = addPermission(dataRole,"1myPermission");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), permission, "default.permission.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The Permission name must match the specified pattern."));
    }

    // ==============================================================================================
    // Mask Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestMaskNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        DataRole dataRole = addDataRole(vdb,"myDataRole");
        Permission permission = addPermission(dataRole,"myPermission");
        Mask mask = addMask(permission,"myMask");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), mask, "default.mask.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestMaskNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        DataRole dataRole = addDataRole(vdb,"myDataRole");
        Permission permission = addPermission(dataRole,"myPermission");
        Mask mask = addMask(permission,"1myMask");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), mask, "default.mask.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The Mask name must match the specified pattern."));
    }

    // ==============================================================================================
    // Condition Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestConditionNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        DataRole dataRole = addDataRole(vdb,"myDataRole");
        Permission permission = addPermission(dataRole,"myPermission");
        Condition condition = addCondition(permission,"myCondition");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), condition, "default.condition.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestConditionNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        DataRole dataRole = addDataRole(vdb,"myDataRole");
        Permission permission = addPermission(dataRole,"myPermission");
        Condition condition = addCondition(permission,"1myCondition");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), condition, "default.condition.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The Condition name must match the specified pattern."));
    }

    // ==============================================================================================
    // Translator Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestTranslatorNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Translator translator = addTranslator(vdb,"myTranslator","transType");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), translator, "default.translator.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestTranslatorNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Translator translator = addTranslator(vdb,"1myTranslator","transType");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), translator, "default.translator.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The Translator name must match the specified pattern."));
    }

    // ==============================================================================================
    // Entry Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestEntryNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Entry entry = addEntry(vdb,"myEntry");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), entry, "default.entry.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestEntryNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Entry entry = addEntry(vdb,"1myEntry");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), entry, "default.entry.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The Entry name must match the specified pattern."));
    }

    // ==============================================================================================
    // VdbImport Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestVdbImportNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        VdbImport vdbImport = addVdbImport(vdb,"myImport");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), vdbImport, "default.importVdb.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestVdbImportNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        VdbImport vdbImport = addVdbImport(vdb,"1myImport");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), vdbImport, "default.importVdb.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The VdbImport name must match the specified pattern."));
    }

    // ==============================================================================================
    // Model Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestModelNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), model, "default.declarativeModel.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestModelNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"1myModel");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), model, "default.declarativeModel.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The Model name must match the specified pattern."));
    }

    // ==============================================================================================
    // Table Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestTableNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), table, "default.table.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestTableNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"1myTable");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), table, "default.table.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The Table name must match the specified pattern."));
    }

    @Test
    public void shouldTestTableColumnCountValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        addColumn(table, "myColumn");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), table, "default.table.column.childCount");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestTableColumnCountValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), table, "default.table.column.childCount");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "A Table must have at least one column."));
    }

    @Test
    public void shouldTestTableColumnUniqueNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        addColumn(table, "myColumn1");
        addColumn(table, "myColumn2");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), table, "default.table.column.childSNSValidation");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestTableColumnUniqueNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        addColumn(table, "myColumn1");
        addColumn(table, "myColumn1");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), table, "default.table.column.childSNSValidation");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "A Table column must have a unique name."));
    }

    @Test
    public void shouldTestTableNameInSourceValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        table.setNameInSource(getTransaction(), "aNIS");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), table, "default.table.nameInSource.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestTableNameInSourceValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), table, "default.table.nameInSource.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.WARNING));
        assertThat( results[0].getMessage(), is( "The Table 'NAMEINSOURCE' property is required and must match the specified pattern."));
    }

    @Test
    public void shouldTestMaterializedTableColumnTypesValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        table.setMaterialized(getTransaction(), true);
        table.setSchemaElementType(getTransaction(), SchemaElement.SchemaElementType.VIRTUAL);
        Column col = addColumn(table,"myColumn");
        col.setDatatypeName(getTransaction(), "string");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), table, "default.materializedTable.column.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestMaterializedTableColumnTypesValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        table.setMaterialized(getTransaction(), true);
        table.setSchemaElementType(getTransaction(), SchemaElement.SchemaElementType.VIRTUAL);
        Column col = addColumn(table,"myColumn");
        col.setDatatypeName(getTransaction(), "object");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), table, "default.materializedTable.column.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "A Materialized table cannot have columns of type 'object'"));
    }



    // ==============================================================================================
    // View Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestViewNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        View view = addView(model,"myView");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), view, "default.view.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestViewNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        View view = addView(model,"1myView");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), view, "default.view.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The View name must match the specified pattern."));
    }

    @Test
    public void shouldTestViewColumnCountValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        View view = addView(model,"myView");
        addColumn(view, "myColumn");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), view, "default.view.column.childCount");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestViewColumnCountValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        View view = addView(model,"myView");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), view, "default.view.column.childCount");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "A View must have at least one column."));
    }

    @Test
    public void shouldTestViewColumnUniqueNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        View view = addView(model,"myView");
        addColumn(view, "myColumn1");
        addColumn(view, "myColumn2");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), view, "default.view.column.childSNSValidation");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestViewColumnUniqueNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        View view = addView(model,"myView");
        addColumn(view, "myColumn1");
        addColumn(view, "myColumn1");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), view, "default.view.column.childSNSValidation");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "A View column must have a unique name."));
    }

    // ==============================================================================================
    // ModelSource Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestModelSourceNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        ModelSource modelSource = addSource(model,"mySource");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), modelSource, "default.modelSource.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestModelSourceNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        ModelSource modelSource = addSource(model,"1mySource");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), modelSource, "default.modelSource.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The ModelSource name must match the specified pattern."));
    }

    // ==============================================================================================
    // PushdownFunction Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestPushdownFunctionNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        PushdownFunction pf = addPushdownFunction(model,"myPF");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), pf, "default.pushdownFunction.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestPushdownFunctionNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        PushdownFunction pf = addPushdownFunction(model,"1myPF");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), pf, "default.pushdownFunction.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The PushdownFunction name must match the specified pattern."));
    }

    // ==============================================================================================
    // UserDefinedFunction Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestUserDefinedFunctionNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        UserDefinedFunction udf = addUserDefinedFunction(model,"myUDF");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), udf, "default.userDefinedFunction.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestUserDefinedFunctionNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        UserDefinedFunction udf = addUserDefinedFunction(model,"1myUDF");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), udf, "default.userDefinedFunction.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The UserDefinedFunction name must match the specified pattern."));
    }

    @Test
    public void shouldTestUserDefinedFunctionCategoryValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        UserDefinedFunction udf = addUserDefinedFunction(model,"myUDF");
        udf.setCategory(getTransaction(), "aCategory");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), udf, "default.userDefinedFunction.category.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestUserDefinedFunctionCategoryValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        UserDefinedFunction udf = addUserDefinedFunction(model,"1myUDF");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), udf, "default.userDefinedFunction.category.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The UserDefinedFunction 'CATEGORY' property is required and must match the specified pattern."));
    }

    @Test
    public void shouldTestUserDefinedFunctionJavaClassValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        UserDefinedFunction udf = addUserDefinedFunction(model,"myUDF");
        udf.setJavaClass(getTransaction(), "aClass");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), udf, "default.userDefinedFunction.javaClass.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestUserDefinedFunctionJavaClassValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        UserDefinedFunction udf = addUserDefinedFunction(model,"1myUDF");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), udf, "default.userDefinedFunction.javaClass.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The UserDefinedFunction 'JAVA_CLASS' property is required and must match the specified pattern."));
    }

    @Test
    public void shouldTestUserDefinedFunctionJavaMethodValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        UserDefinedFunction udf = addUserDefinedFunction(model,"myUDF");
        udf.setJavaMethod(getTransaction(), "aMethod");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), udf, "default.userDefinedFunction.javaMethod.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestUserDefinedFunctionJavaMethodValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        UserDefinedFunction udf = addUserDefinedFunction(model,"1myUDF");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), udf, "default.userDefinedFunction.javaMethod.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The UserDefinedFunction 'JAVA_METHOD' property is required and must match the specified pattern."));
    }

    @Test
    public void shouldTestUserDefinedFunctionReturnParameterValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        UserDefinedFunction udf = addUserDefinedFunction(model,"myUDF");
        Parameter param = addParameter(udf,"returnParam");
        param.setDirection(getTransaction(), Direction.OUT);

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), udf, "default.userDefinedFunction.returnParameterCount");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestUserDefinedFunctionReturnParameterValidationFailureWrongNumberParams() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        UserDefinedFunction udf = addUserDefinedFunction(model,"myUDF");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), udf, "default.userDefinedFunction.returnParameterCount");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "A UserDefinedFunction must have one return parameter."));
    }

    @Test
    public void shouldTestUserDefinedFunctionReturnParameterValidationFailureWrongTypeParam() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        UserDefinedFunction udf = addUserDefinedFunction(model,"myUDF");
        Parameter param = addParameter(udf,"aParam");
        param.setDirection(getTransaction(), Direction.IN);

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), udf, "default.userDefinedFunction.returnParameterCount");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "A UserDefinedFunction must have one return parameter."));
    }

    // ==============================================================================================
    // VirtualProcedure Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestVirtualProcNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        VirtualProcedure vp = addVirtualProcedure(model,"myVP");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), vp, "default.virtualProcedure.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestVirtualProcNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        VirtualProcedure vp = addVirtualProcedure(model,"1myVP");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), vp, "default.virtualProcedure.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The VirtualProcedure name must match the specified pattern."));
    }

    // ==============================================================================================
    // StoredProcedure Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestStoredProcNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        StoredProcedure sp = addStoredProcedure(model,"mySP");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), sp, "default.storedProcedure.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestStoredProcNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        StoredProcedure sp = addStoredProcedure(model,"1mySP");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), sp, "default.storedProcedure.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The StoredProcedure name must match the specified pattern."));
    }

    // ==============================================================================================
    // AccessPattern Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestAccessPatternNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        AccessPattern ap = addAccessPattern(table,"myAP");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), ap, "default.accessPattern.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestAccessPatternNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        AccessPattern ap = addAccessPattern(table,"1myAP");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), ap, "default.accessPattern.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The AccessPattern name must match the specified pattern."));
    }

    // ==============================================================================================
    // Index Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestIndexNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Index index = addIndex(table,"myIndex");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), index, "default.index.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestIndexNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Index index = addIndex(table,"1myIndex");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), index, "default.index.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The Index name must match the specified pattern."));
    }

    // ==============================================================================================
    // PrimaryKey Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestPrimaryKeyNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        PrimaryKey pk = addPrimaryKey(table,"myPK");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), pk, "default.primaryKey.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestPrimaryKeyNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        PrimaryKey pk = addPrimaryKey(table,"1myPK");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), pk, "default.primaryKey.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The PrimaryKey name must match the specified pattern."));
    }

    // ==============================================================================================
    // ForeignKey Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestForeignKeyNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Table refTable = addTable(model,"refTable");
        ForeignKey fk = addForeignKey(table,"myFK",refTable);

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), fk, "default.foreignKey.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestForeignKeyNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Table refTable = addTable(model,"refTable");
        ForeignKey fk = addForeignKey(table,"1myFK",refTable);

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), fk, "default.foreignKey.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The ForeignKey name must match the specified pattern."));
    }

    // ==============================================================================================
    // UniqueConstraint Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestUniqueConstraintNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        UniqueConstraint uc = addUniqueConstraint(table,"myUC");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), uc, "default.uniqueConstraint.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestUniqueConstraintNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        UniqueConstraint uc = addUniqueConstraint(table,"1myUC");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), uc, "default.uniqueConstraint.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The UniqueConstraint name must match the specified pattern."));
    }

    // ==============================================================================================
    // Column Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestColumnNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"myColumn");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), col, "default.column.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestColumnNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"1myColumn");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), col, "default.column.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The Column name must match the specified pattern."));
    }

    @Test
    public void shouldTestColumnDatatypeValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"myColumn");
        col.setDatatypeName(getTransaction(), "string");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), col, "default.column.datatype.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestColumnDatatypeValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"myColumn");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), col, "default.column.datatype.propertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The Column 'ddl:datatypeName' property is required and must match the specified pattern."));
    }

    @Test
    public void shouldTestStringColumnLengthValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"myColumn");
        col.setDatatypeName(getTransaction(), "string");
        col.setLength(getTransaction(), 1);

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), col, "default.column.stringDatatype.lengthPropertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldStringColumnNoLengthValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"myColumn");
        col.setDatatypeName(getTransaction(), "string");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), col, "default.column.stringDatatype.lengthPropertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The length property of a string Column is required and must be 1 or greater."));
    }

    @Test
    public void shouldStringColumnWrongLengthValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"myColumn");
        col.setDatatypeName(getTransaction(), "string");
        col.setLength(getTransaction(), 0);

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), col, "default.column.stringDatatype.lengthPropertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The length property of a string Column is required and must be 1 or greater."));
    }

    @Test
    public void shouldTestNumericColumnPrecisionValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"myColumn");
        col.setDatatypeName(getTransaction(), "decimal");
        col.setPrecision(getTransaction(), 1);

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), col, "default.column.numericDatatype.precisionPropertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestNumericColumnNoPrecisionValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"myColumn");
        col.setDatatypeName(getTransaction(), "decimal");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), col, "default.column.numericDatatype.precisionPropertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The precision property of a numeric Column is required and must be 1 or greater."));
    }

    @Test
    public void shouldTestNumericColumnWrongPrecisionValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"myColumn");
        col.setDatatypeName(getTransaction(), "decimal");
        col.setPrecision(getTransaction(), 0);

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), col, "default.column.numericDatatype.precisionPropertyValue");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The precision property of a numeric Column is required and must be 1 or greater."));
    }

    // ==============================================================================================
    // Parameter Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestParameterNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        StoredProcedure sp = addStoredProcedure(model,"mySP");
        Parameter param = addParameter(sp,"myParam");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), param, "default.parameter.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestParameterNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        StoredProcedure sp = addStoredProcedure(model,"mySP");
        Parameter param = addParameter(sp,"1myParam");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), param, "default.parameter.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The Parameter name must match the specified pattern."));
    }

    // ==============================================================================================
    // DataTypeResultSet Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestDataTypeResultSetNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        StoredProcedure sp = addStoredProcedure(model,"mySP");
        DataTypeResultSet rs = addDataTypeResultSet(sp);

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), rs, "default.dataTypeResultSet.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    // ==============================================================================================
    // TabularResultSet Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestTabularResultSetNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        StoredProcedure sp = addStoredProcedure(model,"mySP");
        TabularResultSet rs = addTabularResultSet(sp);

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), rs, "default.tabularResultSet.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    // ==============================================================================================
    // ResultSetColumn Validation Rules
    // ==============================================================================================

    @Test
    public void shouldTestResultSetColumnNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        StoredProcedure sp = addStoredProcedure(model,"mySP");
        TabularResultSet rs = addTabularResultSet(sp);
        ResultSetColumn rsCol = addResultSetColumn(rs,"myRsCol");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), rsCol, "default.resultSetColumn.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestResultSetColumnNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        StoredProcedure sp = addStoredProcedure(model,"mySP");
        TabularResultSet rs = addTabularResultSet(sp);
        ResultSetColumn rsCol = addResultSetColumn(rs,"1myRsCol");

        final Result[] results = _repo.getValidationManager().evaluate(getTransaction(), rsCol, "default.resultSetColumn.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The ResultSetColumn name must match the specified pattern."));
    }
}
