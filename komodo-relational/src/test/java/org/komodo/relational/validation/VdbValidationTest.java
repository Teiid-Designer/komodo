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
import java.io.File;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.model.AccessPattern;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Index;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.relational.model.ResultSetColumn;
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

    private final File RULES_FILE = new File( getClass().getClassLoader().getResource("relationalValidationRulesDefault.xml").getFile() );
    
    @Before
    public void init() throws Exception {
        _repo.getValidationManager().validateRules( RULES_FILE );
        _repo.getValidationManager().importRules( RULES_FILE , getTransaction(), true);
    }
    
    @Test
    public void shouldGetAllRules() throws Exception {
        final Rule[] rules = _repo.getValidationManager().getAllRules(getTransaction());
        assertThat( rules.length, is( 42 ) );
    }

    @Test
    public void shouldGetValidRulesForVdb() throws Exception {
        Vdb vdb = createVdb("myVDB");
        
        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),vdb);
        assertThat( rules.length, is( 7 ) );
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
        assertThat( rules.length, is( 6 ) );
    }
    
    @Test
    public void shouldGetValidRulesForView() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        View view = addView(model,"myView");
        
        final Rule[] rules = _repo.getValidationManager().getRules(getTransaction(),view);
        assertThat( rules.length, is( 4 ) );
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
        assertThat( rules.length, is( 1 ) );
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

    @Test
    public void shouldTestVdbNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        
        final Result[] results = _repo.getValidationManager().evaluate(vdb, getTransaction(), "default.vdb.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestVdbNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("1myVDB");
        
        final Result[] results = _repo.getValidationManager().evaluate(vdb, getTransaction(), "default.vdb.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The VDB name must match the specified pattern."));
    }

    @Test
    public void shouldTestDataRoleNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        DataRole dataRole = addDataRole(vdb,"myDataRole");
        
        final Result[] results = _repo.getValidationManager().evaluate(dataRole, getTransaction(), "default.dataRole.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }
    
    @Test
    public void shouldTestDataRoleNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        DataRole dataRole = addDataRole(vdb,"1myDataRole");
        
        final Result[] results = _repo.getValidationManager().evaluate(dataRole, getTransaction(), "default.dataRole.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The DataRole name must match the specified pattern."));
    }

    @Test
    public void shouldTestPermissionNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        DataRole dataRole = addDataRole(vdb,"myDataRole");
        Permission permission = addPermission(dataRole,"myPermission");
        
        final Result[] results = _repo.getValidationManager().evaluate(permission, getTransaction(), "default.permission.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }
    
    @Test
    public void shouldTestPermissionNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        DataRole dataRole = addDataRole(vdb,"myDataRole");
        Permission permission = addPermission(dataRole,"1myPermission");
        
        final Result[] results = _repo.getValidationManager().evaluate(permission, getTransaction(), "default.permission.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The Permission name must match the specified pattern."));
    }

    @Test
    public void shouldTestMaskNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        DataRole dataRole = addDataRole(vdb,"myDataRole");
        Permission permission = addPermission(dataRole,"myPermission");
        Mask mask = addMask(permission,"myMask");
        
        final Result[] results = _repo.getValidationManager().evaluate(mask, getTransaction(), "default.mask.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }
    
    @Test
    public void shouldTestMaskNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        DataRole dataRole = addDataRole(vdb,"myDataRole");
        Permission permission = addPermission(dataRole,"myPermission");
        Mask mask = addMask(permission,"1myMask");
        
        final Result[] results = _repo.getValidationManager().evaluate(mask, getTransaction(), "default.mask.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The Mask name must match the specified pattern."));
    }

    @Test
    public void shouldTestConditionNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        DataRole dataRole = addDataRole(vdb,"myDataRole");
        Permission permission = addPermission(dataRole,"myPermission");
        Condition condition = addCondition(permission,"myCondition");
        
        final Result[] results = _repo.getValidationManager().evaluate(condition, getTransaction(), "default.condition.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }
    
    @Test
    public void shouldTestConditionNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        DataRole dataRole = addDataRole(vdb,"myDataRole");
        Permission permission = addPermission(dataRole,"myPermission");
        Condition condition = addCondition(permission,"1myCondition");
        
        final Result[] results = _repo.getValidationManager().evaluate(condition, getTransaction(), "default.condition.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The Condition name must match the specified pattern."));
    }
    
    @Test
    public void shouldTestTranslatorNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Translator translator = addTranslator(vdb,"myTranslator","transType");
        
        final Result[] results = _repo.getValidationManager().evaluate(translator, getTransaction(), "default.translator.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }
    
    @Test
    public void shouldTestTranslatorNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Translator translator = addTranslator(vdb,"1myTranslator","transType");
        
        final Result[] results = _repo.getValidationManager().evaluate(translator, getTransaction(), "default.translator.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The Translator name must match the specified pattern."));
    }
    
    @Test
    public void shouldTestEntryNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Entry entry = addEntry(vdb,"myEntry");
        
        final Result[] results = _repo.getValidationManager().evaluate(entry, getTransaction(), "default.entry.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }
    
    @Test
    public void shouldTestEntryNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Entry entry = addEntry(vdb,"1myEntry");
        
        final Result[] results = _repo.getValidationManager().evaluate(entry, getTransaction(), "default.entry.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The Entry name must match the specified pattern."));
    }
    
    @Test
    public void shouldTestVdbImportNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        VdbImport vdbImport = addVdbImport(vdb,"myImport");
        
        final Result[] results = _repo.getValidationManager().evaluate(vdbImport, getTransaction(), "default.importVdb.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }
    
    @Test
    public void shouldTestVdbImportNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        VdbImport vdbImport = addVdbImport(vdb,"1myImport");
        
        final Result[] results = _repo.getValidationManager().evaluate(vdbImport, getTransaction(), "default.importVdb.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The VdbImport name must match the specified pattern."));
    }

    @Test
    public void shouldTestModelNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        
        final Result[] results = _repo.getValidationManager().evaluate(model, getTransaction(), "default.declarativeModel.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }
    
    @Test
    public void shouldTestModelNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"1myModel");
        
        final Result[] results = _repo.getValidationManager().evaluate(model, getTransaction(), "default.declarativeModel.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The Model name must match the specified pattern."));
    }

    @Test
    public void shouldTestTableNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        
        final Result[] results = _repo.getValidationManager().evaluate(table, getTransaction(), "default.table.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }
    
    @Test
    public void shouldTestTableNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"1myTable");
        
        final Result[] results = _repo.getValidationManager().evaluate(table, getTransaction(), "default.table.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The Table name must match the specified pattern."));
    }

    @Test
    public void shouldTestViewNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        View view = addView(model,"myView");
        
        final Result[] results = _repo.getValidationManager().evaluate(view, getTransaction(), "default.view.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }
    
    @Test
    public void shouldTestViewNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        View view = addView(model,"1myView");
        
        final Result[] results = _repo.getValidationManager().evaluate(view, getTransaction(), "default.view.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The View name must match the specified pattern."));
    }

    @Test
    public void shouldTestModelSourceNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        ModelSource modelSource = addSource(model,"mySource");
        
        final Result[] results = _repo.getValidationManager().evaluate(modelSource, getTransaction(), "default.modelSource.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }
    
    @Test
    public void shouldTestModelSourceNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        ModelSource modelSource = addSource(model,"1mySource");
        
        final Result[] results = _repo.getValidationManager().evaluate(modelSource, getTransaction(), "default.modelSource.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The ModelSource name must match the specified pattern."));
    }

    @Test
    public void shouldTestPushdownFunctionNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        PushdownFunction pf = addPushdownFunction(model,"myPF");
        
        final Result[] results = _repo.getValidationManager().evaluate(pf, getTransaction(), "default.pushdownFunction.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }
    
    @Test
    public void shouldTestPushdownFunctionNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        PushdownFunction pf = addPushdownFunction(model,"1myPF");
        
        final Result[] results = _repo.getValidationManager().evaluate(pf, getTransaction(), "default.pushdownFunction.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The PushdownFunction name must match the specified pattern."));
    }

    @Test
    public void shouldTestUserDefinedFunctionNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        UserDefinedFunction udf = addUserDefinedFunction(model,"myUDF");
        
        final Result[] results = _repo.getValidationManager().evaluate(udf, getTransaction(), "default.userDefinedFunction.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }
    
    @Test
    public void shouldTestUserDefinedFunctionNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        UserDefinedFunction udf = addUserDefinedFunction(model,"1myUDF");
        
        final Result[] results = _repo.getValidationManager().evaluate(udf, getTransaction(), "default.userDefinedFunction.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The UserDefinedFunction name must match the specified pattern."));
    }

    @Test
    public void shouldTestVirtualProcNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        VirtualProcedure vp = addVirtualProcedure(model,"myVP");
        
        final Result[] results = _repo.getValidationManager().evaluate(vp, getTransaction(), "default.virtualProcedure.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }
    
    @Test
    public void shouldTestVirtualProcNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        VirtualProcedure vp = addVirtualProcedure(model,"1myVP");
        
        final Result[] results = _repo.getValidationManager().evaluate(vp, getTransaction(), "default.virtualProcedure.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The VirtualProcedure name must match the specified pattern."));
    }

    @Test
    public void shouldTestStoredProcNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        StoredProcedure sp = addStoredProcedure(model,"mySP");
        
        final Result[] results = _repo.getValidationManager().evaluate(sp, getTransaction(), "default.storedProcedure.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }
    
    @Test
    public void shouldTestStoredProcNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        StoredProcedure sp = addStoredProcedure(model,"1mySP");
        
        final Result[] results = _repo.getValidationManager().evaluate(sp, getTransaction(), "default.storedProcedure.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The StoredProcedure name must match the specified pattern."));
    }
    
    @Test
    public void shouldTestAccessPatternNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        AccessPattern ap = addAccessPattern(table,"myAP");
        
        final Result[] results = _repo.getValidationManager().evaluate(ap, getTransaction(), "default.accessPattern.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }
    
    @Test
    public void shouldTestAccessPatternNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        AccessPattern ap = addAccessPattern(table,"1myAP");
        
        final Result[] results = _repo.getValidationManager().evaluate(ap, getTransaction(), "default.accessPattern.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The AccessPattern name must match the specified pattern."));
    }

    @Test
    public void shouldTestIndexPatternNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Index index = addIndex(table,"myIndex");
        
        final Result[] results = _repo.getValidationManager().evaluate(index, getTransaction(), "default.index.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }
    
    @Test
    public void shouldTestIndexPatternNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Index index = addIndex(table,"1myIndex");
        
        final Result[] results = _repo.getValidationManager().evaluate(index, getTransaction(), "default.index.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The Index name must match the specified pattern."));
    }

    @Test
    public void shouldTestPrimaryKeyNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        PrimaryKey pk = addPrimaryKey(table,"myPK");
        
        final Result[] results = _repo.getValidationManager().evaluate(pk, getTransaction(), "default.primaryKey.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }
    
    @Test
    public void shouldTestPrimaryKeyNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        PrimaryKey pk = addPrimaryKey(table,"1myPK");
        
        final Result[] results = _repo.getValidationManager().evaluate(pk, getTransaction(), "default.primaryKey.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The PrimaryKey name must match the specified pattern."));
    }

    @Test
    public void shouldTestForeignKeyNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Table refTable = addTable(model,"refTable");
        ForeignKey fk = addForeignKey(table,"myFK",refTable);
        
        final Result[] results = _repo.getValidationManager().evaluate(fk, getTransaction(), "default.foreignKey.nodeName");
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
        
        final Result[] results = _repo.getValidationManager().evaluate(fk, getTransaction(), "default.foreignKey.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The ForeignKey name must match the specified pattern."));
    }

    @Test
    public void shouldTestUniqueConstraintNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        UniqueConstraint uc = addUniqueConstraint(table,"myUC");
        
        final Result[] results = _repo.getValidationManager().evaluate(uc, getTransaction(), "default.uniqueConstraint.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }
    
    @Test
    public void shouldTestUniqueConstraintNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        UniqueConstraint uc = addUniqueConstraint(table,"1myUC");
        
        final Result[] results = _repo.getValidationManager().evaluate(uc, getTransaction(), "default.uniqueConstraint.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The UniqueConstraint name must match the specified pattern."));
    }

    @Test
    public void shouldTestColumnNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"myColumn");
        
        final Result[] results = _repo.getValidationManager().evaluate(col, getTransaction(), "default.column.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }
    
    @Test
    public void shouldTestColumnNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        Table table = addTable(model,"myTable");
        Column col = addColumn(table,"1myColumn");
        
        final Result[] results = _repo.getValidationManager().evaluate(col, getTransaction(), "default.column.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The Column name must match the specified pattern."));
    }

    @Test
    public void shouldTestParameterNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        StoredProcedure sp = addStoredProcedure(model,"mySP");
        Parameter param = addParameter(sp,"myParam");
        
        final Result[] results = _repo.getValidationManager().evaluate(param, getTransaction(), "default.parameter.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }
    
    @Test
    public void shouldTestParameterNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        StoredProcedure sp = addStoredProcedure(model,"mySP");
        Parameter param = addParameter(sp,"1myParam");
        
        final Result[] results = _repo.getValidationManager().evaluate(param, getTransaction(), "default.parameter.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The Parameter name must match the specified pattern."));
    }

    @Test
    public void shouldTestDataTypeResultSetNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        StoredProcedure sp = addStoredProcedure(model,"mySP");
        DataTypeResultSet rs = addDataTypeResultSet(sp);
        
        final Result[] results = _repo.getValidationManager().evaluate(rs, getTransaction(), "default.dataTypeResultSet.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestTabularResultSetNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        StoredProcedure sp = addStoredProcedure(model,"mySP");
        TabularResultSet rs = addTabularResultSet(sp);
        
        final Result[] results = _repo.getValidationManager().evaluate(rs, getTransaction(), "default.tabularResultSet.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestResultSetColumnNodeNameValidationSuccess() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        StoredProcedure sp = addStoredProcedure(model,"mySP");
        TabularResultSet rs = addTabularResultSet(sp);
        ResultSetColumn rsCol = addResultSetColumn(rs,"myRsCol");
        
        final Result[] results = _repo.getValidationManager().evaluate(rsCol, getTransaction(), "default.resultSetColumn.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( true ));
    }

    @Test
    public void shouldTestResultSetNodeNameValidationFailure() throws Exception {
        Vdb vdb = createVdb("myVDB");
        Model model = addModel(vdb,"myModel");
        StoredProcedure sp = addStoredProcedure(model,"mySP");
        TabularResultSet rs = addTabularResultSet(sp);
        ResultSetColumn rsCol = addResultSetColumn(rs,"1myRsCol");
        
        final Result[] results = _repo.getValidationManager().evaluate(rsCol, getTransaction(), "default.resultSetColumn.nodeName");
        assertThat( results.length, is( 1 ) );
        assertThat( results[0].isOK(), is( false ));
        assertThat( results[0].getLevel(), is( Level.ERROR));
        assertThat( results[0].getMessage(), is( "The ResultSetColumn name must match the specified pattern."));
    }
}
