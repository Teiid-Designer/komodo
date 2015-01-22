package org.komodo.relational;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.komodo.relational.internal.model.AccessPatternImplTest;
import org.komodo.relational.internal.model.ColumnImplTest;
import org.komodo.relational.internal.model.ForeignKeyImplTest;
import org.komodo.relational.internal.model.IndexImplTest;
import org.komodo.relational.internal.model.ModelImplTest;
import org.komodo.relational.internal.model.ParameterImplTest;
import org.komodo.relational.internal.model.PrimaryKeyImplTest;
import org.komodo.relational.internal.model.ProcedureImplTest;
import org.komodo.relational.internal.model.ProcedureResultSetImplTest;
import org.komodo.relational.internal.model.StatementOptionImplTest;
import org.komodo.relational.internal.model.TableImplTest;
import org.komodo.relational.internal.model.UniqueConstraintImplTest;
import org.komodo.relational.internal.model.ViewImplTest;
import org.komodo.relational.internal.vdb.ConditionImplTest;
import org.komodo.relational.internal.vdb.DataRoleImplTest;
import org.komodo.relational.internal.vdb.EntryImplTest;
import org.komodo.relational.internal.vdb.MaskImplTest;
import org.komodo.relational.internal.vdb.PermissionImplTest;
import org.komodo.relational.internal.vdb.TranslatorImplTest;
import org.komodo.relational.internal.vdb.VdbImplTest;
import org.komodo.relational.internal.vdb.VdbImportImplTest;
import org.komodo.relational.model.legacy.TestAccessPattern;
import org.komodo.relational.model.legacy.TestColumn;
import org.komodo.relational.model.legacy.TestForeignKey;
import org.komodo.relational.model.legacy.TestModel;
import org.komodo.relational.model.legacy.TestParameter;
import org.komodo.relational.model.legacy.TestPrimaryKey;
import org.komodo.relational.model.legacy.TestProcedure;
import org.komodo.relational.model.legacy.TestProcedureResultSet;
import org.komodo.relational.model.legacy.TestSchema;
import org.komodo.relational.model.legacy.TestTable;
import org.komodo.relational.model.legacy.TestUniqueConstraint;
import org.komodo.relational.model.legacy.TestView;
import org.komodo.relational.workspace.WorkspaceManagerTest;


/**
 * Suite for all unit tests
 */
@RunWith(Suite.class)
@Suite.SuiteClasses({
	TestTable.class,
	TestColumn.class,
	TestProcedure.class,
	TestParameter.class,
	TestAccessPattern.class,
	TestForeignKey.class,
	TestModel.class,
	TestPrimaryKey.class,
	TestProcedureResultSet.class,
	TestSchema.class,
	TestUniqueConstraint.class,
	TestView.class,
	AccessPatternImplTest.class,
	ColumnImplTest.class,
	ForeignKeyImplTest.class,
    IndexImplTest.class,
    ModelImplTest.class,
    ParameterImplTest.class,
    PrimaryKeyImplTest.class,
    ProcedureImplTest.class,
    ProcedureResultSetImplTest.class,
    StatementOptionImplTest.class,
    TableImplTest.class,
    UniqueConstraintImplTest.class,
    ViewImplTest.class,
    ConditionImplTest.class,
    DataRoleImplTest.class,
    EntryImplTest.class,
    MaskImplTest.class,
    PermissionImplTest.class,
    TranslatorImplTest.class,
    VdbImplTest.class,
    VdbImportImplTest.class,
    WorkspaceManagerTest.class,
    })
public class AllTests {
    // nothing to do
}
