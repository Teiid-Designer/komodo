package org.komodo.relational;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.komodo.relational.model.internal.AccessPatternImplTest;
import org.komodo.relational.model.internal.ColumnImplTest;
import org.komodo.relational.model.internal.ForeignKeyImplTest;
import org.komodo.relational.model.internal.FunctionImplTest;
import org.komodo.relational.model.internal.IndexImplTest;
import org.komodo.relational.model.internal.ModelImplTest;
import org.komodo.relational.model.internal.ParameterImplTest;
import org.komodo.relational.model.internal.PrimaryKeyImplTest;
import org.komodo.relational.model.internal.ProcedureImplTest;
import org.komodo.relational.model.internal.ProcedureResultSetImplTest;
import org.komodo.relational.model.internal.StatementOptionImplTest;
import org.komodo.relational.model.internal.TableConstraintTest;
import org.komodo.relational.model.internal.TableImplTest;
import org.komodo.relational.model.internal.UniqueConstraintImplTest;
import org.komodo.relational.model.internal.ViewImplTest;
import org.komodo.relational.vdb.internal.ConditionImplTest;
import org.komodo.relational.vdb.internal.DataRoleImplTest;
import org.komodo.relational.vdb.internal.EntryImplTest;
import org.komodo.relational.vdb.internal.MaskImplTest;
import org.komodo.relational.vdb.internal.PermissionImplTest;
import org.komodo.relational.vdb.internal.TranslatorImplTest;
import org.komodo.relational.vdb.internal.VdbImplTest;
import org.komodo.relational.vdb.internal.VdbImportImplTest;
import org.komodo.relational.workspace.WorkspaceManagerTest;


/**
 * Suite for all unit tests
 */
@RunWith(Suite.class)
@Suite.SuiteClasses({
    // Model
	AccessPatternImplTest.class,
	ColumnImplTest.class,
	ForeignKeyImplTest.class,
	FunctionImplTest.class,
    IndexImplTest.class,
    ModelImplTest.class,
    ParameterImplTest.class,
    PrimaryKeyImplTest.class,
    ProcedureImplTest.class,
    ProcedureResultSetImplTest.class,
    StatementOptionImplTest.class,
    TableConstraintTest.class,
    TableImplTest.class,
    UniqueConstraintImplTest.class,
    ViewImplTest.class,

    // VDB
    ConditionImplTest.class,
    DataRoleImplTest.class,
    EntryImplTest.class,
    MaskImplTest.class,
    PermissionImplTest.class,
    TranslatorImplTest.class,
    VdbImplTest.class,
    VdbImportImplTest.class,

    // Workspace
    WorkspaceManagerTest.class,
    })
public class AllTests {
    // nothing to do
}
