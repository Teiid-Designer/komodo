package org.komodo.relational;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.komodo.relational.connection.internal.ConnectionImplTest;
import org.komodo.relational.dataservice.internal.DataserviceImplTest;
import org.komodo.relational.importer.ddl.TestTeiidDdlImporter;
import org.komodo.relational.importer.vdb.TestTeiidVdbImporter;
import org.komodo.relational.model.internal.AbstractProcedureImplTest;
import org.komodo.relational.model.internal.AccessPatternImplTest;
import org.komodo.relational.model.internal.ColumnImplTest;
import org.komodo.relational.model.internal.DataTypeResultSetImplTest;
import org.komodo.relational.model.internal.ForeignKeyImplTest;
import org.komodo.relational.model.internal.FunctionImplTest;
import org.komodo.relational.model.internal.IndexImplTest;
import org.komodo.relational.model.internal.ModelImplTest;
import org.komodo.relational.model.internal.ParameterImplTest;
import org.komodo.relational.model.internal.PrimaryKeyImplTest;
import org.komodo.relational.model.internal.PushdownFunctionImplTest;
import org.komodo.relational.model.internal.RelationalObjectImplTest;
import org.komodo.relational.model.internal.ResultSetColumnImplTest;
import org.komodo.relational.model.internal.SchemaImplTest;
import org.komodo.relational.model.internal.StatementOptionImplTest;
import org.komodo.relational.model.internal.StoredProcedureImplTest;
import org.komodo.relational.model.internal.TableConstraintTest;
import org.komodo.relational.model.internal.TableImplTest;
import org.komodo.relational.model.internal.TabularResultSetImplTest;
import org.komodo.relational.model.internal.UniqueConstraintImplTest;
import org.komodo.relational.model.internal.UserDefinedFunctionImplTest;
import org.komodo.relational.model.internal.ViewImplTest;
import org.komodo.relational.model.internal.VirtualProcedureImplTest;
import org.komodo.relational.teiid.internal.TeiidImplTest;
import org.komodo.relational.template.TeiidTemplatesTest;
import org.komodo.relational.validation.VdbValidationTest;
import org.komodo.relational.vdb.internal.ConditionImplTest;
import org.komodo.relational.vdb.internal.DataRoleImplTest;
import org.komodo.relational.vdb.internal.EntryImplTest;
import org.komodo.relational.vdb.internal.MaskImplTest;
import org.komodo.relational.vdb.internal.ModelSourceImplTest;
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
    RelationalObjectImplTest.class,

    // Dataservice
    DataserviceImplTest.class,

    // Connection
    ConnectionImplTest.class,

    // Model
    AbstractProcedureImplTest.class,
	AccessPatternImplTest.class,
	ColumnImplTest.class,
	DataTypeResultSetImplTest.class,
	ForeignKeyImplTest.class,
	FunctionImplTest.class,
    IndexImplTest.class,
    ModelImplTest.class,
    ParameterImplTest.class,
    PrimaryKeyImplTest.class,
    PushdownFunctionImplTest.class,
    RelationalObjectImplTest.class,
    ResultSetColumnImplTest.class,
    SchemaImplTest.class,
    StatementOptionImplTest.class,
    StoredProcedureImplTest.class,
    TableConstraintTest.class,
    TableImplTest.class,
    TabularResultSetImplTest.class,
    UniqueConstraintImplTest.class,
    UserDefinedFunctionImplTest.class,
    ViewImplTest.class,
    VirtualProcedureImplTest.class,

    // VDB
    ConditionImplTest.class,
    DataRoleImplTest.class,
    EntryImplTest.class,
    MaskImplTest.class,
    ModelSourceImplTest.class,
    PermissionImplTest.class,
    TranslatorImplTest.class,
    VdbImplTest.class,
    VdbImportImplTest.class,

    // Validation
    VdbValidationTest.class,

    // Teiid
    TeiidImplTest.class,
    TeiidTemplatesTest.class,

    // DDL
    SchemaImplTest.class,

    // Workspace
    WorkspaceManagerTest.class,

    // Import
    TestTeiidVdbImporter.class,
    TestTeiidDdlImporter.class

    })
public class AllTests {
    // nothing to do
}
