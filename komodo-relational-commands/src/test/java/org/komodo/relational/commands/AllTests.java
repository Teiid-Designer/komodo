package org.komodo.relational.commands;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.komodo.relational.commands.column.ColumnCommandsI18nTest;
import org.komodo.relational.commands.column.ColumnRenameCommandTest;
import org.komodo.relational.commands.column.SetColumnPropertyCommandTest;
import org.komodo.relational.commands.column.UnsetColumnPropertyCommandTest;
import org.komodo.relational.commands.condition.ConditionCommandsI18nTest;
import org.komodo.relational.commands.condition.ConditionRenameCommandTest;
import org.komodo.relational.commands.condition.SetConditionPropertyCommandTest;
import org.komodo.relational.commands.condition.UnsetConditionPropertyCommandTest;
import org.komodo.relational.commands.connection.ConnectionCommandsI18nTest;
import org.komodo.relational.commands.connection.SetConnectionPropertyCommandTest;
import org.komodo.relational.commands.connection.UnsetConnectionPropertyCommandTest;
import org.komodo.relational.commands.datarole.AddMappedRoleCommandTest;
import org.komodo.relational.commands.datarole.AddPermissionCommandTest;
import org.komodo.relational.commands.datarole.DataRoleCommandsI18nTest;
import org.komodo.relational.commands.datarole.DeleteMappedRoleCommandTest;
import org.komodo.relational.commands.datarole.DeletePermissionCommandTest;
import org.komodo.relational.commands.datarole.SetDataRolePropertyCommandTest;
import org.komodo.relational.commands.datarole.ShowMappedRolesCommandTest;
import org.komodo.relational.commands.datarole.ShowPermissionsCommandTest;
import org.komodo.relational.commands.datarole.UnsetDataRolePropertyCommandTest;
import org.komodo.relational.commands.datatyperesultset.DataTypeResultSetCommandsI18nTest;
import org.komodo.relational.commands.datatyperesultset.SetDataTypeResultSetPropertyCommandTest;
import org.komodo.relational.commands.datatyperesultset.UnsetDataTypeResultSetPropertyCommandTest;
import org.komodo.relational.commands.entry.EntryCommandsI18nTest;
import org.komodo.relational.commands.entry.EntryRenameCommandTest;
import org.komodo.relational.commands.entry.SetEntryPropertyCommandTest;
import org.komodo.relational.commands.entry.UnsetEntryPropertyCommandTest;
import org.komodo.relational.commands.foreignkey.AddReferenceColumnCommandTest;
import org.komodo.relational.commands.foreignkey.DeleteReferenceColumnCommandTest;
import org.komodo.relational.commands.foreignkey.ForeignKeyCommandsI18nTest;
import org.komodo.relational.commands.index.IndexCommandsI18nTest;
import org.komodo.relational.commands.index.SetIndexPropertyCommandTest;
import org.komodo.relational.commands.index.UnsetIndexPropertyCommandTest;
import org.komodo.relational.commands.mask.MaskCommandsI18nTest;
import org.komodo.relational.commands.mask.MaskRenameCommandTest;
import org.komodo.relational.commands.mask.SetMaskPropertyCommandTest;
import org.komodo.relational.commands.mask.UnsetMaskPropertyCommandTest;
import org.komodo.relational.commands.model.AddPushdownFunctionCommandTest;
import org.komodo.relational.commands.model.AddSourceCommandTest;
import org.komodo.relational.commands.model.AddStoredProcedureCommandTest;
import org.komodo.relational.commands.model.AddTableCommandTest;
import org.komodo.relational.commands.model.AddUserDefinedFunctionCommandTest;
import org.komodo.relational.commands.model.AddViewCommandTest;
import org.komodo.relational.commands.model.AddVirtualProcedureCommandTest;
import org.komodo.relational.commands.model.DeletePushdownFunctionCommandTest;
import org.komodo.relational.commands.model.DeleteSourceCommandTest;
import org.komodo.relational.commands.model.DeleteStoredProcedureCommandTest;
import org.komodo.relational.commands.model.DeleteTableCommandTest;
import org.komodo.relational.commands.model.DeleteUserDefinedFunctionCommandTest;
import org.komodo.relational.commands.model.DeleteViewCommandTest;
import org.komodo.relational.commands.model.DeleteVirtualProcedureCommandTest;
import org.komodo.relational.commands.model.ModelCommandsI18nTest;
import org.komodo.relational.commands.model.SetModelPropertyCommandTest;
import org.komodo.relational.commands.model.ShowPushdownFunctionsCommandTest;
import org.komodo.relational.commands.model.ShowSourcesCommandTest;
import org.komodo.relational.commands.model.ShowStoredProceduresCommandTest;
import org.komodo.relational.commands.model.ShowTablesCommandTest;
import org.komodo.relational.commands.model.ShowUserDefinedFunctionsCommandTest;
import org.komodo.relational.commands.model.ShowViewsCommandTest;
import org.komodo.relational.commands.model.ShowVirtualProceduresCommandTest;
import org.komodo.relational.commands.model.UnsetModelPropertyCommandTest;
import org.komodo.relational.commands.modelsource.ModelSourceCommandsI18nTest;
import org.komodo.relational.commands.modelsource.SetModelSourcePropertyCommandTest;
import org.komodo.relational.commands.modelsource.UnsetModelSourcePropertyCommandTest;
import org.komodo.relational.commands.parameter.ParameterCommandsI18nTest;
import org.komodo.relational.commands.parameter.SetParameterPropertyCommandTest;
import org.komodo.relational.commands.parameter.UnsetParameterPropertyCommandTest;
import org.komodo.relational.commands.permission.AddConditionCommandTest;
import org.komodo.relational.commands.permission.AddMaskCommandTest;
import org.komodo.relational.commands.permission.DeleteConditionCommandTest;
import org.komodo.relational.commands.permission.DeleteMaskCommandTest;
import org.komodo.relational.commands.permission.PermissionCommandsI18nTest;
import org.komodo.relational.commands.permission.SetPermissionPropertyCommandTest;
import org.komodo.relational.commands.permission.ShowConditionsCommandTest;
import org.komodo.relational.commands.permission.ShowMasksCommandTest;
import org.komodo.relational.commands.permission.UnsetPermissionPropertyCommandTest;
import org.komodo.relational.commands.pushdownfunction.PushdownFunctionCommandsI18nTest;
import org.komodo.relational.commands.pushdownfunction.PushdownFunctionRenameCommandTest;
import org.komodo.relational.commands.pushdownfunction.SetPushdownFunctionPropertyCommandTest;
import org.komodo.relational.commands.pushdownfunction.UnsetPushdownFunctionPropertyCommandTest;
import org.komodo.relational.commands.resultset.ResultSetRenameCommandTest;
import org.komodo.relational.commands.resultsetcolumn.ResultSetColumnCommandsI18nTest;
import org.komodo.relational.commands.resultsetcolumn.SetResultSetColumnPropertyCommandTest;
import org.komodo.relational.commands.resultsetcolumn.UnsetResultSetColumnPropertyCommandTest;
import org.komodo.relational.commands.schema.SchemaCommandsI18nTest;
import org.komodo.relational.commands.schema.SetSchemaPropertyCommandTest;
import org.komodo.relational.commands.schema.UnsetSchemaPropertyCommandTest;
import org.komodo.relational.commands.server.ServerCommandsI18nTest;
import org.komodo.relational.commands.server.ServerConnectCommandTest;
import org.komodo.relational.commands.server.ServerDatasourceCommandTest;
import org.komodo.relational.commands.server.ServerDatasourceTypeCommandTest;
import org.komodo.relational.commands.server.ServerDatasourceTypesCommandTest;
import org.komodo.relational.commands.server.ServerDatasourcesCommandTest;
import org.komodo.relational.commands.server.ServerDeployDatasourceCommandTest;
import org.komodo.relational.commands.server.ServerDeployVdbCommandTest;
import org.komodo.relational.commands.server.ServerDisconnectCommandTest;
import org.komodo.relational.commands.server.ServerSetPropertyCommandTest;
import org.komodo.relational.commands.server.ServerShowPropertiesCommandTest;
import org.komodo.relational.commands.server.ServerTranslatorCommandTest;
import org.komodo.relational.commands.server.ServerTranslatorsCommandTest;
import org.komodo.relational.commands.server.ServerUndeployDatasourceCommandTest;
import org.komodo.relational.commands.server.ServerUndeployVdbCommandTest;
import org.komodo.relational.commands.server.ServerUnsetPropertyCommandTest;
import org.komodo.relational.commands.server.ServerVdbCommandTest;
import org.komodo.relational.commands.server.ServerVdbsCommandTest;
import org.komodo.relational.commands.storedprocedure.SetStoredProcedurePropertyCommandTest;
import org.komodo.relational.commands.storedprocedure.StoredProcedureCommandsI18nTest;
import org.komodo.relational.commands.storedprocedure.StoredProcedureRenameCommandTest;
import org.komodo.relational.commands.storedprocedure.UnsetStoredProcedurePropertyCommandTest;
import org.komodo.relational.commands.table.AddAccessPatternCommandTest;
import org.komodo.relational.commands.table.AddColumnCommandTest;
import org.komodo.relational.commands.table.AddForeignKeyCommandTest;
import org.komodo.relational.commands.table.AddIndexCommandTest;
import org.komodo.relational.commands.table.AddPrimaryKeyCommandTest;
import org.komodo.relational.commands.table.AddUniqueConstraintCommandTest;
import org.komodo.relational.commands.table.DeleteAccessPatternCommandTest;
import org.komodo.relational.commands.table.DeleteColumnCommandTest;
import org.komodo.relational.commands.table.DeleteForeignKeyCommandTest;
import org.komodo.relational.commands.table.DeleteIndexCommandTest;
import org.komodo.relational.commands.table.DeletePrimaryKeyCommandTest;
import org.komodo.relational.commands.table.DeleteUniqueConstraintCommandTest;
import org.komodo.relational.commands.table.SetTablePropertyCommandTest;
import org.komodo.relational.commands.table.ShowAccessPatternsCommandTest;
import org.komodo.relational.commands.table.ShowColumnsCommandTest;
import org.komodo.relational.commands.table.ShowIndexesCommandTest;
import org.komodo.relational.commands.table.ShowUniqueConstraintsCommandTest;
import org.komodo.relational.commands.table.TableCommandsI18nTest;
import org.komodo.relational.commands.table.UnsetTablePropertyCommandTest;
import org.komodo.relational.commands.tableconstraint.AddConstraintColumnCommandTest;
import org.komodo.relational.commands.tableconstraint.DeleteConstraintColumnCommandTest;
import org.komodo.relational.commands.tableconstraint.TableConstraintCommandsI18nTest;
import org.komodo.relational.commands.tabularresultset.TabularResultSetCommandsI18nTest;
import org.komodo.relational.commands.tabularresultset.TabularResultSetSetPropertyCommandTest;
import org.komodo.relational.commands.tabularresultset.TabularResultSetUnsetPropertyCommandTest;
import org.komodo.relational.commands.translator.SetTranslatorPropertyCommandTest;
import org.komodo.relational.commands.translator.TranslatorCommandsI18nTest;
import org.komodo.relational.commands.translator.TranslatorRenameCommandTest;
import org.komodo.relational.commands.translator.UnsetTranslatorPropertyCommandTest;
import org.komodo.relational.commands.userdefinedfunction.SetUserDefinedFunctionPropertyCommandTest;
import org.komodo.relational.commands.userdefinedfunction.UnsetUserDefinedFunctionPropertyCommandTest;
import org.komodo.relational.commands.userdefinedfunction.UserDefinedFunctionCommandsI18nTest;
import org.komodo.relational.commands.vdb.AddDataRoleCommandTest;
import org.komodo.relational.commands.vdb.AddEntryCommandTest;
import org.komodo.relational.commands.vdb.AddImportCommandTest;
import org.komodo.relational.commands.vdb.AddModelCommandTest;
import org.komodo.relational.commands.vdb.AddTranslatorCommandTest;
import org.komodo.relational.commands.vdb.DeleteDataRoleCommandTest;
import org.komodo.relational.commands.vdb.DeleteEntryCommandTest;
import org.komodo.relational.commands.vdb.DeleteImportCommandTest;
import org.komodo.relational.commands.vdb.DeleteModelCommandTest;
import org.komodo.relational.commands.vdb.DeleteTranslatorCommandTest;
import org.komodo.relational.commands.vdb.SetVdbPropertyCommandTest;
import org.komodo.relational.commands.vdb.UnsetVdbPropertyCommandTest;
import org.komodo.relational.commands.vdb.UploadModelCommandTest;
import org.komodo.relational.commands.vdb.VdbCommandsI18nTest;
import org.komodo.relational.commands.vdbimport.SetVdbImportPropertyCommandTest;
import org.komodo.relational.commands.vdbimport.UnsetVdbImportPropertyCommandTest;
import org.komodo.relational.commands.vdbimport.VdbImportCommandsI18nTest;
import org.komodo.relational.commands.vdbimport.VdbImportRenameCommandTest;
import org.komodo.relational.commands.view.SetViewPropertyCommandTest;
import org.komodo.relational.commands.view.UnsetViewPropertyCommandTest;
import org.komodo.relational.commands.view.ViewCommandsI18nTest;
import org.komodo.relational.commands.virtualprocedure.SetVirtualProcedurePropertyCommandTest;
import org.komodo.relational.commands.virtualprocedure.UnsetVirtualProcedurePropertyCommandTest;
import org.komodo.relational.commands.virtualprocedure.VirtualProcedureCommandsI18nTest;
import org.komodo.relational.commands.workspace.CreateConnectionCommandTest;
import org.komodo.relational.commands.workspace.CreateSchemaCommandTest;
import org.komodo.relational.commands.workspace.CreateVdbCommandTest;
import org.komodo.relational.commands.workspace.DeleteConnectionCommandTest;
import org.komodo.relational.commands.workspace.DeleteSchemaCommandTest;
import org.komodo.relational.commands.workspace.DeleteVdbCommandTest;
import org.komodo.relational.commands.workspace.ExportConnectionCommandTest;
import org.komodo.relational.commands.workspace.ExportVdbCommandTest;
import org.komodo.relational.commands.workspace.UploadConnectionCommandTest;
import org.komodo.relational.commands.workspace.UploadVdbCommandTest;
import org.komodo.relational.commands.workspace.WorkspaceCommandsI18nTest;
import org.komodo.relational.commands.workspace.WorkspaceSetPropertyCommandTest;
import org.komodo.relational.commands.workspace.WorkspaceUnsetPropertyCommandTest;


/**
 * Suite for all unit tests
 */
@RunWith(Suite.class)
@Suite.SuiteClasses({
    // Core relational
    FindCommandTest.class,
    SetCustomPropertyCommandTest.class,
    UnsetCustomPropertyCommandTest.class,
    SetCustomOptionCommandTest.class,
    UnsetCustomOptionCommandTest.class,
    RelationalAddChildCommandTest.class,
    RelationalCommandsI18nTest.class,
    RelationalDeleteChildCommandTest.class,
    RelationalAddDescriptorCommandTest.class,
    RelationalRemoveDescriptorCommandTest.class,
    RelationalSetPrimaryTypeCommandTest.class,
    RelationalShowDescriptorsCommandTest.class,

    // Workspace
    CreateConnectionCommandTest.class,
    CreateSchemaCommandTest.class,
    CreateVdbCommandTest.class,
    DeleteConnectionCommandTest.class,
    DeleteSchemaCommandTest.class,
    DeleteVdbCommandTest.class,
//    ImportVdbCommandTest.class,
    UploadVdbCommandTest.class,
    ExportVdbCommandTest.class,
    UploadConnectionCommandTest.class,
    ExportConnectionCommandTest.class,
    WorkspaceCommandsI18nTest.class,
    WorkspaceSetPropertyCommandTest.class,
    WorkspaceUnsetPropertyCommandTest.class,

    // Column
    ColumnCommandsI18nTest.class,
    ColumnRenameCommandTest.class,
    SetColumnPropertyCommandTest.class,
    UnsetColumnPropertyCommandTest.class,

    // Condition
    ConditionCommandsI18nTest.class,
    ConditionRenameCommandTest.class,
    SetConditionPropertyCommandTest.class,
    UnsetConditionPropertyCommandTest.class,

    // DataRole
    AddMappedRoleCommandTest.class,
    AddPermissionCommandTest.class,
    DataRoleCommandsI18nTest.class,
    DeleteMappedRoleCommandTest.class,
    DeletePermissionCommandTest.class,
    SetDataRolePropertyCommandTest.class,
    ShowMappedRolesCommandTest.class,
    ShowPermissionsCommandTest.class,
    UnsetDataRolePropertyCommandTest.class,

    // Connection
    ConnectionCommandsI18nTest.class,
    SetConnectionPropertyCommandTest.class,
    UnsetConnectionPropertyCommandTest.class,

    // DataTypeResultSet
    DataTypeResultSetCommandsI18nTest.class,
    SetDataTypeResultSetPropertyCommandTest.class,
    UnsetDataTypeResultSetPropertyCommandTest.class,

    // Entry
    EntryCommandsI18nTest.class,
    EntryRenameCommandTest.class,
    SetEntryPropertyCommandTest.class,
    UnsetEntryPropertyCommandTest.class,

    // ForeignKey
    AddReferenceColumnCommandTest.class,
    DeleteReferenceColumnCommandTest.class,
    ForeignKeyCommandsI18nTest.class,

    // Index
    IndexCommandsI18nTest.class,
    SetIndexPropertyCommandTest.class,
    UnsetIndexPropertyCommandTest.class,

    // Mask
    MaskCommandsI18nTest.class,
    MaskRenameCommandTest.class,
    SetMaskPropertyCommandTest.class,
    UnsetMaskPropertyCommandTest.class,

    // Model
    AddPushdownFunctionCommandTest.class,
    AddSourceCommandTest.class,
    AddStoredProcedureCommandTest.class,
    AddTableCommandTest.class,
    AddUserDefinedFunctionCommandTest.class,
    AddViewCommandTest.class,
    AddVirtualProcedureCommandTest.class,
    DeletePushdownFunctionCommandTest.class,
    DeleteSourceCommandTest.class,
    DeleteStoredProcedureCommandTest.class,
    DeleteTableCommandTest.class,
    DeleteUserDefinedFunctionCommandTest.class,
    DeleteViewCommandTest.class,
    DeleteVirtualProcedureCommandTest.class,
//    ImportCommandTest.class,
//    org.komodo.relational.commands.model.ExportCommandTest.class,
    ModelCommandsI18nTest.class,
    SetModelPropertyCommandTest.class,
    ShowPushdownFunctionsCommandTest.class,
    ShowSourcesCommandTest.class,
    ShowStoredProceduresCommandTest.class,
    ShowTablesCommandTest.class,
    ShowUserDefinedFunctionsCommandTest.class,
    ShowViewsCommandTest.class,
    ShowVirtualProceduresCommandTest.class,
    UnsetModelPropertyCommandTest.class,

    // ModelSource
    ModelSourceCommandsI18nTest.class,
    SetModelSourcePropertyCommandTest.class,
    UnsetModelSourcePropertyCommandTest.class,

    // Parameter
    ParameterCommandsI18nTest.class,
    SetParameterPropertyCommandTest.class,
    UnsetParameterPropertyCommandTest.class,

    // Permission
    AddConditionCommandTest.class,
    AddMaskCommandTest.class,
    DeleteConditionCommandTest.class,
    DeleteMaskCommandTest.class,
    PermissionCommandsI18nTest.class,
    SetPermissionPropertyCommandTest.class,
    ShowConditionsCommandTest.class,
    ShowMasksCommandTest.class,
    UnsetPermissionPropertyCommandTest.class,

    // ProcedureResultSet
    ResultSetRenameCommandTest.class,

    // PushdownFunction
    org.komodo.relational.commands.pushdownfunction.AddParameterCommandTest.class,
    org.komodo.relational.commands.pushdownfunction.DeleteParameterCommandTest.class,
    org.komodo.relational.commands.pushdownfunction.SetResultSetCommandTest.class,
    org.komodo.relational.commands.pushdownfunction.RemoveResultSetCommandTest.class,
    PushdownFunctionCommandsI18nTest.class,
    PushdownFunctionRenameCommandTest.class,
    SetPushdownFunctionPropertyCommandTest.class,
    UnsetPushdownFunctionPropertyCommandTest.class,

    // ResultSetColumn
    ResultSetColumnCommandsI18nTest.class,
    SetResultSetColumnPropertyCommandTest.class,
    UnsetResultSetColumnPropertyCommandTest.class,

    // Schema
    SchemaCommandsI18nTest.class,
    SetSchemaPropertyCommandTest.class,
    UnsetSchemaPropertyCommandTest.class,
//    org.komodo.relational.commands.schema.ExportCommandTest.class,

    // Server
    ServerCommandsI18nTest.class,
    ServerConnectCommandTest.class,
    ServerDatasourceCommandTest.class,
    ServerDatasourcesCommandTest.class,
    ServerDatasourceTypeCommandTest.class,
    ServerDatasourceTypesCommandTest.class,
    ServerDeployDatasourceCommandTest.class,
    ServerDeployVdbCommandTest.class,
    ServerDisconnectCommandTest.class,
    ServerShowPropertiesCommandTest.class,
    ServerTranslatorCommandTest.class,
    ServerTranslatorsCommandTest.class,
    ServerUndeployDatasourceCommandTest.class,
    ServerUndeployVdbCommandTest.class,
    ServerVdbCommandTest.class,
    ServerVdbsCommandTest.class,
    ServerSetPropertyCommandTest.class,
    ServerUnsetPropertyCommandTest.class,

    // StoredProcedure
    org.komodo.relational.commands.storedprocedure.AddParameterCommandTest.class,
    org.komodo.relational.commands.storedprocedure.DeleteParameterCommandTest.class,
    org.komodo.relational.commands.storedprocedure.SetResultSetCommandTest.class,
    org.komodo.relational.commands.storedprocedure.RemoveResultSetCommandTest.class,
    SetStoredProcedurePropertyCommandTest.class,
    StoredProcedureCommandsI18nTest.class,
    StoredProcedureRenameCommandTest.class,
    UnsetStoredProcedurePropertyCommandTest.class,

    // Table
    AddAccessPatternCommandTest.class,
    AddColumnCommandTest.class,
    AddForeignKeyCommandTest.class,
    AddIndexCommandTest.class,
    AddUniqueConstraintCommandTest.class,
    AddPrimaryKeyCommandTest.class,
    DeleteAccessPatternCommandTest.class,
    DeleteColumnCommandTest.class,
    DeleteForeignKeyCommandTest.class,
    DeleteIndexCommandTest.class,
    DeleteUniqueConstraintCommandTest.class,
    DeletePrimaryKeyCommandTest.class,
    SetTablePropertyCommandTest.class,
    ShowAccessPatternsCommandTest.class,
    ShowColumnsCommandTest.class,
    ShowIndexesCommandTest.class,
    ShowUniqueConstraintsCommandTest.class,
    TableCommandsI18nTest.class,
    UnsetTablePropertyCommandTest.class,

    // TableConstraint
    AddConstraintColumnCommandTest.class,
    DeleteConstraintColumnCommandTest.class,
    TableConstraintCommandsI18nTest.class,

    // TabularResultSet
    org.komodo.relational.commands.tabularresultset.AddColumnCommandTest.class,
    org.komodo.relational.commands.tabularresultset.DeleteColumnCommandTest.class,
    TabularResultSetCommandsI18nTest.class,
    TabularResultSetSetPropertyCommandTest.class,
    TabularResultSetUnsetPropertyCommandTest.class,

    // Translator
    SetTranslatorPropertyCommandTest.class,
    TranslatorCommandsI18nTest.class,
    TranslatorRenameCommandTest.class,
    UnsetTranslatorPropertyCommandTest.class,

    // UserDefinedFunction
    org.komodo.relational.commands.userdefinedfunction.AddParameterCommandTest.class,
    org.komodo.relational.commands.userdefinedfunction.DeleteParameterCommandTest.class,
    SetUserDefinedFunctionPropertyCommandTest.class,
    UnsetUserDefinedFunctionPropertyCommandTest.class,
    UserDefinedFunctionCommandsI18nTest.class,

    // Vdb
    AddDataRoleCommandTest.class,
    AddEntryCommandTest.class,
    AddImportCommandTest.class,
    AddModelCommandTest.class,
    AddTranslatorCommandTest.class,
    DeleteDataRoleCommandTest.class,
    DeleteEntryCommandTest.class,
    DeleteImportCommandTest.class,
    DeleteModelCommandTest.class,
    DeleteTranslatorCommandTest.class,
    SetVdbPropertyCommandTest.class,
    UnsetVdbPropertyCommandTest.class,
    UploadModelCommandTest.class,
    VdbCommandsI18nTest.class,

    // VdbImport
    SetVdbImportPropertyCommandTest.class,
    UnsetVdbImportPropertyCommandTest.class,
    VdbImportCommandsI18nTest.class,
    VdbImportRenameCommandTest.class,

    // View
    org.komodo.relational.commands.view.AddColumnCommandTest.class,
    org.komodo.relational.commands.view.DeleteColumnCommandTest.class,
    SetViewPropertyCommandTest.class,
    UnsetViewPropertyCommandTest.class,
    ViewCommandsI18nTest.class,

    // VirtualProcedure
    org.komodo.relational.commands.virtualprocedure.AddParameterCommandTest.class,
    org.komodo.relational.commands.virtualprocedure.DeleteParameterCommandTest.class,
    SetVirtualProcedurePropertyCommandTest.class,
    UnsetVirtualProcedurePropertyCommandTest.class,
    VirtualProcedureCommandsI18nTest.class

    })
public class AllTests {
    // nothing to do
}
