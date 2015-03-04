package org.komodo.eclipse.workspace;

public interface CoreConstants {
	/**
	 * Keys for images and image descriptors stored in the image registry.
	 * 
	 * @since 4.0
	 */
	interface Images {
		String ICONS = "icons/"; //$NON-NLS-1$
		String FULL = ICONS + "full/"; //$NON-NLS-1$

		String CTOOL16 = FULL + "ctool16/"; //$NON-NLS-1$
		String CVIEW16 = FULL + "cview16/"; //$NON-NLS-1$
		String DLCL16 = FULL + "dlcl16/"; //$NON-NLS-1$
		String ELCL16 = FULL + "elcl16/"; //$NON-NLS-1$
		String OBJ16 = FULL + "obj16/"; //$NON-NLS-1$
		String OVR16 = FULL + "ovr16/"; //$NON-NLS-1$
		String WIZBAN = FULL + "wizban/"; //$NON-NLS-1$

		String KOMODO_ICON = OBJ16 + "dragon_blue_16x16.png"; //$NON-NLS-1$
		String VDB_ICON = OBJ16 + "VDB.gif"; //$NON-NLS-1$
		String MODEL_ICON = OBJ16 + "Model.gif"; //$NON-NLS-1$
		String VIEW_MODEL_ICON = OBJ16 + "ViewModel.gif"; //$NON-NLS-1$
		String COLUMN_ICON = OBJ16 + "column.png"; //$NON-NLS-1$
		String COLUMN_ERROR_ICON = OBJ16 + "column-error.png"; //$NON-NLS-1$
		String COLUMN_WARNING_ICON = OBJ16 + "column-warning.png"; //$NON-NLS-1$
		String PARAMETER_ICON = OBJ16 + "parameter.png"; //$NON-NLS-1$
		String PARAMETER_IN_ICON = OBJ16 + "parameter-in.png"; //$NON-NLS-1$
		String PARAMETER_INOUT_ICON = OBJ16 + "parameter-inout.png"; //$NON-NLS-1$
		String PARAMETER_OUT_ICON = OBJ16 + "parameter-out.png"; //$NON-NLS-1$
		String PARAMETER_RETURN_ICON = OBJ16 + "parameter-return.png"; //$NON-NLS-1$
		String PARAMETER_UNKNOWN_ICON = OBJ16 + "parameter-unknown.png"; //$NON-NLS-1$
		String PARAMETER_ERROR_ICON = OBJ16 + "parameter-error.png"; //$NON-NLS-1$
		String PARAMETER_WARNING_ICON = OBJ16 + "parameter-warning.png"; //$NON-NLS-1$
		String FK_ICON = OBJ16 + "foreign-key.png"; //$NON-NLS-1$
		String FK_ERROR_ICON = OBJ16 + "foreign-key-error.png"; //$NON-NLS-1$
		String FK_WARNING_ICON = OBJ16 + "foreign-key-warning.png"; //$NON-NLS-1$
		String PK_ICON = OBJ16 + "primary-key.png"; //$NON-NLS-1$
		String PK_ERROR_ICON = OBJ16 + "primary-key-error.png"; //$NON-NLS-1$
		String PK_WARNING_ICON = OBJ16 + "primary-key-warning.png"; //$NON-NLS-1$
		String UC_ICON = OBJ16 + "unique-constraint.png"; //$NON-NLS-1$
		String UC_ERROR_ICON = OBJ16 + "unique-constraint-error.png"; //$NON-NLS-1$
		String UC_WARNING_ICON = OBJ16 + "unique-constraint-warning.png"; //$NON-NLS-1$
		String AP_ICON = OBJ16 + "access-pattern.png"; //$NON-NLS-1$
		String AP_ERROR_ICON = OBJ16 + "access-pattern-error.png"; //$NON-NLS-1$
		String AP_WARNING_ICON = OBJ16 + "access-pattern-warning.png"; //$NON-NLS-1$
		String TABLE_ICON = OBJ16 + "relational-table.png"; //$NON-NLS-1$
		String TABLE_ERROR_ICON = OBJ16 + "relational-table-error.png"; //$NON-NLS-1$
		String TABLE_WARNING_ICON = OBJ16 + "relational-table-warning.png"; //$NON-NLS-1$
		String NEW_TABLE_ICON = OBJ16 + "new-relational-table.png"; //$NON-NLS-1$
		String NEW_PROCEDURE_ICON = OBJ16 + "new-relational-procedure.png"; //$NON-NLS-1$
		String VIRTUAL_TABLE_ICON = OBJ16 + "virtual-relational-table.png"; //$NON-NLS-1$
		String NEW_VIRTUAL_TABLE_ICON = OBJ16 + "new-view-table.png"; //$NON-NLS-1$
		String VIRTUAL_TABLE_ERROR_ICON = OBJ16
				+ "virtual-relational-table-error.png"; //$NON-NLS-1$
		String VIRTUAL_TABLE_WARNING_ICON = OBJ16
				+ "virtual-relational-table-warning.png"; //$NON-NLS-1$
		String PROCEDURE_ICON = OBJ16 + "relational-procedure.png"; //$NON-NLS-1$
		String PROCEDURE_ERROR_ICON = OBJ16 + "relational-procedure-error.png"; //$NON-NLS-1$
		String PROCEDURE_WARNING_ICON = OBJ16
				+ "relational-procedure-warning.png"; //$NON-NLS-1$
		String VIRTUAL_PROCEDURE_ICON = OBJ16
				+ "virtual-relational-procedure.png"; //$NON-NLS-1$
		String NEW_VIRTUAL_PROCEDURE_ICON = OBJ16 + "new-view-procedure.png"; //$NON-NLS-1$
		String VIRTUAL_PROCEDURE_ERROR_ICON = OBJ16
				+ "virtual-relational-procedure-error.png"; //$NON-NLS-1$
		String VIRTUAL_PROCEDURE_WARNING_ICON = OBJ16
				+ "virtual-relational-procedure-warning.png"; //$NON-NLS-1$
		String RESULT_SET_ERROR_ICON = OBJ16 + "result-set-error.png"; //$NON-NLS-1$
		String RESULT_SET_WARNING_ICON = OBJ16 + "result-set-warning.png"; //$NON-NLS-1$
		String VIRTUAL_RESULT_SET_ICON = OBJ16 + "virtual-result-set.png"; //$NON-NLS-1$
		String VIRTUAL_RESULT_SET_ERROR_ICON = OBJ16
				+ "virtual-result-set-error.png"; //$NON-NLS-1$
		String VIRTUAL_RESULT_SET_WARNING_ICON = OBJ16
				+ "virtual-result-set-warning.png"; //$NON-NLS-1$
		String NEW_INDEX_ICON = OBJ16 + "new-relational-index.png"; //$NON-NLS-1$
		String INDEX_ICON = OBJ16 + "index.png"; //$NON-NLS-1$
		String INDEX_ERROR_ICON = OBJ16 + "index-error.png"; //$NON-NLS-1$
		String INDEX_WARNING_ICON = OBJ16 + "index-warning.png"; //$NON-NLS-1$
		String EDIT_DESCRIPTION_ICON = OBJ16 + "edit-description.png"; //$NON-NLS-1$
		String EDIT_DESCRIPTION_ERROR_ICON = OBJ16
				+ "edit-description-error.png"; //$NON-NLS-1$
		String EDIT_DESCRIPTION_WARNING_ICON = OBJ16
				+ "edit-description-warning.png"; //$NON-NLS-1$
		String NATIVE_SQL_ICON = OBJ16 + "native-sql.png"; //$NON-NLS-1$
		String NATIVE_SQL_ERROR_ICON = OBJ16 + "native-sql-error.png"; //$NON-NLS-1$
		String NATIVE_SQL_WARNING_ICON = OBJ16 + "native-sql-warning.png"; //$NON-NLS-1$

		String NEW_VDB_ICON = OBJ16 + "new-vdb.png"; //$NON-NLS-1$
		String IMPORT_VDB_ICON = OBJ16 + "import-vdb-metadata.png"; //$NON-NLS-1$
		String IMPORT_MODEL_ICON = OBJ16 + "import-teiid-metadata.png"; //$NON-NLS-1$

		String INFO_ICON = OBJ16 + "info.gif"; //$NON-NLS-1$
		String WARNING_ICON = OBJ16 + "warning.gif"; //$NON-NLS-1$
		String ERROR_ICON = OBJ16 + "error.gif"; //$NON-NLS-1$

		String CHECKED_CHECKBOX = OBJ16 + "checked_box.png"; //$NON-NLS-1$
		String UNCHECKED_CHECKBOX = OBJ16 + "unchecked_box.png"; //$NON-NLS-1$

		String ASCENDING_ICON = OBJ16 + "ascending.gif"; //$NON-NLS-1$
		String DESCENDING_ICON = OBJ16 + "descending.gif"; //$NON-NLS-1$
		String DOWN = OBJ16 + "down.gif"; //$NON-NLS-1$
		String LEFT = OBJ16 + "left.gif"; //$NON-NLS-1$
		String RIGHT = OBJ16 + "right.gif"; //$NON-NLS-1$
		String UP = OBJ16 + "up.gif"; //$NON-NLS-1$

		String DELETE = OBJ16 + "delete.gif"; //$NON-NLS-1$
		String REFRESH = OBJ16 + "refresh.gif"; //$NON-NLS-1$

		String TRANSLATOR_ICON = OBJ16 + "connector.gif"; //$NON-NLS-1$
		String CONNECTION_ICON = OBJ16 + "connector.gif"; //$NON-NLS-1$
	}

	interface RelationalType {

		int UNKNOWN = -1;
		int VDB = 0;
		int MODEL = 1;
		int TABLE = 2;
		int VIEW = 3;
		int STORED_PROCEDURE = 4;
		int VIRTUAL_PROCEDURE = 5;
		int INDEX = 6;
		int COLUMN = 7;
		int PARAMETER = 8;
		int RESULT_SET = 9;
		int FK = 10;
		int UC = 11;
		int PK = 12;
		int AP = 13;
		int MODEL_SOURCE = 14;
		int ENTRY = 15;
		int DATA_ROLE = 16;
		int VDB_IMPORT = 17;
		int TRANSLATOR = 18;
		int PERMISSION = 19;
		int LIBRARY = 20;
		int MASK = 21;
		int CONDITION = 22;
		int USER_DEFINED_FUNCTION = 23;
		int PUSHDOWN_FUNCTION = 24;
	}
}