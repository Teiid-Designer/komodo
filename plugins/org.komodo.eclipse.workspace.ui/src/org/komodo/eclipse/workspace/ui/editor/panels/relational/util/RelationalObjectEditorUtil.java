package org.komodo.eclipse.workspace.ui.editor.panels.relational.util;

import org.komodo.eclipse.workspace.CoreConstants;
import org.komodo.eclipse.workspace.util.RelationalIdentifier;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.View;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;

public class RelationalObjectEditorUtil implements CoreConstants.RelationalType {


	public static boolean setDescription(final UnitOfWork txn, KomodoObject ko, String description) {
		int type = RelationalIdentifier.getType(ko);
		
		try {
			switch(type) {
				case VDB: ((Vdb)ko).setDescription(txn, description); return true;
				case MODEL: ((Model)ko).setDescription(txn, description); return true;
				case TABLE:((Table)ko).setDescription(txn, description); return true;
				case VIEW: ((View)ko).setDescription(txn, description); return true;
				case STORED_PROCEDURE: ((Procedure)ko).setDescription(txn, description); return true;
				case VIRTUAL_PROCEDURE: ((Procedure)ko).setDescription(txn, description); return true;
				case COLUMN: ((Column)ko).setDescription(txn, description); return true;
// TODO: MISSING DESCRIPTION ON PARAMETER
//			case PARAMETER: ((Parameter)ko).setDescription(txn, description); break;
//			case RESULT_SET: ((Vdb)ko).setDescription(txn, description); break;
//			case TRANSLATOR: ((Vdb)ko).setDescription(txn, description); break;
//			case MODEL_SOURCE: ((Vdb)ko).setDescription(txn, description); break;
				case ENTRY: ((Vdb)ko).setDescription(txn, description); return true;
//			case VDB_IMPORT: ((Vdb)ko).setDescription(txn, description); break;
				default: break;
			}
		} catch (KException e) {
			e.printStackTrace();
		}
		
		return false;
	}
	
	public static String getDescription(final UnitOfWork txn, KomodoObject ko) {
		int type = RelationalIdentifier.getType(ko);
		
		try {
			switch(type) {
				case VDB: ((Vdb)ko).getDescription(txn); break;
				case MODEL: ((Model)ko).getDescription(txn); break;
				case TABLE:((Table)ko).getDescription(txn); break;
				case VIEW: ((View)ko).getDescription(txn); break;
				case STORED_PROCEDURE: ((Procedure)ko).getDescription(txn); break;
				case VIRTUAL_PROCEDURE: ((Procedure)ko).getDescription(txn); break;
				case COLUMN: ((Column)ko).getDescription(txn); break;
// TODO: MISSING DESCRIPTION ON PARAMETER
//			case PARAMETER: ((Parameter)ko).setDescription(txn, description); break;
//			case RESULT_SET: ((Vdb)ko).setDescription(txn, description); break;
//			case TRANSLATOR: ((Vdb)ko).setDescription(txn, description); break;
//			case MODEL_SOURCE: ((Vdb)ko).setDescription(txn, description); break;
				case ENTRY: ((Vdb)ko).getDescription(txn); break;
//			case VDB_IMPORT: ((Vdb)ko).setDescription(txn, description); break;
				default: break;
			}
		} catch (KException e) {
			e.printStackTrace();
		}
		
		return StringUtils.EMPTY_STRING;
	}
}
