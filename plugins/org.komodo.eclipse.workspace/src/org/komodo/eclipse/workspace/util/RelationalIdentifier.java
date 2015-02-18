package org.komodo.eclipse.workspace.util;

import org.eclipse.swt.graphics.Image;
import org.komodo.core.KomodoLexicon;
import org.komodo.core.KomodoLexicon.VdbEntry;
import org.komodo.core.KomodoLexicon.VdbModelSource;
import org.komodo.eclipse.workspace.CoreConstants;
import org.komodo.eclipse.workspace.CorePlugin;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.ProcedureResultSet;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.View;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateProcedure;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateTable;
import org.modeshape.sequencer.teiid.lexicon.CoreLexicon;
import org.modeshape.sequencer.teiid.lexicon.CoreLexicon.ModelType;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

public class RelationalIdentifier implements CoreConstants.RelationalType, CoreConstants.Images {

	public static boolean isType(KomodoObject ko, int targetType) {
		int type = getType(ko);
		
		return type == targetType;
	}
	
	public static int getType(KomodoObject ko) {
		int instanceOfType = 	getTypeByInstanceOf(ko);
		if( instanceOfType != UNKNOWN ) return instanceOfType;
		
		try {

			// check primary type
			Descriptor pType = ko.getPrimaryType(null);
			if( pType != null ) {
				if( pType.getName().equalsIgnoreCase(VdbLexicon.Vdb.VIRTUAL_DATABASE)) {
					return VDB;
				}
				if( pType.getName().equalsIgnoreCase(KomodoLexicon.VdbModel.NODE_TYPE)) {
					return MODEL;
				}
				if( pType.getName().equalsIgnoreCase(VdbLexicon.Translator.TRANSLATOR)) {
					return TRANSLATOR;
				}
				if( pType.getName().equalsIgnoreCase(VdbLexicon.Source.SOURCE)) {
					return MODEL_SOURCE;
				}
				if( pType.getName().equalsIgnoreCase(VdbLexicon.ImportVdb.IMPORT_VDB)) {
					return ENTRY;
				}
				if( pType.getName().equalsIgnoreCase(VdbLexicon.ImportVdb.IMPORT_VDB)) {
					return VDB_IMPORT;
				}
				if( pType.getName().equalsIgnoreCase(VdbLexicon.DataRole.DATA_ROLE)) {
					return DATA_ROLE;
				}
				if( pType.getName().equalsIgnoreCase(VdbLexicon.DataRole.Permission.PERMISSION)) {
					return PERMISSION;
				}
			}
			
			Descriptor[] descriptors = ko.getDescriptors(null);
			if( descriptors.length > 0 ) {
				for( Descriptor des : descriptors ) {
					if( des.getName().equalsIgnoreCase(KomodoLexicon.Vdb.NODE_TYPE)) return VDB;
					else if( des.getName().equalsIgnoreCase(KomodoLexicon.VdbModel.NODE_TYPE)) return MODEL;
					else if( des.getName().equalsIgnoreCase(CreateTable.TABLE_STATEMENT)) return TABLE;
					else if( des.getName().equalsIgnoreCase(CreateTable.VIEW_STATEMENT)) return VIEW;
					else if( des.getName().equalsIgnoreCase(CreateProcedure.PROCEDURE_STATEMENT)) {
						try {
							Property type = (ko.getParent(null)).getProperty(null, CoreLexicon.JcrId.MODEL_TYPE);
							if( type != null && type.getStringValue(null).equalsIgnoreCase(ModelType.VIRTUAL)) {
								return VIRTUAL_PROCEDURE;
							}
						} catch (KException e) {
							e.printStackTrace();
						}
						return PROCEDURE;
					}
					else if( des.getName().equalsIgnoreCase(CreateProcedure.RESULT_SET)) return RESULT_SET;
					else if( des.getName().equalsIgnoreCase(CreateTable.TABLE_ELEMENT)) return COLUMN;
					else if( des.getName().equalsIgnoreCase(CreateProcedure.PARAMETER)) return PARAMETER;
					else if( des.getName().equalsIgnoreCase(VdbLexicon.Translator.TRANSLATOR)) return TRANSLATOR;
					else if( des.getName().equalsIgnoreCase(VdbLexicon.Source.SOURCE)) return MODEL_SOURCE;
					else if( des.getName().equalsIgnoreCase(VdbLexicon.ImportVdb.IMPORT_VDB)) return ENTRY;
					else if( des.getName().equalsIgnoreCase(VdbLexicon.ImportVdb.IMPORT_VDB)) return VDB_IMPORT;
				}
			}
		} catch (KException e) {
			e.printStackTrace();
		}
		
		
		return UNKNOWN;

	}
	
	public static String getTypeDisplayName(int type) {
		switch(type) {
			case VDB: return "VDB";
			case MODEL: return "Model";
			case TABLE: return "Table";
			case VIEW: return "View";
			case PROCEDURE: return "Procedure";
			case VIRTUAL_PROCEDURE: return "Virtual Procedure";
			case COLUMN: return "Column";
			case PARAMETER: return "Parameter";
			case RESULT_SET: return "Result Set";
			case TRANSLATOR: return "Translator";
			case MODEL_SOURCE: return "Model Source";
			case ENTRY: return "File Entry";
			case VDB_IMPORT: return "Vdb Import";
			case UNKNOWN: return "Unknown";
		}
		return "Unknown";
	}
	
	public static int getTypeByInstanceOf(KomodoObject ko) {
		if( ko instanceof Vdb ) return VDB;
		else if( ko instanceof Model) return MODEL;
		else if( ko instanceof View) return VIEW;
		else if( ko instanceof Table) return TABLE;
		else if( ko instanceof Procedure) {
			try {
				Property type = (ko.getParent(null)).getProperty(null, CoreLexicon.JcrId.MODEL_TYPE);
				if( type != null && type.getStringValue(null).equalsIgnoreCase(ModelType.VIRTUAL)) {
					return VIRTUAL_PROCEDURE;
				}
			} catch (KException e) {
				e.printStackTrace();
			}
			
			return PROCEDURE;
		}
		else if( ko instanceof ProcedureResultSet) return RESULT_SET;
		else if( ko instanceof Column) return COLUMN;
		else if( ko instanceof Parameter) return PARAMETER;
		else if( ko instanceof Translator) return TRANSLATOR;
		else if( ko instanceof VdbImport) return VDB_IMPORT;
		else if( ko instanceof VdbModelSource) return MODEL_SOURCE;
		else if( ko instanceof VdbEntry) return ENTRY;
		
		return UNKNOWN;
	}
	
	public static Image getImage(int type) {
		
		switch(type) {
			case VDB: return CorePlugin.getDefault().getImage(VDB_ICON);
			case MODEL: return CorePlugin.getDefault().getImage(MODEL_ICON);
			case TABLE: return CorePlugin.getDefault().getImage(TABLE_ICON);
			case VIEW: return CorePlugin.getDefault().getImage(VIRTUAL_TABLE_ICON);
			case PROCEDURE: return CorePlugin.getDefault().getImage(PROCEDURE_ICON);
			case VIRTUAL_PROCEDURE: return CorePlugin.getDefault().getImage(VIRTUAL_PROCEDURE_ICON);
			case COLUMN: return CorePlugin.getDefault().getImage(COLUMN_ICON);
			case PARAMETER: return CorePlugin.getDefault().getImage(PARAMETER_ICON);
			case RESULT_SET: return CorePlugin.getDefault().getImage(TABLE_ICON);
			case TRANSLATOR: return CorePlugin.getDefault().getImage(TRANSLATOR_ICON);
			case MODEL_SOURCE: return CorePlugin.getDefault().getImage(CONNECTION_ICON);
//			case ENTRY: return CorePlugin.getDefault().getImage(VDB_ICON);
			case VDB_IMPORT: return CorePlugin.getDefault().getImage(VDB_ICON);
		}
		return null;
	}
	
	public static boolean isVirtual(KomodoObject ko) {
		switch(RelationalIdentifier.getType(ko)) {
			case MODEL: {
				try {
					Property type = (ko).getProperty(null, CoreLexicon.JcrId.MODEL_TYPE);
					if( type != null && type.getStringValue(null).equalsIgnoreCase(ModelType.VIRTUAL)) {
						return true;
					}
				} catch (KException e) {
					e.printStackTrace();
				}
				return false;
			}
			case TABLE:
			case PROCEDURE:
				try {
					return isVirtual(ko.getParent(null));
				} catch (KException e) {
					e.printStackTrace();
				}
				return false;
				
			case VIEW: return true;

		}
		return false;
	}

}
