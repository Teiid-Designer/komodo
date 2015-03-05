package org.komodo.eclipse.workspace.ui.explorer;

import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.komodo.eclipse.workspace.CoreConstants;
import org.komodo.eclipse.workspace.CorePlugin;
import org.komodo.eclipse.workspace.util.RelationalIdentifier;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.utils.StringUtils;
import org.modeshape.sequencer.teiid.lexicon.CoreLexicon;
import org.modeshape.sequencer.teiid.lexicon.CoreLexicon.ModelType;

public class WorkspaceLabelProvider extends ColumnLabelProvider implements CoreConstants.RelationalType, CoreConstants.Images {

//	public static final int TABLE = 1;
//	public static final int COLUMN = 2;
//	public static final int PK = 3;
	
	
	@Override
	public Image getImage(Object element) {
		if( element instanceof KomodoObject ) {
			switch(RelationalIdentifier.getType((KomodoObject)element)) {
				case VDB: return getImageInternal(VDB_ICON);
				case MODEL: {
					try {
						Property type = ((KomodoObject)element).getProperty(null, CoreLexicon.JcrId.MODEL_TYPE);
						if( type != null && type.getStringValue(null).equalsIgnoreCase(ModelType.VIRTUAL)) {
							return getImageInternal(VIEW_MODEL_ICON);
						}
					} catch (KException e) {
						e.printStackTrace();
					}
					return getImageInternal(MODEL_ICON);
				}
				case TABLE: {
					if( isVirtual((KomodoObject)element)) {
						return getImageInternal(VIRTUAL_TABLE_ICON);
					}
					return getImageInternal(TABLE_ICON);
				}
				case VIEW: return getImageInternal(VIRTUAL_TABLE_ICON);
				case STORED_PROCEDURE:  {
					return getImageInternal(PROCEDURE_ICON);
				}
				case VIRTUAL_PROCEDURE: return getImageInternal(VIRTUAL_PROCEDURE_ICON);
				case COLUMN: return getImageInternal(COLUMN_ICON);
				case PARAMETER: return getImageInternal(PARAMETER_ICON);
				case RESULT_SET: return getImageInternal(TABLE_ICON);
				case TRANSLATOR: return getImageInternal(CONNECTION_ICON);
				case VDB_IMPORT: return getImageInternal(VDB_ICON);
				case DATA_ROLE: return getImageInternal(CONNECTION_ICON);
				case PERMISSION: return getImageInternal(CONNECTION_ICON);
				case LIBRARY:
				case MODEL_SOURCE:
				case MASK:
				case CONDITION:
				case UNKNOWN: return getImageInternal(KOMODO_ICON);
			}
		}
		return super.getImage(element);
	}
		
	private Image getImageInternal(String id) {
		return  CorePlugin.getDefault().getImage(id);
	}

	@Override
	public String getText(Object element) {
		if( element instanceof ObjectImpl ) {
			return getNameAndType((ObjectImpl)element);
		}
		return super.getText(element);
	}

	
	private boolean isVirtual(KomodoObject ko) {
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
			case STORED_PROCEDURE:
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
	
	
	private String getNameAndType(KomodoObject ko) {
		StringBuilder sb = new StringBuilder();

		try {
//			RepositoryImpl.UnitOfWork transaction = repo.createTransaction("Get Name And Type", false, null);
			String type = getType(ko);
			if( type != null ) {
				sb.append(ko.getName(null)).append(" : <").append(type).append(">");
			} else {
				sb.append(ko.getName(null)).append(" : <").append(ko.getPrimaryType(null)).append(">");
			}
		} catch (KException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return sb.toString();
	}
	
	private String getName(KomodoObject ko) {

		try {
			return ko.getName(null);
		} catch (KException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return StringUtils.EMPTY_STRING;
	}
	
	private String getType(KomodoObject ko) {
		switch( RelationalIdentifier.getType(ko)) {
			case VDB: return "vdb";
			case MODEL: return "model";
			case TABLE: return "table";
			case VIEW: return "view";
			case STORED_PROCEDURE: return "stored-procedure";
			case VIRTUAL_PROCEDURE: return "virtual-procedure";
			case USER_DEFINED_FUNCTION: return "user-defined-function";
			case PUSHDOWN_FUNCTION: return "pushdown-funciton";
			case COLUMN: return "column";
			case PARAMETER: return "parameter";
			case RESULT_SET: return "result-set";
			case TRANSLATOR: return "translator";
			case DATA_ROLE: return "data role";
			case PERMISSION: return "permission";
			case LIBRARY: return "library";
			case MODEL_SOURCE: return "source";
			case MASK: return "mask";
			case CONDITION: return "condition";
			case UNKNOWN: return "unknown";
		}
		
		return "unknown";
	}

	@Override
	public String getToolTipText(Object element) {
		if( element instanceof KomodoObject ) {
			KomodoObject ko = (KomodoObject)element;
			StringBuilder tooltip = new StringBuilder("Name: ").append(getName(ko));
			tooltip.append(StringUtils.NEW_LINE).append("Type: " ).append(getType(ko));
			tooltip.append(StringUtils.NEW_LINE).append("Path : ").append(ko.getAbsolutePath());
			return tooltip.toString();
		}
		
		return super.getToolTipText(element);
	}
	
	

}
