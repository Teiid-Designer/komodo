package org.komodo.eclipse.workspace.ui.explorer;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.komodo.eclipse.workspace.CoreConstants;
import org.komodo.eclipse.workspace.CorePlugin;
import org.komodo.eclipse.workspace.util.RelationalIdentifier;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.modeshape.sequencer.teiid.lexicon.CoreLexicon;
import org.modeshape.sequencer.teiid.lexicon.CoreLexicon.ModelType;

public class WorkspaceLabelProvider extends LabelProvider implements CoreConstants.RelationalType, CoreConstants.Images {

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
				case PROCEDURE:  {
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
	
	private String getType(KomodoObject ko) throws KException {
		switch(RelationalIdentifier.getType(ko)) {
			case VDB: return "vdb";
			case MODEL: return "model";
			case TABLE: return "table";
			case VIEW: return "view";
			case PROCEDURE: return "procedure";
			case VIRTUAL_PROCEDURE: return "procedure";
			case COLUMN: return "column";
			case PARAMETER: return "parameter";
			case RESULT_SET: return "result-set";
			case TRANSLATOR: return "translator";
			case DATA_ROLE: return "data role";
			case PERMISSION: return "permission";
			case UNKNOWN: return "unknown";
		}
//		if( RelationalIdentifier.getType(ko) == TABLE) retyrb
//		Descriptor[] descriptors = ko.getDescriptors(null);
//		for( Descriptor des : descriptors ) {
//			if( des.getName().equalsIgnoreCase(CreateTable.TABLE_STATEMENT)) return TeiidDdlLexicon.TABLE;
//			if( des.getName().equalsIgnoreCase(CreateTable.TABLE_ELEMENT)) return "column";
//		}
//		
		return "<unknown>";
	}

}
