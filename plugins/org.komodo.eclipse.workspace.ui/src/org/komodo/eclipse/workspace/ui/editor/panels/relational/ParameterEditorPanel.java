package org.komodo.eclipse.workspace.ui.editor.panels.relational;

import org.eclipse.swt.widgets.Composite;
import org.komodo.eclipse.workspace.ui.editor.panels.IKObjectChangeListener;
import org.komodo.eclipse.workspace.ui.editor.panels.ObjectEditorPanel;

public class ParameterEditorPanel extends ObjectEditorPanel {
//	Text nameText;
	
	public ParameterEditorPanel(Composite parent, int style, int nodeType,	IKObjectChangeListener objectChangeListener) {
		super(parent, style, nodeType, objectChangeListener);
	}
	
	@SuppressWarnings("unused")
	public void addCustomPanel() {
		
//		TITLE : {
//			WidgetFactory.createLabel(this, SWT.NONE, "Table Name :");
//			nameText = new Text(this, SWT.BORDER);
//			nameText.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_BLUE));
//			GridDataFactory.fillDefaults().grab(true, false).applyTo(nameText);
//			nameText.setEditable(true);
//			nameText.addModifyListener(new ModifyListener() {
//	    		public void modifyText( final ModifyEvent event ) {
//	    			if( getTargetObject() != null && !synching()) {
//	    				// DO SOMETHING????((Column)getTargetObject()).setName(null, nameText.getText());
//		        		handleInfoChanged();
//	    			}
//	    		}
//	    	});
//		}
	}
	
	@Override
	protected void syncWithTarget() {
		super.syncWithTarget();
		setSynching(true);

		setSynching(false);
	}

}
