package org.komodo.eclipse.workspace.ui.editor.panels.vdb;

import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Text;
import org.komodo.eclipse.workspace.ui.common.util.WidgetFactory;
import org.komodo.eclipse.workspace.ui.editor.panels.IKObjectChangeListener;
import org.komodo.eclipse.workspace.ui.editor.panels.ObjectEditorPanel;
import org.komodo.spi.KException;

public class TranslatorEditorPanel extends ObjectEditorPanel {
	Text nameText;
	
	public TranslatorEditorPanel(Composite parent, int style, int nodeType, IKObjectChangeListener objectChangeListener) {
		super(parent, style, nodeType, objectChangeListener);
	}
	
	@SuppressWarnings("unused")
	public void addCustomPanel() {
		
		TITLE : {
			WidgetFactory.createLabel(this, SWT.NONE, "Translator Name :");
			nameText = new Text(this, SWT.BORDER);
			nameText.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_BLUE));
			GridDataFactory.fillDefaults().grab(true, false).applyTo(nameText);
			nameText.setEditable(true);
			nameText.addModifyListener(new ModifyListener() {
	    		public void modifyText( final ModifyEvent event ) {
	    			if( getTargetObject() != null && !synching()) {
	    				// DO SOMETHING????((Vdb)getTargetObject()).setName(null, nameText.getText());
		        		handleInfoChanged();
	    			}
	    		}
	    	});
		}
	}
	
	@Override
	protected void syncWithTarget() {
		super.syncWithTarget();
		setSynching(true);
		if( isVisible() && getTargetObject() != null) {
			String oldText = nameText.getText();
			String newName = oldText;
			try {
				newName = getTargetObject().getName(null);
			} catch (KException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			setTextValue(nameText, newName);
		} else {
			nameText.setText("");
		}
		setSynching(false);
	}

}
