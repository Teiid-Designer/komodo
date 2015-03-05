package org.komodo.eclipse.workspace.ui.editor.panels;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.komodo.spi.repository.KomodoObject;

public class KObjectEditorPanel  extends Composite {
	private KomodoObject docNode;
	private String type;

	private Text nodeTypeText;
	
//	private IDocChangedListener listener;
	
	private boolean synching = false;
	
//	public DocbookEditorPanel(Composite parent, int style, int nodeType, IDocChangedListener docChangedListener) {
//		super(parent, style);
//		this.type = nodeType;
//		this.listener = docChangedListener;
//		
//        GridLayout twoColumnLayout = new GridLayout(2, false);
//        setLayout(twoColumnLayout);
//        setLayoutData(new GridData(GridData.FILL_BOTH));
//        
//        addCommonStuff();
//        
//        addCustomPanel();
//	}
	
	public KObjectEditorPanel(Composite parent, int style, String nodeType) {
		super(parent, style);
		this.type = nodeType;
//		this.listener = docChangedListener;
		
        GridLayout twoColumnLayout = new GridLayout(2, false);
        setLayout(twoColumnLayout);
        setLayoutData(new GridData(GridData.FILL_BOTH));
        
        addCommonStuff();
        
        addCustomPanel();
	}

	public String getNodeType() {
		return this.type;
	}
	
	public KomodoObject getDocNode() {
		return this.docNode;
	}
	
	public void setDocNode(KomodoObject docNode) {
		this.docNode = docNode;
		if( docNode != null ) {
			syncWithDocNode();
		}
	}
	
	@SuppressWarnings("unused")
	private void addCommonStuff() {
		// Add label for Node Type
		TYPE : {
			Label label = new Label(this, SWT.NONE);
			label.setText("Type :");
			nodeTypeText = new Text(this, SWT.BORDER);
			nodeTypeText.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_WIDGET_LIGHT_SHADOW));
			nodeTypeText.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_BLUE));
			nodeTypeText.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
			nodeTypeText.setEditable(false);
		}

	}
	
	public void addCustomPanel() {
		// NOOP  Override to add other stuff
	}
	
	public void handleInfoChanged() {
		if( !synching) {
//			listener.documentChanged(new DocChangedEvent(getDocNode()));
		}
	}

	@Override
	public void setVisible(boolean visible) {
		super.setVisible(visible);
	}
	
	protected boolean synching() {
		return this.synching;
	}
	
	protected void setSynching(boolean synching) {
		this.synching = synching;
	}
	
	protected void syncWithDocNode() {
		this.synching = true;
//		if( getDocNode() != null ) {
//			if( this.allowsId ) {
//				setTextValue(idText, getDocNode().getId());
//			}
//			setTextValue(nodeTypeText, getDocNode().getElementName());
//		} else {
//			if( this.allowsId ) {
//				idText.setText(StringUtilities.EMPTY_STRING);
//			}
//			nodeTypeText.setText(StringUtilities.EMPTY_STRING);
//		}
		this.synching = false;
	}
	
	protected void setTextValue(Text textWidget, String value) {
		if( value == null ) {
			textWidget.setText("");
		} else {
			textWidget.setText(value);
		}
	}
	
}