package org.komodo.eclipse.workspace.ui.editor.panels.relational;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;
import org.komodo.eclipse.workspace.ui.Messages;
import org.komodo.eclipse.workspace.ui.common.util.WidgetFactory;
import org.komodo.eclipse.workspace.ui.editor.panels.IKObjectChangeListener;
import org.komodo.eclipse.workspace.ui.editor.panels.ObjectEditorPanel;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.StoredProcedure;
import org.komodo.spi.KException;
import org.komodo.utils.StringUtils;

public class ColumnEditorPanel extends ObjectEditorPanel {
	private Text datatypeText, nameInSourceText;
	private TabItem generalPropertiesTab;
	
	public ColumnEditorPanel(Composite parent, int style, int nodeType,	IKObjectChangeListener objectChangeListener) {
		super(parent, style, nodeType, objectChangeListener);
	}
	
	@SuppressWarnings("unused")
	public void addCustomPanel() {
		createGeneralPropertiesTab(getTabFolder());
	}
	
	private Column getColumn() {
		return ((Column)getTargetObject());
	}
	
	@Override
	protected void syncWithTarget() {
		super.syncWithTarget();
		setSynching(true);
		
		try {
			String value = getColumn().getNameInSource(null);
			if( value == null ) value = StringUtils.EMPTY_STRING;
			this.nameInSourceText.setText(value);
		} catch (KException e) {
			e.printStackTrace();
		}
		
		try {
			String value = getColumn().getDatatypeName(null);
			if( value == null ) value = StringUtils.EMPTY_STRING;
			this.datatypeText.setText(value);
		} catch (KException e) {
			e.printStackTrace();
		}
		
		setSynching(false);
	}

	private void createGeneralPropertiesTab(TabFolder folderParent) {
        // build the SELECT tab
		Composite thePanel = createPropertiesPanel(folderParent);

        this.generalPropertiesTab = new TabItem(folderParent, SWT.NONE);
        this.generalPropertiesTab.setControl(thePanel);
        this.generalPropertiesTab.setText(Messages.propertiesLabel);
//        this.generalPropertiesTab.setImage(RelationalUiUtil.getRelationalImage(TYPES.TABLE, ModelType.PHYSICAL, Status.OK_STATUS));
	}
	
	private Composite createPropertiesPanel(Composite parent) {
		Composite thePanel = WidgetFactory.createPanel(parent, SWT.NONE, 1, 3);
		GridLayoutFactory.fillDefaults().numColumns(2).margins(5, 5).applyTo(thePanel);
		GridDataFactory.fillDefaults().grab(true, true).applyTo(thePanel);

        // ========  NAME IN SOURCE ====
		{
            Label label = new Label(thePanel, SWT.NONE);
            label.setText("Name In Source");
            
            this.nameInSourceText =  new Text(thePanel, SWT.BORDER | SWT.SINGLE);
            this.nameInSourceText.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_BLUE));
            GridDataFactory.fillDefaults().grab(true, false).applyTo(this.nameInSourceText);
            this.nameInSourceText.addModifyListener(new ModifyListener() {
        		@Override
    			public void modifyText( final ModifyEvent event ) {
//        			if( isSynchronizing() )
//        			    return;

        			if( !nameInSourceText.getText().isEmpty()) {
        			    try {	
        			        String nis = nameInSourceText.getText();
        			        if( nis != null && nis.isEmpty() ) {
        			        	nis = null;
        			        }
        			        getColumn().setNameInSource(null, nis);
        			    } catch (KException ex) {
        			        MessageDialog.openError(
        			                                getShell(), 
        			                                "Problem setting name in source for procedure",  
        			                                ex.getMessage()); 
    	        				    return;
        			    }
        			}
        		}
            });
        }
		
		// == DATATYPE INFO
		{
			WidgetFactory.createLabel(thePanel, SWT.NONE, "Data Type :");
			datatypeText = new Text(thePanel, SWT.BORDER);
			datatypeText.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_BLUE));
			GridDataFactory.fillDefaults().grab(true, false).applyTo(datatypeText);
			datatypeText.setEditable(true);
			datatypeText.addModifyListener(new ModifyListener() {
	    		public void modifyText( final ModifyEvent event ) {
	    			if( getTargetObject() != null && !synching()) {
	    				// DO SOMETHING????((Column)getTargetObject()).setName(null, nameText.getText());
		        		handleInfoChanged();
	    			}
	    		}
	    	});
		}
		
        return thePanel;
	}
}
