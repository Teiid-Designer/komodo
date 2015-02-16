package org.komodo.eclipse.workspace.ui.editor.panels.vdb;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;
import org.komodo.eclipse.workspace.ui.Messages;
import org.komodo.eclipse.workspace.ui.common.util.StyledTextEditor;
import org.komodo.eclipse.workspace.ui.common.util.UILabelUtil;
import org.komodo.eclipse.workspace.ui.common.util.UiLabelConstants;
import org.komodo.eclipse.workspace.ui.common.util.WidgetFactory;
import org.komodo.eclipse.workspace.ui.common.widget.Label;
import org.komodo.eclipse.workspace.ui.editor.panels.IKObjectChangeListener;
import org.komodo.eclipse.workspace.ui.editor.panels.ObjectEditorPanel;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;

public class VdbEditorPanel extends ObjectEditorPanel {
//	Text nameText;
	
	private TabItem generalPropertiesTab;
    private StyledTextEditor descriptionTextEditor;
    private Label validationDateTimeLabel;
    private Label validationVersionLabel;
	
	public VdbEditorPanel(Composite parent, int style, int nodeType, IKObjectChangeListener objectChangeListener) {
		super(parent, style, nodeType, objectChangeListener, true);
	}
	
	@SuppressWarnings("unused")
	public void addCustomPanel() {
		TabFolder tabFolder = getTabFolder();
        
		Composite thePanel = createPropertiesPanel(tabFolder);

        this.generalPropertiesTab = new TabItem(tabFolder, SWT.NONE);
        this.generalPropertiesTab.setControl(thePanel);
        this.generalPropertiesTab.setText(Messages.propertiesLabel);
	}
	
	private Composite createPropertiesPanel(Composite parent) {
		Composite thePanel = WidgetFactory.createPanel(parent, SWT.NONE, 2, 3);
		GridLayoutFactory.fillDefaults().numColumns(2).margins(5, 5).applyTo(thePanel);
		GridDataFactory.fillDefaults().grab(true, true).applyTo(thePanel);
		
		// VDB Properties
		// TODO:  
		/*
		  Version ##
		  Date/Time stamp
		  Validation version
		  Description
		  
		 */
		
		{ // Validation Info
        	Color blueColor = Display.getCurrent().getSystemColor(SWT.COLOR_DARK_BLUE);

        	
            Label vdbVersionLabel = WidgetFactory.createLabel(thePanel, "Version"); //$NON-NLS-1$
        	GridDataFactory.fillDefaults().align(SWT.LEFT, SWT.CENTER).applyTo(vdbVersionLabel);
        	
        	final Text vdbVersionText = WidgetFactory.createTextField(thePanel);
        	GridDataFactory.fillDefaults().align(SWT.LEFT, SWT.CENTER).applyTo(vdbVersionText);
        	((GridData)vdbVersionText.getLayoutData()).widthHint = 30;
        	vdbVersionText.addModifyListener(new ModifyListener() {
    			
    			@Override
    			public void modifyText(ModifyEvent e) {
    				// TODO:
//    				try {
//                        int versionValue = Integer.parseInt(vdbVersionText.getText());
//                        if (versionValue > -1) {
//                            getVdb().setVersion(versionValue);
//    					}
//    				} catch (NumberFormatException ex) {
//    					MessageDialog.openWarning(Display.getCurrent().getActiveShell(),
//                                INVALID_INTEGER_INPUT_TITLE,
//                                INVALID_INTEGER_INPUT_MESSAGE);
//    					vdbVersionText.setText(Integer.toString(getVdb().getVersion()));
//    				}
    				
    			}
    		});
        	// TODO: 
        	vdbVersionText.setText(Integer.toString(1)); //getVdb().getVersion()));
        	
        	Label label1 = WidgetFactory.createLabel(thePanel, "Last Validated"); // + i18n("lastValidated") + " : "); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        	GridDataFactory.fillDefaults().align(SWT.LEFT, SWT.CENTER).applyTo(label1);
        	
        	
        	String dateTimeString = "<undefined>";
//        	if( getVdb().getValidationDateTime() != null ) {
//        		dateTimeString = getVdb().getValidationDateTime().toString();
//        	}
        	this.validationDateTimeLabel = WidgetFactory.createLabel(thePanel, dateTimeString);
        	validationDateTimeLabel.setForeground(blueColor);
        	validationDateTimeLabel.setText(dateTimeString);
        	GridDataFactory.fillDefaults().align(SWT.LEFT, SWT.CENTER).applyTo(validationDateTimeLabel);
        	
        	Label label3 = WidgetFactory.createLabel(thePanel, "Teiid Runtime Version"); // + i18n("teiidRuntimeVersion") + " : "); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        	GridDataFactory.fillDefaults().align(SWT.LEFT, SWT.CENTER).applyTo(label3);
        	
        	String versionString = "8.10";
//        	if( getVdb().getValidationVersion() != null ) {
//        		versionString = getVdb().getValidationVersion();
//        	}
        	validationVersionLabel = WidgetFactory.createLabel(thePanel, versionString);
        	validationVersionLabel.setForeground(blueColor);
        	GridDataFactory.fillDefaults().align(SWT.LEFT, SWT.CENTER).applyTo(validationVersionLabel);
        }

        final Group descGroup = WidgetFactory.createGroup(thePanel, UILabelUtil.getLabel(UiLabelConstants.LABEL_IDS.DESCRIPTION), GridData.FILL_BOTH, 2);
        descriptionTextEditor = new StyledTextEditor(descGroup, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL | SWT.WRAP | SWT.BORDER);
        GridDataFactory.fillDefaults().grab(true, true).hint(SWT.DEFAULT, 120).minSize(SWT.DEFAULT, 30).applyTo(descriptionTextEditor.getTextWidget());
        descriptionTextEditor.setText(""); //$NON-NLS-1$
        descriptionTextEditor.getTextWidget().addModifyListener(new ModifyListener() {

            @Override
            public void modifyText(ModifyEvent e) {
//                getRelationalReference().setDescription(descriptionTextEditor.getText());
            }
        });

        return thePanel;
	}
	
	
	@Override
	protected void syncWithTarget() {
		super.syncWithTarget();
		setSynching(true);
//		if( isVisible() && getTargetObject() != null && getTargetObject() instanceof Vdb) {
//			String oldText = nameText.getText();
//			String newName = oldText;
//			try {
//				newName = ((Vdb)getTargetObject()).getName(null);
//			} catch (KException e) {
//				// TODO Auto-generated catch block
//				e.printStackTrace();
//			}
//			setTextValue(nameText, newName);
//		} else {
//			nameText.setText("");
//		}
		setSynching(false);
	}

}
