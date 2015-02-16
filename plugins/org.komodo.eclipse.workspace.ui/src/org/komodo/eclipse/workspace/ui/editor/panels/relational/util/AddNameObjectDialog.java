package org.komodo.eclipse.workspace.ui.editor.panels.relational.util;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.custom.CLabel;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.komodo.eclipse.workspace.CoreConstants;
import org.komodo.eclipse.workspace.CorePlugin;
import org.komodo.eclipse.workspace.ui.common.util.WidgetFactory;
import org.komodo.eclipse.workspace.ui.common.widget.Dialog;

public class AddNameObjectDialog  extends Dialog {
    private Text nameText;
    private CLabel messageLabel;

    private static final int LABEL_GRID_STYLE = GridData.HORIZONTAL_ALIGN_BEGINNING;
    
    //=============================================================
    // Instance variables
    //=============================================================
    private String name;
        
    //================================aliasPanel=============================
    // Constructors
    //=============================================================
    /**
     * AliasEntryDialog constructor.
     * 
     * @param parent   parent of this dialog
     * @param transObj the transformation EObject
     * @param title    dialog display title
     */
    public AddNameObjectDialog(Shell shell, String title, String initialName) {
        super(shell, title);
        name = initialName;
    }
        
    //=============================================================
    // Instance methods
    //=============================================================

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite panel = (Composite)super.createDialogArea(parent);
               
        GridLayoutFactory.swtDefaults().numColumns(2).applyTo(panel);
        GridDataFactory.swtDefaults().hint(300, 80).grab(true,  true).applyTo(panel);

        WidgetFactory.createLabel(panel, LABEL_GRID_STYLE, 1, "Name");

        nameText = WidgetFactory.createTextField(panel);
        nameText.setTextLimit(50);
        
        if( name != null ) {
        	nameText.setText(name);
        }
        
        nameText.addModifyListener(new ModifyListener() {
			
			@Override
			public void modifyText(ModifyEvent e) {
	            String text = nameText.getText();
	            if ((text == null) || (text.trim().equals(""))) { //$NON-NLS-1$
	                setError("Name cannot be empty");
	                setOkEnabled(false);
	            } else {
	                setError(null);
	                setOkEnabled(true);            
	            }
			}
		});
        
        GridDataFactory.swtDefaults().hint(300, 16).grab(true,  false).applyTo(nameText);

        this.messageLabel = WidgetFactory.createLabel(panel);
        this.messageLabel.setText(" "); //$NON-NLS-1$
        GridDataFactory.swtDefaults().span(2, 1).grab(true,  false).applyTo(messageLabel);

        
        return panel;
    }


    private void setError(String message) {
        if (message == null) {
        	this.messageLabel.setVisible(false);
        } else {
        	this.messageLabel.setVisible(true);
            Image image = CorePlugin.getDefault().getImage(CoreConstants.Images.ERROR_ICON);
            this.messageLabel.setImage(image);
        }
        this.messageLabel.setText(message);
        this.messageLabel.redraw();
    }
    
    @Override
    public void create() {
        super.create();
//        setOkEnabled(false);
    }
    @Override
    protected void okPressed() {
        name = nameText.getText();
        super.okPressed();
    }
    
    public void setOkEnabled(boolean enabled) {
        getButton(IDialogConstants.OK_ID).setEnabled(enabled);
    }
    
    /**
     *  Get the alias name entry
     * @return the desired alias name
     */
    public String getName() {
        return name;
    }

}