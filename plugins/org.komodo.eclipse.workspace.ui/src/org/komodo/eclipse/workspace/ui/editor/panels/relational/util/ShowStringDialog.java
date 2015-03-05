package org.komodo.eclipse.workspace.ui.editor.panels.relational.util;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.komodo.eclipse.sql.ui.editor.panel.StyledTextEditor;
import org.komodo.eclipse.workspace.ui.common.util.WidgetFactory;
import org.komodo.eclipse.workspace.ui.common.widget.Dialog;

public class ShowStringDialog extends Dialog {
    private static int currentSize = 10;
    private static String currentName = "Monospace";  //$NON-NLS-1$
    private static int currentStyle = 0;
    
    private Font textFont;
    //============================================================================================================================
    // Variables
    
    private StyledTextEditor textEditor;
    
    private String fileText;
    


    //============================================================================================================================
    // Constructors
        
    /**<p>
     * </p>
     * @param shell the shell
     * @param theXmlText the xml text
     * @since 4.0
     */
    public ShowStringDialog(final Shell shell, final String title, String fileText) {
        super(shell, title);
        
        this.fileText = fileText;
    }
    
    //============================================================================================================================
    // Overridden Methods

    /**<p>
     * </p>
     * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
     * @since 4.0
     */
    @Override
    protected Control createDialogArea(final Composite parent) {
        final Composite dlgPanel = (Composite)super.createDialogArea(parent);
        
        textFont = new Font(null, currentName, currentSize, SWT.BOLD);
        
        Group descGroup = WidgetFactory.createGroup(dlgPanel, "Contents", SWT.NONE);
        descGroup.setLayoutData(new GridData(GridData.FILL_BOTH));
        
        Composite innerPanel = new Composite(descGroup, SWT.NONE);
        innerPanel.setLayout(new GridLayout());
        GridData pgd = new GridData(GridData.FILL_BOTH);
        pgd.minimumWidth = 400;
        pgd.minimumHeight = 400;
        pgd.grabExcessVerticalSpace = true;
        pgd.grabExcessHorizontalSpace = true;
        innerPanel.setLayoutData(pgd);
        
        this.textEditor = new StyledTextEditor(innerPanel, SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL | SWT.BORDER);
        this.textEditor.getTextWidget().setFont(textFont);
        this.textEditor.getTextWidget().setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_WIDGET_LIGHT_SHADOW));
//        this.textEditor.getTextWidget().setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_MAGENTA));
        GridData gdt = new GridData(GridData.FILL_BOTH);
        gdt.widthHint = 400;
        gdt.heightHint = 400;
        this.textEditor.setLayoutData(gdt);
        this.textEditor.setEditable(false);
        this.textEditor.setAllowFind(false);
        this.textEditor.getTextWidget().setWordWrap(true);
        
        this.textEditor.setText(fileText);
        
        return dlgPanel;
    }

	@Override
	protected Control createContents(Composite parent) {
		// TODO Auto-generated method stub
		Control superControl =  super.createContents(parent);
		
		getButton(IDialogConstants.OK_ID).setEnabled(true);
		
		return superControl;
	}

	@Override
	public boolean close() {
		textFont.dispose();
		return super.close();
	}
}