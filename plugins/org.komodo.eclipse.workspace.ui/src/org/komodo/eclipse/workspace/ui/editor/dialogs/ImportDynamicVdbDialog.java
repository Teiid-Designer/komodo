package org.komodo.eclipse.workspace.ui.editor.dialogs;

import java.io.File;
import java.io.IOException;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.komodo.eclipse.workspace.ui.common.util.FileUtils;
import org.komodo.eclipse.workspace.ui.common.util.StyledTextEditor;
import org.komodo.eclipse.workspace.ui.common.util.WidgetFactory;
import org.komodo.eclipse.workspace.ui.common.widget.Dialog;

public class ImportDynamicVdbDialog extends Dialog {
    
	File selectedFile;
    private Text dynamicVdbFileName;
    private Text vdbNameFld;
    String vdbName;
    
    private String initDlgFolderName;
    
    private StyledTextEditor textEditor;
    
    private Font textFont;
	
	public ImportDynamicVdbDialog(Shell shell, String title) {
		super(shell, title);
	}
	
	@Override
	protected Control createDialogArea(Composite parent) {
		Composite panel = (Composite)super.createDialogArea(parent);
		GridLayoutFactory.swtDefaults().applyTo(panel);
		GridDataFactory.fillDefaults().grab(true, true).hint(400, 400).minSize(300, 300).applyTo(panel);
        
		textFont = new Font(null, "Monospace", 10, SWT.BOLD);
        
		internalCreateControls(panel);
		
		return panel;
	}
	
    void chooseVdbXmlFile() {
        final FileDialog dlg = new FileDialog(getShell());
        final String ddlFileName = "<undefined>";
//        final File file = new File(ddlFileName);
//        if (file.exists() && file.isDirectory()) initDlgFolderName = ddlFileName;
//        else {
//            int ndx = ddlFileName.lastIndexOf(File.separatorChar);
//            if (ndx < 0) ndx = ddlFileName.lastIndexOf('/');
//            if (file.exists()) dlg.setFileName(ddlFileName.substring(ndx + 1));
//            if (ndx >= 0) {
//                final String folderName = ddlFileName.substring(0, ndx + 1);
//                final File folder = new File(folderName);
//                if (folder.exists() && folder.isDirectory()) initDlgFolderName = folderName;
//            }
//            if (initDlgFolderName == null) initDlgFolderName = System.getProperty("user.home"); //$NON-NLS-1$
//        }
        dlg.setFilterPath(initDlgFolderName);
        dlg.setFilterExtensions(new String[] {"*.xml", "*.*"}); //$NON-NLS-1$ //$NON-NLS-2$
        dlg.setFilterNames(new String[] {"VDB XML Files", "All Files"}); //$NON-NLS-1$ //$NON-NLS-2$
        final String fileName = dlg.open();
        if (fileName != null) {
        	IPath folderPath = new Path(dlg.getFilterPath());
        	IPath fullFilePath = folderPath.append(dlg.getFileName());
        	this.selectedFile = fullFilePath.toFile();
            this.dynamicVdbFileName.setText(dlg.getFileName());
        }
        initDlgFolderName = dlg.getFilterPath();
    }

    private void internalCreateControls(Composite parent) {
        
        final Composite panel = WidgetFactory.createPanel(parent, SWT.NONE, GridData.FILL_BOTH, 1, 1);
        GridDataFactory.fillDefaults().grab(true, true).applyTo(panel);
        GridLayoutFactory.fillDefaults().applyTo(panel);

        Composite ddlFileGroup = WidgetFactory.createGroup(panel, "XML File", GridData.FILL_HORIZONTAL, 1, 3);
//        GridLayoutFactory.swtDefaults().numColumns(3).applyTo(ddlFileGroup);
//        GridDataFactory.fillDefaults().grab(true,  false).applyTo(ddlFileGroup);
        // ----------------------------------------
        // DDL File controls 
        // ----------------------------------------
        WidgetFactory.createLabel(ddlFileGroup, GridData.VERTICAL_ALIGN_CENTER, "Name");

        dynamicVdbFileName = WidgetFactory.createTextField(ddlFileGroup, SWT.NONE, GridData.FILL_HORIZONTAL) ;
        GridDataFactory.fillDefaults().grab(true,  false).applyTo(dynamicVdbFileName);
        
        Button button = WidgetFactory.createButton(ddlFileGroup, "...", GridData.HORIZONTAL_ALIGN_FILL);
        button.setToolTipText("Choose Schema DDL File");
        GridDataFactory.fillDefaults().applyTo(button);
        button.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected( final SelectionEvent event ) {
            	chooseVdbXmlFile();
                vdbXmlFileModified();
            }
        });

        Composite modelInfoGroup = WidgetFactory.createGroup(panel, "Model Definition", GridData.FILL_HORIZONTAL, 1, 3);

        // ----------------------------------------
        // Model Name controls
        // ----------------------------------------
        WidgetFactory.createLabel(modelInfoGroup, GridData.VERTICAL_ALIGN_CENTER, "Name");
        
        vdbNameFld = WidgetFactory.createTextField(modelInfoGroup);
        GridDataFactory.fillDefaults().grab(true,  false).applyTo(vdbNameFld);
        vdbNameFld.addModifyListener(new ModifyListener() {

            @Override
            public void modifyText( final ModifyEvent event ) {
            	vdbName = vdbNameFld.getText();
                validate();
            }
        });

        // ----------------------------------------
        // File contents area
        // ----------------------------------------
        Group ddlGroup = WidgetFactory.createGroup(panel, "VDB XML", SWT.NONE);
        ddlGroup.setLayoutData(new GridData(GridData.FILL_BOTH));
        
        Composite innerPanel = new Composite(ddlGroup, SWT.NONE);
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


    }
    void vdbXmlFileModified() {
        validate();

        if (selectedFile != null) {

            try {

            	textEditor.setText(FileUtils.readSafe(selectedFile));
            } catch (final IOException error) {
                // TODO: handle exception
            }
        }
    }
    
    void validate() {
    	// TODO:
    }
    
    public File getFile() {
    	return this.selectedFile;
    }
    
    public String getVdbName() {
    	return vdbName;
    }
    

	@Override
	public boolean close() {
		textFont.dispose();
		return super.close();
	}
}
