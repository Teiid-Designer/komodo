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
import org.komodo.eclipse.sql.ui.editor.panel.StyledTextEditor;
import org.komodo.eclipse.workspace.ui.common.util.WidgetFactory;
import org.komodo.eclipse.workspace.ui.common.widget.Dialog;
import org.komodo.utils.FileUtils;

public class ImportSchemaDialog  extends Dialog {
    
	File selectedFile;
    private Text schemaFileName;
    private Text modelNameFld;
    String modelName;
    
    private String initDlgFolderName;
    
    private StyledTextEditor textEditor;
    
    private Font textFont;
	
	public ImportSchemaDialog(Shell shell, String title) {
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
	
    void chooseDdlFile() {
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
        dlg.setFilterExtensions(new String[] {"*.ddl; *.sql", "*.*"}); //$NON-NLS-1$ //$NON-NLS-2$
        dlg.setFilterNames(new String[] {"DDL Files", "All Files"}); //$NON-NLS-1$ //$NON-NLS-2$
        final String fileName = dlg.open();
        if (fileName != null) {
        	IPath folderPath = new Path(dlg.getFilterPath());
        	IPath fullFilePath = folderPath.append(dlg.getFileName());
        	this.selectedFile = fullFilePath.toFile();
            this.schemaFileName.setText(dlg.getFileName());
        }
        initDlgFolderName = dlg.getFilterPath();
    }

    private void internalCreateControls(Composite parent) {
        
        final Composite panel = WidgetFactory.createPanel(parent, SWT.NONE, GridData.FILL_BOTH, 1, 1);
        GridDataFactory.fillDefaults().grab(true, true).applyTo(panel);
        GridLayoutFactory.fillDefaults().applyTo(panel);

        Composite ddlFileGroup = WidgetFactory.createGroup(panel, "DDL File", GridData.FILL_HORIZONTAL, 1, 3);
//        GridLayoutFactory.swtDefaults().numColumns(3).applyTo(ddlFileGroup);
//        GridDataFactory.fillDefaults().grab(true,  false).applyTo(ddlFileGroup);
        // ----------------------------------------
        // DDL File controls 
        // ----------------------------------------
        WidgetFactory.createLabel(ddlFileGroup, GridData.VERTICAL_ALIGN_CENTER, "Name");

        schemaFileName = WidgetFactory.createTextField(ddlFileGroup, SWT.NONE, GridData.FILL_HORIZONTAL) ;
        GridDataFactory.fillDefaults().grab(true,  false).applyTo(schemaFileName);
        
        Button button = WidgetFactory.createButton(ddlFileGroup, "...", GridData.HORIZONTAL_ALIGN_FILL);
        button.setToolTipText("Choose Schema DDL File");
        GridDataFactory.fillDefaults().applyTo(button);
        button.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected( final SelectionEvent event ) {
                chooseDdlFile();
                ddlFileModified();
            }
        });

        Composite modelInfoGroup = WidgetFactory.createGroup(panel, "Model Definition", GridData.FILL_HORIZONTAL, 1, 3);

        // ----------------------------------------
        // Model Name controls
        // ----------------------------------------
        WidgetFactory.createLabel(modelInfoGroup, GridData.VERTICAL_ALIGN_CENTER, "Name");
        
        modelNameFld = WidgetFactory.createTextField(modelInfoGroup);
        GridDataFactory.fillDefaults().grab(true,  false).applyTo(modelNameFld);
        modelNameFld.addModifyListener(new ModifyListener() {

            @Override
            public void modifyText( final ModifyEvent event ) {
            	modelName = modelNameFld.getText();
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
    void ddlFileModified() {
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
    
    public String getModelName() {
    	return modelName;
    }
    

	@Override
	public boolean close() {
		textFont.dispose();
		return super.close();
	}
}
