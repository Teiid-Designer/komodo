package org.komodo.eclipse.workspace.ui.editor.dialogs;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;

import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.SWT;
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
import org.komodo.eclipse.sql.ui.editor.panel.StyledTextEditor;
import org.komodo.eclipse.workspace.ui.common.util.WidgetFactory;
import org.komodo.eclipse.workspace.ui.common.widget.Dialog;

public class ExportToDynamicVdbDialog   extends Dialog {
	private static final String XML_EXT = "xml";
    
    private Font textFont;
    //============================================================================================================================
    // Variables
    
    private StyledTextEditor textEditor;
    
    private String vdbXmlText;
    
    private Button selectFileForExport;
	
	public ExportToDynamicVdbDialog(Shell shell, String title, String vdbXmlText) {
		super(shell, title);
        
        this.vdbXmlText = vdbXmlText;
	}
	
	@Override
	protected Control createDialogArea(Composite parent) {
		Composite panel = (Composite)super.createDialogArea(parent);
		GridLayoutFactory.swtDefaults().applyTo(panel);
		GridDataFactory.fillDefaults().grab(true, true).hint(400, 400).minSize(300, 300).applyTo(panel);
		
		internalCreateControls(panel);
		
		return panel;
	}
    public String askUserForOutputFilename() {
        Shell shell = getShell();
        FileDialog dlg = new FileDialog(shell, SWT.SAVE);
        dlg.setFilterExtensions(new String[] {"*.xml"}); //$NON-NLS-1$ //$NON-NLS-2$ 
        dlg.setText("Export as Dynamic VDB XML File");
        dlg.setFileName("dynamic_vdb.xml");

        return dlg.open();
    }
    
    void internalCreateControls(Composite panel) {
        textFont = new Font(null, "Monospace", 10, SWT.BOLD);
        
        /*
         * Create the VDB Deploy button 
         */
        {
            Composite buttonPanel = new Composite(panel,SWT.NONE);
            buttonPanel.setLayout(new GridLayout(2, false));
            buttonPanel.setLayoutData(new GridData()); 

            selectFileForExport = new Button(buttonPanel, SWT.PUSH);
            selectFileForExport.setText("Export");
            selectFileForExport.setToolTipText("Export VDB as Dynamic VDB XML File");
            selectFileForExport.setLayoutData(new GridData());
            selectFileForExport.setEnabled(true);
            selectFileForExport.addSelectionListener(new SelectionAdapter() {

                @Override
                public void widgetSelected(SelectionEvent e) {
                   handleExportDDLToFileSystem();
                }
            });

            
        }
        
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
        
        this.textEditor.setText(this.vdbXmlText);
    }
    
    /**
     * Export the current string content of the sql display to a user-selected file
     */
    public File handleExportDDLToFileSystem() {

    	String fileNameString = askUserForOutputFilename();
    	String fileExtension = XML_EXT;
    	String fileOuputString = vdbXmlText;
    			
        // If there is no file extension, add .xml
        if (fileNameString.indexOf('.') == -1 && fileExtension != null) {
            fileNameString = fileNameString + "." + fileExtension; //$NON-NLS-1$
        }

        FileWriter fileWriter = null;
        BufferedWriter outputBufferWriter = null;
        PrintWriter printWriter = null;
        try {
            fileWriter = new FileWriter(fileNameString);
            outputBufferWriter = new BufferedWriter(fileWriter);
            printWriter = new PrintWriter(outputBufferWriter);
            printWriter.write(fileOuputString);
        } catch (Exception e) {
            // TODO: log...
        }

        finally {
            // Clean up writers & buffers
            if (printWriter != null) {
                printWriter.close();
            }

            try {
                if (outputBufferWriter != null) {
                    outputBufferWriter.close();
                }
            } catch (java.io.IOException e) {
            }

            try {
                if (fileWriter != null) {
                    fileWriter.close();
                }
            } catch (java.io.IOException e) {
            }
        }
        return new File(fileNameString);
    }

	@Override
	public boolean close() {
		textFont.dispose();
		return super.close();
	}
}
