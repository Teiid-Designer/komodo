package org.komodo.eclipse.workspace.ui.editor.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.AddNameObjectDialog;

public class NewModelDialog extends AddNameObjectDialog {
	boolean isVirtual = false;

	public NewModelDialog(Shell shell, String title, String initialName) {
		super(shell, title, initialName);
	}

	@Override
	protected Control createDialogArea(Composite parent) {
		Composite panel = (Composite)super.createDialogArea(parent);
		
		final Button isVirtualCB = new Button(panel, SWT.CHECK);
		isVirtualCB.setText("Virtual Model");
		isVirtualCB.addSelectionListener(new SelectionListener() {
			
			@Override
			public void widgetSelected(SelectionEvent arg0) {
				isVirtual = isVirtualCB.getSelection();
			}
			
			@Override
			public void widgetDefaultSelected(SelectionEvent arg0) {
				// TODO Auto-generated method stub
				
			}
		});
		
		return panel;
	}

	public boolean isVirtual() {
		return isVirtual;
	}
	
}
