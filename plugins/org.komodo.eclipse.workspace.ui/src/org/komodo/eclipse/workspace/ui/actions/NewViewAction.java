package org.komodo.eclipse.workspace.ui.actions;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.komodo.eclipse.workspace.CoreConstants;
import org.komodo.eclipse.workspace.CorePlugin;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.AddNameObjectDialog;
import org.komodo.relational.model.Model;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;

public class NewViewAction extends NewObjectAction {

	public NewViewAction() {
		super(CoreConstants.RelationalType.VIEW);
		setText("New View");
		setImageDescriptor(CorePlugin.getDefault().getImageDescriptor(CoreConstants.Images.NEW_VIRTUAL_TABLE_ICON));
	}

	@Override
	boolean runInTransaction(KomodoObject parent) {
		ArgCheck.isInstanceOf(Model.class, parent);
		
		AddNameObjectDialog dialog = new AddNameObjectDialog(getShell(), "Add View", "newView");
		dialog.open();
		boolean completed = false;
		if( dialog.getReturnCode() == IDialogConstants.OK_ID ) {
			String name = dialog.getName();
			try {
				UnitOfWork transaction = CorePlugin.getKEngine().getDefaultRepository().createTransaction("Add View", false, null);
				((Model)parent).addView(transaction, name);
				transaction.commit();
				completed = true;
			} catch (KException ex) {
				MessageDialog.openError(Display.getCurrent().getActiveShell(), "Error Adding View", ex.getMessage());
			}
		}
		MessageDialog.openWarning(getShell(), "New View Action", "Not Yet Implemented");
		return false;
	}

}
