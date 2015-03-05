package org.komodo.eclipse.workspace.ui.actions;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.komodo.eclipse.workspace.CoreConstants;
import org.komodo.eclipse.workspace.CorePlugin;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.AddNameObjectDialog;
import org.komodo.eclipse.workspace.util.RelationalIdentifier;
import org.komodo.relational.model.Model;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;

public class NewTableAction extends NewObjectAction {

	public NewTableAction() {
		super(CoreConstants.RelationalType.TABLE);
		setText("New Table");
		setImageDescriptor(CorePlugin.getDefault().getImageDescriptor(CoreConstants.Images.NEW_TABLE_ICON));
	}

	@Override
	protected boolean runInTransaction(KomodoObject parent) {
		ArgCheck.isInstanceOf(Model.class, parent);
		
		AddNameObjectDialog dialog = new AddNameObjectDialog(getShell(), "Add Table", StringUtils.EMPTY_STRING);
		dialog.open();
		boolean completed = false;
		if( dialog.getReturnCode() == IDialogConstants.OK_ID ) {
			String name = dialog.getName();
			try {
				UnitOfWork transaction = CorePlugin.getKEngine().getDefaultRepository().createTransaction("Add Table", false, null);
				((Model)parent).addTable(transaction, name);
				transaction.commit();
				completed = true;
			} catch (KException ex) {
				MessageDialog.openError(Display.getCurrent().getActiveShell(), "Error Adding Table", ex.getMessage());
			}
		}
		
		return completed;
	}

	@Override
	public void setTarget(KomodoObject target) {
		super.setTarget(target);
		
		if( RelationalIdentifier.isVirtual(target)) {
			setImageDescriptor(CorePlugin.getDefault().getImageDescriptor(CoreConstants.Images.NEW_VIRTUAL_TABLE_ICON));
		} else {
			setImageDescriptor(CorePlugin.getDefault().getImageDescriptor(CoreConstants.Images.NEW_TABLE_ICON));
		}
	}
	
	
}
