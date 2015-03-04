package org.komodo.eclipse.workspace.ui.actions;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.komodo.eclipse.workspace.CoreConstants;
import org.komodo.eclipse.workspace.CorePlugin;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.AddNameObjectDialog;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;

public class NewVdbAction extends NewObjectAction {

	public NewVdbAction() {
		super(CoreConstants.RelationalType.VDB);
		setText("New VDB");
		setImageDescriptor(CorePlugin.getDefault().getImageDescriptor(CoreConstants.Images.NEW_VDB_ICON));
	}

	@Override
	boolean runInTransaction(KomodoObject parent) {
		AddNameObjectDialog dialog = new AddNameObjectDialog(getShell(), "Add VDB", StringUtils.EMPTY_STRING);
		dialog.open();
		boolean completed = false;
		if( dialog.getReturnCode() == IDialogConstants.OK_ID ) {
			String name = dialog.getName();
			try {
				UnitOfWork transaction = CorePlugin.getKEngine().getDefaultRepository().createTransaction("Add VDB", false, null);
				Vdb vdb = CorePlugin.getWorkspaceManager().createVdb(transaction, parent, name, "extPath");
				transaction.commit();
				completed = true;
			} catch (KException ex) {
				MessageDialog.openError(getShell(), "Error Adding VDB", ex.getMessage());
			}
		}
		return completed;
	}

}
