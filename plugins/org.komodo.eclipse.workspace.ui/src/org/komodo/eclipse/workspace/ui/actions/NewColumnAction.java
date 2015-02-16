package org.komodo.eclipse.workspace.ui.actions;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.komodo.eclipse.workspace.CoreConstants;
import org.komodo.eclipse.workspace.CorePlugin;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.AddNameObjectDialog;
import org.komodo.relational.model.Table;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;

public class NewColumnAction extends NewObjectAction {

	public NewColumnAction() {
		super(CoreConstants.RelationalType.COLUMN);
		setText("New Column");
		setImageDescriptor(CorePlugin.getDefault().getImageDescriptor(CoreConstants.Images.COLUMN_ICON));
	}

	@Override
	boolean runInTransaction(KomodoObject parent) {
		ArgCheck.isInstanceOf(Table.class, parent);
//				parent instanceof Table || 
//				parent instanceof View ||
//				parent instanceof ProcedureResultSet, "column can only be added to a table, view or result set");
		
		AddNameObjectDialog dialog = new AddNameObjectDialog(getShell(), "Add Column", "newColumn");
		dialog.open();
		boolean completed = false;
		if( dialog.getReturnCode() == IDialogConstants.OK_ID ) {
			String name = dialog.getName();
			try {
				UnitOfWork transaction = CorePlugin.getKEngine().getDefaultRepository().createTransaction("Add Column", false, null);
				((Table)parent).addColumn(transaction, name);
				transaction.commit();
				completed = true;
			} catch (KException ex) {
				MessageDialog.openError(getShell(), "Error Adding Column", ex.getMessage());
			}
		}
		
		return completed;
	}

}
