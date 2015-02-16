package org.komodo.eclipse.workspace.ui.actions;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.komodo.eclipse.workspace.CoreConstants;
import org.komodo.eclipse.workspace.CorePlugin;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.AddNameObjectDialog;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Procedure;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

public class NewResultSetAction extends NewObjectAction {

	public NewResultSetAction() {
		super(CoreConstants.RelationalType.PARAMETER);
		setText("New Parameter");
		setImageDescriptor(CorePlugin.getDefault().getImageDescriptor(CoreConstants.Images.NEW_TABLE_ICON));
	}
	@Override
	boolean runInTransaction(KomodoObject parent) {
		ArgCheck.isInstanceOf(Procedure.class, parent);
		
		boolean completed = false;
		
//		AddNameObjectDialog dialog = new AddNameObjectDialog(getShell(), "Add Result Set", "newResultSet");
//		dialog.open();
//		
//		if( dialog.getReturnCode() == IDialogConstants.OK_ID ) {
//			String name = dialog.getName();
//			try {
//				UnitOfWork transaction = CorePlugin.getKEngine().getDefaultRepository().createTransaction("Add Result Set", false, null);
//				// TODO: FIX((Procedure)parent).addResultSet(transaction, name); //CreateTable.TABLE_ELEMENT);
//				transaction.commit();
//				completed = true;
//			} catch (KException ex) {
//				MessageDialog.openError(getShell(), "Error Adding Result Set", ex.getMessage());
//			}
//		}
		MessageDialog.openError(getShell(), "Error Adding Result Set", "Not Yet Implemented....");
		
		return completed;
	}

}
