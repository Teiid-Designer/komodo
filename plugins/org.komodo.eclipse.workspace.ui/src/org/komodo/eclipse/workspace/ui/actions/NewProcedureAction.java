package org.komodo.eclipse.workspace.ui.actions;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.komodo.eclipse.workspace.CoreConstants;
import org.komodo.eclipse.workspace.CorePlugin;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.AddNameObjectDialog;
import org.komodo.relational.model.Model;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

public class NewProcedureAction extends NewObjectAction {

	public NewProcedureAction() {
		super(CoreConstants.RelationalType.PROCEDURE);
		setText("New Procedure");
		setImageDescriptor(CorePlugin.getDefault().getImageDescriptor(CoreConstants.Images.NEW_PROCEDURE_ICON));
	}

	@Override
	boolean runInTransaction(KomodoObject parent) {
		ArgCheck.isInstanceOf(Model.class, parent);
		
		AddNameObjectDialog dialog = new AddNameObjectDialog(getShell(), "Add Procedure", "newProcedure");
		dialog.open();
		boolean completed = false;
		if( dialog.getReturnCode() == IDialogConstants.OK_ID ) {
			String name = dialog.getName();
			try {
				UnitOfWork transaction = CorePlugin.getKEngine().getDefaultRepository().createTransaction("Add Procedure", false, null);
				((Model)parent).addProcedure(transaction, name);
				transaction.commit();
				completed = true;
			} catch (KException ex) {
				MessageDialog.openError(getShell(), "Error Adding Procedure", ex.getMessage());
			}
		}
		
		return completed;
	}

}
