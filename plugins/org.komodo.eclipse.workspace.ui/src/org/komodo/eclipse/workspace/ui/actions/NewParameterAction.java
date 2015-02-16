package org.komodo.eclipse.workspace.ui.actions;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.komodo.eclipse.workspace.CoreConstants;
import org.komodo.eclipse.workspace.CorePlugin;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.AddNameObjectDialog;
import org.komodo.relational.model.AbstractProcedure;
import org.komodo.relational.model.Function;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.Table;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

public class NewParameterAction extends NewObjectAction {

	public NewParameterAction() {
		super(CoreConstants.RelationalType.PARAMETER);
		setText("New Parameter");
		setImageDescriptor(CorePlugin.getDefault().getImageDescriptor(CoreConstants.Images.PARAMETER_ICON));
	}

	@Override
	boolean runInTransaction(KomodoObject parent) {
		ArgCheck.isTrue(
				parent instanceof Procedure || 
				parent instanceof Function, "parameter can only be added to a procedure or function");
		
		AddNameObjectDialog dialog = new AddNameObjectDialog(getShell(), "Add Parameter", "newParameter");
		dialog.open();
		boolean completed = false;
		if( dialog.getReturnCode() == IDialogConstants.OK_ID ) {
			String name = dialog.getName();
			try {
				UnitOfWork transaction = CorePlugin.getKEngine().getDefaultRepository().createTransaction("Add Parameter", false, null);
				((AbstractProcedure)parent).addParameter(transaction, name); 
				transaction.commit();
				completed = true;
			} catch (KException ex) {
				MessageDialog.openError(getShell(), "Error Adding Column", ex.getMessage());
			}
		}
		
		return completed;
	}

}
