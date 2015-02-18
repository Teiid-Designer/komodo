package org.komodo.eclipse.workspace.ui.actions;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.komodo.core.KomodoLexicon;
import org.komodo.eclipse.workspace.CoreConstants;
import org.komodo.eclipse.workspace.CorePlugin;
import org.komodo.eclipse.workspace.ui.editor.dialogs.NewModelDialog;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.AddNameObjectDialog;
import org.komodo.relational.model.Model;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.modeshape.sequencer.teiid.lexicon.CoreLexicon.ModelType;

public class NewModelAction extends NewObjectAction {

	public NewModelAction() {
		super(CoreConstants.RelationalType.MODEL);
		setText("New Model");
		setImageDescriptor(CorePlugin.getDefault().getImageDescriptor(CoreConstants.Images.MODEL_ICON));
	}

	@Override
	boolean runInTransaction(KomodoObject parent) {
		ArgCheck.isInstanceOf(Vdb.class, parent);
		
		NewModelDialog dialog = new NewModelDialog(getShell(), "Add Model", "newModel");
		dialog.open();
		boolean completed = false;
		if( dialog.getReturnCode() == IDialogConstants.OK_ID ) {
			String name = dialog.getName();
			boolean isVirtual = dialog.isVirtual();
			try {
				Vdb vdb = (Vdb)parent;
				UnitOfWork transaction = CorePlugin.getKEngine().getDefaultRepository().createTransaction("Add Model", false, null);
				//KomodoObject newChild = parent.addChild(transaction, name, KomodoLexicon.VdbModel.NODE_TYPE);
				Model model = CorePlugin.getWorkspaceManager().createModel(transaction,  (Vdb)vdb, name);
				if( isVirtual ) {
					model.setModelType(transaction, Model.Type.VIRTUAL);
				}
				transaction.commit();
				completed = true;
			} catch (KException ex) {
				MessageDialog.openError(getShell(), "Error Adding Model", ex.getMessage());
			}
		}
		return completed;
	}

}
