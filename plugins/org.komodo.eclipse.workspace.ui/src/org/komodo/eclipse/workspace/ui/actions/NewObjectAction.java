package org.komodo.eclipse.workspace.ui.actions;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.komodo.eclipse.workspace.CoreConstants;
import org.komodo.eclipse.workspace.CorePlugin;
import org.komodo.eclipse.workspace.util.RelationalIdentifier;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

public abstract class NewObjectAction extends Action {
	
	int objectType = CoreConstants.RelationalType.UNKNOWN;
	KomodoObject parent;

	public NewObjectAction(int type) {
		this.objectType = type;
		setText(RelationalIdentifier.getTypeDisplayName(type));
	}
	
	public boolean execute() {
		if( parent != null ) {
			return runInTransaction(parent);
		} else {
			MessageDialog.openError(getShell(), "Error in New Object Action", "Parent is NULL");
		}
		
		return false;
	}
	
	abstract boolean runInTransaction(KomodoObject parent);

	protected Shell getShell() {
		return Display.getCurrent().getActiveShell();
	}
	
	public int getType() {
		return objectType;
	}
	
	public void setTarget(KomodoObject target) {
		this.parent = target;
	}
}
