package org.komodo.eclipse.workspace.ui.actions;

import java.util.ArrayList;
import java.util.Collection;

import org.komodo.spi.repository.KomodoObject;

public class NewObjectActionFactory {

	private static Collection<NewObjectAction> actionArray = new ArrayList<NewObjectAction>();
	
	public static NewObjectAction getAction(int type, KomodoObject parent) {
		if( actionArray.isEmpty() ) createActions();
		for( NewObjectAction action : actionArray) {
			if( type == action.getType()) {
				action.setTarget(parent);
				return action;
			}
		}
		return null;
	}
	
	private static void createActions() {
		actionArray.add(new NewVdbAction());
		actionArray.add(new NewModelAction());
		actionArray.add(new NewTableAction());
		actionArray.add(new NewViewAction());
		actionArray.add(new NewColumnAction());
		actionArray.add(new NewProcedureAction());
		actionArray.add(new NewParameterAction());
		actionArray.add(new NewResultSetAction());
	}
}
