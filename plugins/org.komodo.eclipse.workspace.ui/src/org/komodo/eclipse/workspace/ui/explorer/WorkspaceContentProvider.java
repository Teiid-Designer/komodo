package org.komodo.eclipse.workspace.ui.explorer;

import java.util.List;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.komodo.eclipse.workspace.CorePlugin;
import org.komodo.relational.vdb.Vdb;
import org.komodo.repository.RepositoryTools;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;

public class WorkspaceContentProvider implements ITreeContentProvider {

	@Override
	public void dispose() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public Object[] getElements(Object inputElement) {
		if( inputElement instanceof KomodoObject ) {
			KomodoObject ko = (KomodoObject)inputElement;

			Object[] children = new Object[0];

			try {
				children = ko.getChildren(null);
			} catch (KException e) {
				e.printStackTrace();
			}
			
			return children;
		} else {
			Repository repo = CorePlugin.getKEngine().getDefaultRepository();
			
			try {
				KomodoObject wsNode = repo.komodoWorkspace(null);
				Vdb[] vdbs = CorePlugin.getWorkspaceManager().findVdbs(null);
				System.out.println("  WSCP getElements()");
				for( Vdb vdb:vdbs) {
					System.out.println("   >>>  VDB class = " + vdb.getClass());
				}
				return vdbs;
	//			return wsNode.getChildren(null);
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return new Object[0];
	}

	@Override
	public Object[] getChildren(Object parentElement) {
		
		if( parentElement instanceof KomodoObject ) {
			KomodoObject ko = (KomodoObject)parentElement;
			try {
				String parentClass = parentElement.getClass().toString();
				if( parentClass.equalsIgnoreCase("org.komodo.repository.ObjectImpl")) {
					System.out.println("  WSCP getChildren() parent name = [" + ko.getName(null) + "] = " + parentElement.getClass());
				}
			} catch (Exception e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
			KomodoObject[] children = new KomodoObject[0];

			try {
				children = ko.getChildren(null);
			} catch (KException e) {
				e.printStackTrace();
			}
			
			try {
				for( KomodoObject obj:children) {
					System.out.println("   >>>  Child name = [" + obj.getName(null) + "] class = " + obj.getClass());
				}
			} catch (KException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			return children;
		}
		return null;
	}

	@Override
	public Object getParent(Object element) {
		if( element instanceof KomodoObject ) {
			KomodoObject ko = (KomodoObject)element;
			if( ko.getAbsolutePath().equals("/")) return null;
			try {
				return ko.getParent(null);
			} catch (KException e) {
				// TODO may not have a parent, so swallowing the 
				//e.printStackTrace();
			}
			
		}
		return null;
	}

	@Override
	public boolean hasChildren(Object element ) {
		return  getChildren(element).length > 0;
	}
}
