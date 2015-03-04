package org.komodo.eclipse.workspace.ui.explorer;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.komodo.eclipse.workspace.CorePlugin;
import org.komodo.relational.vdb.Vdb;
import org.komodo.repository.RepositoryTools;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.KLog;

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
//			Repository repo = CorePlugin.getKEngine().getDefaultRepository();
			
			try {
//				KomodoObject wsNode = repo.komodoWorkspace(null);
				KomodoObject[] vdbs = CorePlugin.getWorkspaceManager().findVdbs(null);
				KomodoObject lib = CorePlugin.getLibrary();
				Set<KomodoObject> allRoots = new HashSet<KomodoObject>();
				if( lib != null )  allRoots.add(lib);
				for( KomodoObject vdb : vdbs) {
					allRoots.add(vdb);
				}
				KomodoObject[] models = CorePlugin.getWorkspaceManager().findModels(null);
				for( KomodoObject model : models) {
					allRoots.add(model);
				}
				
				return allRoots.toArray(new KomodoObject[allRoots.size()]);
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
		boolean debug = false;
		
		if( parentElement instanceof KomodoObject ) {
			KomodoObject ko = (KomodoObject)parentElement;
			if( debug ) {
				try {
					String parentClass = parentElement.getClass().toString();
					if( parentClass.equalsIgnoreCase("org.komodo.repository.ObjectImpl")) {
						KLog.getLogger().debug("  WSCP getChildren() parent name = [" + ko.getName(null) + "] = " + parentElement.getClass(), new Object[0]);
					}
				} catch (Exception e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
			}
			KomodoObject[] children = new KomodoObject[0];

			try {
				children = ko.getChildren(null);
			} catch (KException e) {
				e.printStackTrace();
			}
			if( debug ) {
				try {
					for( KomodoObject obj:children) {
						KLog.getLogger().debug("   >>>  Child name = [" + obj.getName(null) + "] class = " + obj.getClass(), new Object[0]);
					}
				} catch (KException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
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
