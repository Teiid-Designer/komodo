package org.komodo.eclipse.workspace.ui.explorer;

import java.awt.GridLayout;

import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;

public class WorkspaceExplorerPanel extends Composite implements ISelectionChangedListener {
	
	private TreeViewer _viewer;

	public WorkspaceExplorerPanel(Composite parent, int style) {
		super(parent, style | SWT.FILL);
		
		GridLayoutFactory.swtDefaults().applyTo(this);
//		GridDataFactory.swtDefaults().grab(true, true).applyTo(this);
	
	     GridData gd = new GridData(GridData.FILL_BOTH);
	     this.setLayoutData(gd);
	        
		createContents();
	}
	
	private void createContents() {
		
		WorkspaceTreeViewer treeViewer = new WorkspaceTreeViewer(this);

		// Add Label
//		Label label_1 = new Label(this, SWT.NONE);
//		label_1.setText("This is the Komodo workspace tree");
//		
//		Composite treeViewerPanel = new Composite(this, SWT.FILL);
//    	//since tooltip will show sql statements, we must use left to right orientation
//        _viewer = new TreeViewer(treeViewerPanel, SWT.BORDER | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL); 
//        
//        GridLayoutFactory.swtDefaults().applyTo(treeViewerPanel);
//        GridData gd = new GridData(GridData.FILL_BOTH);
//        treeViewerPanel.setLayoutData(gd);
//		
//        _viewer.addSelectionChangedListener(this);
//        _viewer.setContentProvider(new WorkspaceContentProvider());
//        WorkspaceLabelProvider labelProvider = new WorkspaceLabelProvider();
//        _viewer.setLabelProvider(labelProvider);
//        
//        _viewer.setInput(this);
//
//        MenuManager menuManager = new MenuManager();
//        menuManager.setRemoveAllWhenShown(true);
//        menuManager.addMenuListener(new IMenuListener()
//        {
//            public void menuAboutToShow(IMenuManager manager)
//            {
//                fillMenu(manager);
//            }
//
//			private void fillMenu(IMenuManager manager) {
//				// TODO Auto-generated method stub
//				
//			}
//        }
//        );
//        Menu contextMenu = menuManager.createContextMenu(_viewer.getControl());
//        _viewer.getControl().setMenu(contextMenu);
		
//        Label label_2 = new Label(this, SWT.NONE);
//		label_2.setText("AFTER tree");
		
		
	}

	@Override
	public void selectionChanged(SelectionChangedEvent event) {
		// TODO Auto-generated method stub
		
	}


}
