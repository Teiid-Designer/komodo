package org.komodo.eclipse.workspace.ui.editor.panels;

import java.util.Properties;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.komodo.eclipse.workspace.CoreConstants;
import org.komodo.eclipse.workspace.CorePlugin;
import org.komodo.eclipse.workspace.ui.actions.NewColumnAction;
import org.komodo.eclipse.workspace.ui.actions.NewObjectAction;
import org.komodo.eclipse.workspace.ui.actions.NewObjectActionFactory;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.AddNameObjectDialog;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ShowStringDialog;
import org.komodo.eclipse.workspace.ui.explorer.WorkspaceTreeViewer;
import org.komodo.eclipse.workspace.util.RelationalIdentifier;
import org.komodo.relational.model.Model;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.constants.ExportConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

public class WorkspaceTreeViewerPanel extends Composite {
	KomodoObject rootNode;
	
	WorkspaceTreeViewer wsTreeViewer;
	IAction refreshTreeAction;
	IAction deleteObjectAction;
	IAction newVdbAction;

	public WorkspaceTreeViewerPanel(Composite parent, int style) {
		super(parent, style);
		
		GridLayoutFactory.fillDefaults().applyTo(this);
		GridDataFactory.fillDefaults().grab(true,  true).applyTo(this);
		
		addTreeViewer();
	}
	
	private void addTreeViewer() {

    	this.wsTreeViewer = new WorkspaceTreeViewer(this, SWT.BORDER | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
    	this.wsTreeViewer.getControl().setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        final MenuManager actionMenu = new MenuManager();
        this.wsTreeViewer.getControl().setMenu(actionMenu.createContextMenu(this));
        this.wsTreeViewer.addSelectionChangedListener(new ISelectionChangedListener() {
            /**
             * {@inheritDoc}
             * 
             * @see oblafond@redhat.comrg.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
             */
            @Override
            public void selectionChanged( final SelectionChangedEvent event ) {

            	IStructuredSelection sel = (IStructuredSelection)wsTreeViewer.getSelection();
                actionMenu.removeAll();
                if (sel.size() == 1 && sel.getFirstElement() instanceof KomodoObject ) {
                	final KomodoObject node = (KomodoObject)sel.getFirstElement();
                	
					if( RelationalIdentifier.isType(node, CoreConstants.RelationalType.TABLE) || 
						RelationalIdentifier.isType(node, CoreConstants.RelationalType.VIEW)) {
						final NewObjectAction action = NewObjectActionFactory.getAction(CoreConstants.RelationalType.COLUMN, node);
						IAction newColumn = new Action(action.getText(), action.getImageDescriptor()) {
							 @Override
						     public void run() {
								 boolean completed = action.execute();
								 if( completed ) {
									 wsTreeViewer.refresh();
								 }
							 }
						};
						actionMenu.add(newColumn);
					} else if( RelationalIdentifier.isType(node, CoreConstants.RelationalType.STORED_PROCEDURE) ||
							RelationalIdentifier.isType(node, CoreConstants.RelationalType.VIRTUAL_PROCEDURE) ) {
							final NewObjectAction action = NewObjectActionFactory.getAction(CoreConstants.RelationalType.PARAMETER, node);
							IAction newParmeter = new Action(action.getText(), action.getImageDescriptor()) {
								 @Override
							     public void run() {
									 boolean completed = action.execute();
									 if( completed ) {
										 wsTreeViewer.refresh();
									 }
								 }
							};
							actionMenu.add(newParmeter);
					} else if( RelationalIdentifier.isType(node, CoreConstants.RelationalType.MODEL )) {
						if( !RelationalIdentifier.isVirtual(node)) {
							final NewObjectAction tAction = NewObjectActionFactory.getAction(CoreConstants.RelationalType.TABLE, node);
							IAction newTable = new Action(tAction.getText(), tAction.getImageDescriptor()) {
								 @Override
							     public void run() {
									 boolean completed = tAction.execute();
									 if( completed ) {
										 wsTreeViewer.refresh();
									 }
								 }
							};
							actionMenu.add(newTable);
						}
						final NewObjectAction pAction = NewObjectActionFactory.getAction(CoreConstants.RelationalType.STORED_PROCEDURE, node);
						IAction newPAction = new Action(pAction.getText(), pAction.getImageDescriptor()) {
							 @Override
						     public void run() {
								 boolean completed = pAction.execute();
								 if( completed ) {
									 wsTreeViewer.refresh();
								 }
							 }
						};
						actionMenu.add(newPAction);
						final NewObjectAction vAction = NewObjectActionFactory.getAction(CoreConstants.RelationalType.VIEW, node);
						IAction newVAction = new Action(vAction.getText(), vAction.getImageDescriptor()) {
							 @Override
						     public void run() {
								 boolean completed = vAction.execute();
								 if( completed ) {
									 wsTreeViewer.refresh();
								 }
							 }
						};
						actionMenu.add(newVAction);
					} else if( RelationalIdentifier.isType(node, CoreConstants.RelationalType.VDB )) {
						final NewObjectAction mAction = NewObjectActionFactory.getAction(CoreConstants.RelationalType.MODEL, node);
						IAction newMAction = new Action(mAction.getText(), mAction.getImageDescriptor()) {
							 @Override
						     public void run() {
								 boolean completed = mAction.execute();
								 if( completed ) {
									 wsTreeViewer.refresh();
								 }
							 }
						};
						actionMenu.add(newMAction);
						IAction showVdbContents = new Action("Show VDB Contents (formatted)") {
							 @Override
						     public void run() {
								 try {
									Properties props = new Properties();
									props.put(ExportConstants.USE_TABS_PROP_KEY, true);
									
									String ddlXmlString = ((Vdb)node).export(null, props);
									if( ddlXmlString == null || ddlXmlString.isEmpty() ) {
										ddlXmlString = "< No VDB contents >";
									}
									ShowStringDialog dialog = new ShowStringDialog(getShell(), "Dynamic VDB Contents", ddlXmlString);
									dialog.open();
								} catch (KException e) {
									// TODO Auto-generated catch block
									e.printStackTrace();
								}
							 }
						};
						actionMenu.add(showVdbContents);
						IAction showVdbContentsRaw = new Action("Show VDB Contents (unformatted)") {
							 @Override
						     public void run() {
								 try {
									Properties props = new Properties();
									
									String ddlXmlString = ((Vdb)node).export(null, props);
									if( ddlXmlString == null || ddlXmlString.isEmpty() ) {
										ddlXmlString = "< No VDB contents >";
									}
									ShowStringDialog dialog = new ShowStringDialog(getShell(), "Dynamic VDB Contents (Unformatted)", ddlXmlString);
									dialog.open();
								} catch (KException e) {
									// TODO Auto-generated catch block
									e.printStackTrace();
								}
							 }
						};
						actionMenu.add(showVdbContentsRaw);
					} else if( RelationalIdentifier.isType(node, CoreConstants.RelationalType.LIBRARY )) {
						final NewObjectAction mAction = NewObjectActionFactory.getAction(CoreConstants.RelationalType.MODEL, node);
						IAction newMAction = new Action(mAction.getText(), mAction.getImageDescriptor()) {
							 @Override
						     public void run() {
								 boolean completed = mAction.execute();
								 if( completed ) {
									 wsTreeViewer.refresh();
								 }
							 }
						};
						actionMenu.add(newMAction);
						final NewObjectAction vdbAction = NewObjectActionFactory.getAction(CoreConstants.RelationalType.VDB, node);
						IAction newVdbAction = new Action(vdbAction.getText(), mAction.getImageDescriptor()) {
							 @Override
						     public void run() {
								 boolean completed = vdbAction.execute();
								 if( completed ) {
									 wsTreeViewer.refresh();
								 }
							 }
						};
						actionMenu.add(newVdbAction);
					}
					
					actionMenu.add(new Separator());
					actionMenu.add(deleteObjectAction);
					actionMenu.add(new Separator());
					actionMenu.add(refreshTreeAction);
                }
            }
        });
        
        refreshTreeAction = new Action("Refresh") {
			 @Override
	        public void run() {
				 wsTreeViewer.refresh();
	        }
		};
		
        deleteObjectAction = new Action("Delete") {
			 @Override
	        public void run() {
				IStructuredSelection sel = (IStructuredSelection)wsTreeViewer.getSelection();
				if (sel.size() == 1 && sel.getFirstElement() instanceof KomodoObject ) {
					KomodoObject ko = (KomodoObject)sel.getFirstElement();
					UnitOfWork transaction = null;
					boolean success = false;
	                try {
	                	transaction = CorePlugin.getKEngine().getDefaultRepository().createTransaction("Delete", false, null);
	                	KomodoObject parentKO = ko.getParent(null);
						parentKO.removeChild(null, ko.getName(transaction));
						transaction.commit();
						success = true;
					} catch (KException e) {
						e.printStackTrace();
					} finally {
						if( transaction != null ) {
							if( !success ) {
							}
						}
						wsTreeViewer.refresh();
					}
				}
	        }
		};
		deleteObjectAction.setImageDescriptor(CorePlugin.getDefault().getImageDescriptor(CoreConstants.Images.DELETE));
		
		newVdbAction = new Action("Add VDB") {
			 @Override
	        public void run() {
				AddNameObjectDialog dialog = new AddNameObjectDialog(getShell(), "Add VDB", "newVDB");
				dialog.open();
				
				if( dialog.getReturnCode() == IDialogConstants.OK_ID ) {
					String name = dialog.getName();
					try {
						UnitOfWork transaction = CorePlugin.getKEngine().getDefaultRepository().createTransaction("Add Column", false, null);
						Vdb vdb = CorePlugin.getWorkspaceManager().createVdb(transaction, null, name, "extPath");
						transaction.commit();
					} catch (KException ex) {
						MessageDialog.openError(getShell(), "Error Adding VDB", ex.getMessage());
					} finally {
						wsTreeViewer.refresh();
					}
				}
	        }
		};
		actionMenu.add(newVdbAction);
		
        //this.wsTreeViewer.setInput(getRootNode());
        
        //System.out.println(" Finished creating WS Tree Viewer");
    }
	
	private KomodoObject getRootNode() {
		Repository repo = CorePlugin.getKEngine().getDefaultRepository();
		
		try {
			rootNode = repo.komodoWorkspace(null);
		} catch (KException e) {
			e.printStackTrace();
		}
		
		return rootNode;
	}
	
	public void refresh() {
		this.wsTreeViewer.refresh();
	}
	
	public KomodoObject getCurrentSelection() {
        IStructuredSelection sel = (IStructuredSelection)wsTreeViewer.getSelection();

        if (sel.size() == 1 && sel.getFirstElement() instanceof KomodoObject ) {
        	return (KomodoObject)sel.getFirstElement();
        }
        
        return null;
	}
	
	public void addDoubleClickListener(IDoubleClickListener listener) {
		wsTreeViewer.addDoubleClickListener(listener);
	}
	
	@Override
	public void dispose() {
		if( wsTreeViewer != null ) {
			wsTreeViewer.close();
		}
		super.dispose();
	}
}
