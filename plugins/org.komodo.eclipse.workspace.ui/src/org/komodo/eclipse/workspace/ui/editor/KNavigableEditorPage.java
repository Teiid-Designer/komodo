package org.komodo.eclipse.workspace.ui.editor;

import java.beans.PropertyChangeEvent;
import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Hyperlink;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.internal.forms.widgets.FormHeading;
import org.komodo.eclipse.workspace.CoreConstants;
import org.komodo.eclipse.workspace.CorePlugin;
import org.komodo.eclipse.workspace.ui.UiConstants;
import org.komodo.eclipse.workspace.ui.editor.panels.IKObjectChangeListener;
import org.komodo.eclipse.workspace.ui.editor.panels.ObjectChangedEvent;
import org.komodo.eclipse.workspace.ui.editor.panels.ObjectEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.WorkspaceTreeViewerPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.ColumnEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.ModelEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.ParameterEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.ProcedureEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.TableEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.ViewEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.VirtualProcedureEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.AddNameObjectDialog;
import org.komodo.eclipse.workspace.ui.editor.panels.vdb.DataPolicyEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.vdb.FileEntryEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.vdb.PermissionEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.vdb.TranslatorEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.vdb.VdbEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.vdb.VdbImportEditorPanel;
import org.komodo.eclipse.workspace.util.RelationalIdentifier;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;

public class KNavigableEditorPage  extends KWorkspaceEditorPage implements IKObjectChangeListener, IDoubleClickListener  {
	
	// ====================================================
	// GENERAL WIDGETS
	Composite stackedEditorPanel;
	StackLayout stackedEditorsLayout;
	
	Collection<ObjectEditorPanel> editorPanels;
	WorkspaceTreeViewerPanel workspaceTreeViewerPanel;
	
	KomodoObject currentKO;
	Label locationLabel;
	

	protected KNavigableEditorPage(KWorkspaceEditor docEditor) {
		super(docEditor, UiConstants.PLUGIN_ID, "Komodo Workspace Navigable Editor");
	}

	/**
     * {@inheritDoc}
     * 
     * @see org.teiid.designer.extension.ui.editors.MedEditorPage#createBody(org.eclipse.swt.widgets.Composite,
     *      org.eclipse.ui.forms.widgets.FormToolkit)
     */
    @SuppressWarnings("unused")
    @Override
	protected void createBody(Composite body, FormToolkit toolkit) {
	    BODY: {
            GridLayoutFactory.swtDefaults().applyTo(body);
            GridDataFactory.swtDefaults().grab(true, true).applyTo(body);
	    }
	
	    final Section finalSection;
	
	    SECTION: {
	        Section section = toolkit.createSection(body, ExpandableComposite.NO_TITLE | ExpandableComposite.TITLE_BAR
	                | ExpandableComposite.LEFT_TEXT_CLIENT_ALIGNMENT);
	        section.setLayout(new GridLayout());
	        section.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
	        finalSection = section;
	    }
	    
	    final Composite finalContainer;
	    
        CONTAINER: {
	    	finalContainer = toolkit.createComposite(finalSection);
            GridLayoutFactory.swtDefaults().numColumns(1).equalWidth(false).applyTo(finalContainer);
            GridDataFactory.swtDefaults().grab(true, true).applyTo(finalContainer);
            finalSection.setClient(finalContainer);
            
            Composite buttonPanel = new Composite(finalContainer, SWT.NONE);
            GridLayoutFactory.swtDefaults().margins(1,  1).numColumns(4).equalWidth(false).applyTo(buttonPanel);

            
            Button upButton = new Button(buttonPanel, SWT.PUSH);
            upButton.setImage(CorePlugin.getDefault().getImage(CoreConstants.Images.UP));
            upButton.addSelectionListener(new SelectionListener() {
				
				@Override
				public void widgetSelected(SelectionEvent e) {
					KomodoObject currentKO = null;
					if( stackedEditorsLayout.topControl instanceof ObjectEditorPanel ) {
						currentKO = ((ObjectEditorPanel)stackedEditorsLayout.topControl).getTargetObject();
					}
					
					if( currentKO != null ) {
						KomodoObject parentKO = null;
						try {
							parentKO = currentKO.getParent(null);
						} catch (KException e1) {

						}
						selectEditorPanel(parentKO);
					} else {
						selectEditorPanel(null);
					}
				}
				
				@Override
				public void widgetDefaultSelected(SelectionEvent e) {
				}
			});
            
            Button treeViewButton = new Button(buttonPanel, SWT.PUSH);
            treeViewButton.setText("Top");
            GridDataFactory.fillDefaults().grab(false, true).applyTo(treeViewButton);
            treeViewButton.addSelectionListener(new SelectionListener() {
				
				@Override
				public void widgetSelected(SelectionEvent e) {
					selectEditorPanel(null);
				}
				
				@Override
				public void widgetDefaultSelected(SelectionEvent e) {
				}
			});
            
            Button newVdbButton = new Button(buttonPanel, SWT.PUSH);
            newVdbButton.setImage(CorePlugin.getDefault().getImage(CoreConstants.Images.NEW_VDB_ICON));
            GridDataFactory.fillDefaults().grab(false, true).applyTo(newVdbButton);
            newVdbButton.addSelectionListener(new SelectionListener() {
				
				@Override
				public void widgetSelected(SelectionEvent e) {
					AddNameObjectDialog dialog = new AddNameObjectDialog(getShell(), "Add VDB", "newVDB");
					dialog.open();
					
					if( dialog.getReturnCode() == IDialogConstants.OK_ID ) {
						String name = dialog.getName();
						try {
							UnitOfWork transaction = CorePlugin.getKEngine().getDefaultRepository().createTransaction("Add VDB", false, null);
							Vdb vdb = CorePlugin.getWorkspaceManager().createVdb(transaction, null, name, "extPath");
							transaction.commit();
						} catch (KException ex) {
							MessageDialog.openError(getShell(), "Error Adding VDB", ex.getMessage());
						} finally {
							if( stackedEditorsLayout.topControl == workspaceTreeViewerPanel ) {
								workspaceTreeViewerPanel.refresh();
							}
						}
					}
				}
				
				@Override
				public void widgetDefaultSelected(SelectionEvent e) {
				}
			});
            
            
        	locationLabel = new Label(finalContainer, SWT.NONE);
        	locationLabel.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_BLUE));
        	GridDataFactory.swtDefaults().grab(true, false).applyTo(locationLabel);
        	setCurrentLocation();
        }
        
	    EDITOR_PANEL: {
        	this.stackedEditorPanel = toolkit.createComposite(finalContainer, SWT.BORDER); // | SWT.H_SCROLL | SWT.V_SCROLL);
        	this.stackedEditorsLayout = new StackLayout();
        	this.stackedEditorsLayout.marginWidth = 0;
        	this.stackedEditorsLayout.marginHeight = 0;
        	this.stackedEditorPanel.setLayout(stackedEditorsLayout);
        	this.stackedEditorPanel.setLayoutData(new GridData(GridData.FILL_BOTH));
        	
        	workspaceTreeViewerPanel = new WorkspaceTreeViewerPanel(stackedEditorPanel, SWT.NONE);
        	
        	createEditorPanels(stackedEditorPanel);

        	this.stackedEditorsLayout.topControl = workspaceTreeViewerPanel;
        	this.stackedEditorPanel.layout();
        	
        	workspaceTreeViewerPanel.addDoubleClickListener(this);
				

        }
		
		selectEditorPanel(null);
	}
    
    private void selectEditorPanel(KomodoObject selectedNode) {
    	this.currentKO = selectedNode;
    	if( selectedNode == null ) {
    		this.stackedEditorsLayout.topControl = workspaceTreeViewerPanel;
    		workspaceTreeViewerPanel.setVisible(true);
    		this.stackedEditorPanel.layout();
    	} else {
    		boolean foundPanel = false;
    		int pType = RelationalIdentifier.getType(selectedNode);
    		
    		if( this.stackedEditorsLayout.topControl instanceof ObjectEditorPanel ) {
    			((ObjectEditorPanel)this.stackedEditorsLayout.topControl).clear();
    		}
    		
	    	for( ObjectEditorPanel panel : editorPanels ) {
	    		
	    		int panelType = panel.getNodeType();
	    		if( pType == panelType ) {
	    			//this.stackedEditorsLayout.topControl.setVisible(false);
	    			this.stackedEditorsLayout.topControl = panel;
	    			panel.setTargetObject(selectedNode);
	    			this.stackedEditorPanel.layout();
	    			panel.setVisible(true);
	    			foundPanel = true;
	    			break;
	    		}
	    	}
	    	if( !foundPanel) {
	    		this.stackedEditorsLayout.topControl = workspaceTreeViewerPanel;
	    		workspaceTreeViewerPanel.setVisible(true);
	    		this.stackedEditorPanel.layout();
	    	}
    	}
    	
    	setCurrentLocation();
    }

    private void createEditorPanels( Composite parent ) {
	    	editorPanels = new ArrayList<ObjectEditorPanel>();
	    	ObjectEditorPanel panel = new VdbEditorPanel(parent, SWT.NONE, RelationalIdentifier.VDB, this);
	    	panel.addDoubleClickListener(this);
	    	editorPanels.add(panel);
	    	panel = new ModelEditorPanel(parent, SWT.NONE, RelationalIdentifier.MODEL, this);
	    	panel.addDoubleClickListener(this);
	    	editorPanels.add(panel);
	    	panel = new TableEditorPanel(parent, SWT.NONE, RelationalIdentifier.TABLE, this);
	    	panel.addDoubleClickListener(this);
	    	editorPanels.add(panel);
	    	panel = new ProcedureEditorPanel(parent, SWT.NONE, RelationalIdentifier.PROCEDURE, this);
	    	panel.addDoubleClickListener(this);
	    	editorPanels.add(panel);
	    	panel = new ViewEditorPanel(parent, SWT.NONE, RelationalIdentifier.VIEW, this);
	    	panel.addDoubleClickListener(this);
	    	editorPanels.add(panel);
	    	panel = new VirtualProcedureEditorPanel(parent, SWT.NONE, RelationalIdentifier.VIRTUAL_PROCEDURE, this);
	    	panel.addDoubleClickListener(this);
	    	editorPanels.add(panel);
	    	panel = new ColumnEditorPanel(parent, SWT.NONE, RelationalIdentifier.COLUMN, this);
	    	panel.addDoubleClickListener(this);
	    	editorPanels.add(panel);
	    	panel = new ParameterEditorPanel(parent, SWT.NONE, RelationalIdentifier.PARAMETER, this);
	    	panel.addDoubleClickListener(this);
	    	editorPanels.add(panel);
	    	panel = new TranslatorEditorPanel(parent, SWT.NONE, RelationalIdentifier.TRANSLATOR, this);
	    	panel.addDoubleClickListener(this);
	    	editorPanels.add(panel);
	    	panel = new DataPolicyEditorPanel(parent, SWT.NONE, RelationalIdentifier.DATA_ROLE, this);
	    	panel.addDoubleClickListener(this);
	    	editorPanels.add(panel);
	    	panel = new FileEntryEditorPanel(parent, SWT.NONE, RelationalIdentifier.ENTRY, this);
	    	panel.addDoubleClickListener(this);
	    	editorPanels.add(panel);
	    	panel = new PermissionEditorPanel(parent, SWT.NONE, RelationalIdentifier.PERMISSION, this);
	    	panel.addDoubleClickListener(this);
	    	editorPanels.add(panel);
	    	panel = new VdbImportEditorPanel(parent, SWT.NONE, RelationalIdentifier.VDB_IMPORT, this);
	    	panel.addDoubleClickListener(this);
	    	editorPanels.add(panel);
    }

    private String setCurrentLocation() {
    	String location = "Workspace Location: ROOT";
    	if( currentKO != null ) {
    		location = "Workspace Location: " + currentKO.getAbsolutePath();
    	}
    	
    	locationLabel.setText(location);
    	locationLabel.pack();
    	
    	return location;
    }
	@Override
	protected void handlePropertyChanged(PropertyChangeEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
	protected void setResourceReadOnly(boolean readOnly) {
		// TODO Auto-generated method stub
		
	}

	@Override
	protected void updateAllMessages() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void documentChanged(ObjectChangedEvent event) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void doubleClick(DoubleClickEvent event) {
		KomodoObject dClickedObject = workspaceTreeViewerPanel.getCurrentSelection();
		if( this.stackedEditorsLayout.topControl != workspaceTreeViewerPanel) {
			for( ObjectEditorPanel panel : editorPanels ) {
				if( panel == this.stackedEditorsLayout.topControl ) {
					dClickedObject = panel.getCurrentSelection();
				}
			}
		}
		
		selectEditorPanel(dClickedObject);
	}

}