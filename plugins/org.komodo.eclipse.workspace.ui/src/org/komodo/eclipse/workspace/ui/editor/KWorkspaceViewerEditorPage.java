package org.komodo.eclipse.workspace.ui.editor;

import java.beans.PropertyChangeEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Properties;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
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
import org.eclipse.ui.forms.widgets.Section;
import org.komodo.eclipse.workspace.CoreConstants;
import org.komodo.eclipse.workspace.CorePlugin;
import org.komodo.eclipse.workspace.ui.UiConstants;
import org.komodo.eclipse.workspace.ui.actions.NewObjectAction;
import org.komodo.eclipse.workspace.ui.actions.NewObjectActionFactory;
import org.komodo.eclipse.workspace.ui.editor.dialogs.ExportToDynamicVdbDialog;
import org.komodo.eclipse.workspace.ui.editor.dialogs.ImportDynamicVdbDialog;
import org.komodo.eclipse.workspace.ui.editor.dialogs.ImportSchemaDialog;
import org.komodo.eclipse.workspace.ui.editor.panels.IKObjectChangeListener;
import org.komodo.eclipse.workspace.ui.editor.panels.ObjectChangedEvent;
import org.komodo.eclipse.workspace.ui.editor.panels.ObjectEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.ColumnEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.ModelEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.ParameterEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.ProcedureEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.TableEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.ViewEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.VirtualProcedureEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.AddNameObjectDialog;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ShowStringDialog;
import org.komodo.eclipse.workspace.ui.editor.panels.vdb.DataPolicyEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.vdb.DataSourceEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.vdb.FileEntryEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.vdb.PermissionEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.vdb.TranslatorEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.vdb.VdbEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.vdb.VdbImportEditorPanel;
import org.komodo.eclipse.workspace.ui.explorer.WorkspaceTreeViewer;
import org.komodo.eclipse.workspace.util.RelationalIdentifier;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.ImportType;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.importer.ddl.DdlImporter;
import org.komodo.importer.vdb.VdbImporter;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.KException;
import org.komodo.spi.constants.ExportConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon.CreateTable;

public class KWorkspaceViewerEditorPage extends KWorkspaceEditorPage implements IKObjectChangeListener  {
	
	// ====================================================
	// GENERAL WIDGETS
	Composite stackedEditorPanel;
	StackLayout stackedEditorsLayout;
	
	Collection<ObjectEditorPanel> editorPanels;
	Composite noSelectionEditorPanel;
	
	WorkspaceTreeViewer wsTreeViewer;
	IAction refreshTreeAction;
	IAction deleteObjectAction;
	

	protected KWorkspaceViewerEditorPage(KWorkspaceEditor docEditor) {
		super(docEditor, UiConstants.PLUGIN_ID, "Komodo Workspace Object Editor");
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
            GridLayoutFactory.swtDefaults().numColumns(2).equalWidth(false).applyTo(finalContainer);
            GridDataFactory.swtDefaults().grab(true, true).applyTo(finalContainer);
            finalSection.setClient(finalContainer);
            
            Composite buttonPanel = new Composite(finalContainer, SWT.NONE);
            GridLayoutFactory.swtDefaults().margins(1,  1).numColumns(4).equalWidth(false).applyTo(buttonPanel);
            GridDataFactory.swtDefaults().span(2, 1).applyTo(buttonPanel);
	        
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
						}
					}
				}
				
				@Override
				public void widgetDefaultSelected(SelectionEvent e) {
				}
			});
            
            Button importSchemaButton =  new Button(buttonPanel, SWT.PUSH);
            importSchemaButton.setImage(CorePlugin.getDefault().getImage(CoreConstants.Images.IMPORT_MODEL_ICON));
            GridDataFactory.fillDefaults().grab(false, true).applyTo(importSchemaButton);
            importSchemaButton.addSelectionListener(new SelectionListener() {
				
				@Override
				public void widgetSelected(SelectionEvent e) {
					ImportSchemaDialog dialog = new ImportSchemaDialog(getShell(), "Import Schema DDL File");
					dialog.open();
					
					if( dialog.getReturnCode() == IDialogConstants.OK_ID ) {
						File selectedFile = dialog.getFile();
						try {
							UnitOfWork transaction = CorePlugin.getKEngine().getDefaultRepository().createTransaction("Add Model", false, null);
							DdlImporter importer = new DdlImporter(CorePlugin.getKEngine().getDefaultRepository(), transaction);
					    	ImportOptions importOptions = new ImportOptions();
					    	importOptions.setImportType(ImportType.MODEL);
					    	importOptions.setOption(OptionKeys.NAME, dialog.getModelName());

					    	ImportMessages importMessages = new ImportMessages();
					    	
					    	importer.importDdl(selectedFile, importOptions, importMessages);
					    	
							transaction.commit();
							
							if( importMessages.hasError() ) {
								MessageDialog.openError(getShell(), "Schema Import Errors", importMessages.errorMessagesToString());
							}
						} catch (KException ex) {
							MessageDialog.openError(getShell(), "Error Importing Schema", ex.getMessage());
						}
					}
				}
				
				@Override
				public void widgetDefaultSelected(SelectionEvent e) {
				}
			});
            
            Button importVdbButton =  new Button(buttonPanel, SWT.PUSH);
            importVdbButton.setImage(CorePlugin.getDefault().getImage(CoreConstants.Images.IMPORT_VDB_ICON));
            GridDataFactory.fillDefaults().grab(false, true).applyTo(importVdbButton);
            importVdbButton.addSelectionListener(new SelectionListener() {
				
				@Override
				public void widgetSelected(SelectionEvent e) {
					ImportDynamicVdbDialog dialog = new ImportDynamicVdbDialog(getShell(), "Import VDB XML File");
					dialog.open();
					
					if( dialog.getReturnCode() == IDialogConstants.OK_ID ) {
						File selectedFile = dialog.getFile();
						try {
							UnitOfWork transaction = CorePlugin.getKEngine().getDefaultRepository().createTransaction("Add VDB", false, null);
							VdbImporter importer = new VdbImporter(CorePlugin.getKEngine().getDefaultRepository(), transaction);
					    	ImportOptions importOptions = new ImportOptions();
					    	importOptions.setImportType(ImportType.VDB);
					    	importOptions.setOption(OptionKeys.NAME,  dialog.getVdbName()); // selectedFile.getName()); //
					    	importOptions.setOption(OptionKeys.VDB_FILE_PATH, selectedFile.getAbsolutePath());

					    	ImportMessages importMessages = new ImportMessages();
					    	
					    	importer.importVdb(selectedFile, importOptions, importMessages);
					    	
							transaction.commit();
							
							if( importMessages.hasError() ) {
								MessageDialog.openError(getShell(), "Schema Import Errors", importMessages.errorMessagesToString());
							}
						} catch (KException ex) {
							MessageDialog.openError(getShell(), "Error Importing Schema", ex.getMessage());
						}
					}
				}
				
				@Override
				public void widgetDefaultSelected(SelectionEvent e) {
				}
			});
            
        }

	    TREE: {
            //Tree theTree = toolkit.createTree(finalContainer, SWT.BORDER | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
        	this.wsTreeViewer = new WorkspaceTreeViewer(finalContainer, SWT.BORDER | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
        	this.wsTreeViewer.getControl().setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

            final MenuManager actionMenu = new MenuManager();
            this.wsTreeViewer.getControl().setMenu(actionMenu.createContextMenu(finalContainer));
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
							IAction showVdbContents = new Action("Show VDB Contents") {
								@Override
								public void run() {
									try {
										Properties props = new Properties();
										props.put( ExportConstants.USE_TABS_PROP_KEY, true);

										String ddlXmlString = ((Vdb) node) .export(null, props);
										if (ddlXmlString == null || ddlXmlString .isEmpty()) {
											ddlXmlString = "< No VDB contents >";
										}
										ShowStringDialog dialog = new ShowStringDialog( getShell(), "Dynamic VDB Contents", ddlXmlString);
										dialog.open();
									} catch (KException e) {
										// TODO Auto-generated catch
										// block
										e.printStackTrace();
									}
								}
							};
   	   						actionMenu.add(showVdbContents);
   							IAction exportDynamicVdbAction = new Action("Export VDB") {
      							 @Override
      						     public void run() {
      								 try {
    									Properties props = new Properties();
    									props.put(ExportConstants.USE_TABS_PROP_KEY, true);
    									
    									String ddlXmlString = ((Vdb)node).export(null, props);
      									if( ddlXmlString == null || ddlXmlString.isEmpty() ) {
      										ddlXmlString = "< No VDB contents >";
      									}
      									ExportToDynamicVdbDialog dialog = new ExportToDynamicVdbDialog(getShell(), "Export Dynamic VDB", ddlXmlString);
      									dialog.open();
      								} catch (KException e) {
      									// TODO Auto-generated catch block
      									e.printStackTrace();
      								}
      							 }
       						};
       						actionMenu.add(exportDynamicVdbAction);
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
              		
                    	selectEditorPanel((KomodoObject)sel.getFirstElement());
                    } else {
                    	selectEditorPanel(null);
                    }

                }
            });
//            this.wsTreeViewer.setInput(getRootNode());
        }
        
	    EDITOR_PANEL: {
        	this.stackedEditorPanel = toolkit.createComposite(finalContainer, SWT.BORDER); // | SWT.H_SCROLL | SWT.V_SCROLL);
        	this.stackedEditorsLayout = new StackLayout();
        	this.stackedEditorsLayout.marginWidth = 0;
        	this.stackedEditorsLayout.marginHeight = 0;
        	this.stackedEditorPanel.setLayout(stackedEditorsLayout);
        	this.stackedEditorPanel.setLayoutData(new GridData(GridData.FILL_BOTH));
        	
        	noSelectionEditorPanel = new Composite(stackedEditorPanel, SWT.NONE);
        	Label emtpyLabel = new Label(noSelectionEditorPanel, SWT.NONE);
        	emtpyLabel.setText("No Selection");
        	
        	createEditorPanels(stackedEditorPanel);

        	this.stackedEditorsLayout.topControl = noSelectionEditorPanel;
        }
        
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
		
//		Composite bottomPanel = toolkit.createComposite(finalContainer, SWT.BORDER); // | SWT.H_SCROLL | SWT.V_SCROLL);
	}
    
    private void selectEditorPanel(KomodoObject selectedNode) {
    	if( selectedNode == null ) {
    		this.stackedEditorsLayout.topControl = noSelectionEditorPanel;
    		noSelectionEditorPanel.setVisible(true);
    		this.stackedEditorPanel.layout();
    	} else {
    		boolean foundPanel = false;
    		int pType = RelationalIdentifier.getType(selectedNode);
    		
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
	    		this.stackedEditorsLayout.topControl = noSelectionEditorPanel;
	    		noSelectionEditorPanel.setVisible(true);
	    		this.stackedEditorPanel.layout();
	    	}
    	}
    }

    private void createEditorPanels( Composite parent ) {
	    	editorPanels = new ArrayList<ObjectEditorPanel>();
//	    	ObjectEditorPanel panel = new VdbEditorPanel(parent, SWT.NONE, RelationalIdentifier.VDB, this);
	    	editorPanels.add(new VdbEditorPanel(parent, SWT.NONE, RelationalIdentifier.VDB, this));
	    	editorPanels.add(new ModelEditorPanel(parent, SWT.NONE, RelationalIdentifier.MODEL, this));
	    	editorPanels.add(new TableEditorPanel(parent, SWT.NONE, RelationalIdentifier.TABLE, this));
	    	editorPanels.add(new ColumnEditorPanel(parent, SWT.NONE, RelationalIdentifier.COLUMN, this));
	    	editorPanels.add(new ProcedureEditorPanel(parent, SWT.NONE, RelationalIdentifier.STORED_PROCEDURE, this));
	    	editorPanels.add(new ViewEditorPanel(parent, SWT.NONE, RelationalIdentifier.VIEW, this));
	    	editorPanels.add(new VirtualProcedureEditorPanel(parent, SWT.NONE, RelationalIdentifier.VIRTUAL_PROCEDURE, this));
	    	editorPanels.add(new ParameterEditorPanel(parent, SWT.NONE, RelationalIdentifier.PARAMETER, this));
	    	editorPanels.add(new TranslatorEditorPanel(parent, SWT.NONE, RelationalIdentifier.TRANSLATOR, this));
	    	editorPanels.add(new DataPolicyEditorPanel(parent, SWT.NONE, RelationalIdentifier.DATA_ROLE, this));
	    	editorPanels.add(new FileEntryEditorPanel(parent, SWT.NONE, RelationalIdentifier.ENTRY, this));
	    	editorPanels.add(new PermissionEditorPanel(parent, SWT.NONE, RelationalIdentifier.PERMISSION, this));
	    	editorPanels.add(new VdbImportEditorPanel(parent, SWT.NONE, RelationalIdentifier.VDB_IMPORT, this));
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
	public void dispose() {
		if( wsTreeViewer != null ) {
			wsTreeViewer.close();
		}
		super.dispose();
	}

}