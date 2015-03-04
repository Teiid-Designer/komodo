/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.eclipse.workspace.ui.editor.panels;

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
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;
import org.komodo.eclipse.workspace.CoreConstants;
import org.komodo.eclipse.workspace.CorePlugin;
import org.komodo.eclipse.workspace.ui.Messages;
import org.komodo.eclipse.workspace.ui.actions.NewObjectAction;
import org.komodo.eclipse.workspace.ui.actions.NewObjectActionFactory;
import org.komodo.eclipse.workspace.ui.common.util.CoreStringUtil;
import org.komodo.eclipse.workspace.ui.common.util.ObjectEditorUtils;
import org.komodo.eclipse.workspace.ui.common.util.StyledTextEditor;
import org.komodo.eclipse.workspace.ui.common.util.UILabelUtil;
import org.komodo.eclipse.workspace.ui.common.util.UiLabelConstants;
import org.komodo.eclipse.workspace.ui.common.util.WidgetFactory;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.AddNameObjectDialog;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.RelationalObjectEditorUtil;
import org.komodo.eclipse.workspace.ui.explorer.WorkspaceTreeViewer;
import org.komodo.eclipse.workspace.util.RelationalIdentifier;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.RelationalObject;
import org.komodo.relational.model.Table;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

public class ObjectEditorPanel extends Composite {
	private static final String EMPTY_STRING = CoreStringUtil.Constants.EMPTY_STRING;
	private KomodoObject targetObject;
	private int type;
	
	private Text nameText;
	private Text pathText;
	private Label nodeTypeLabel;
	

	private TabFolder tabFolder;
	private TabItem treeViewTab;
	WorkspaceTreeViewer wsTreeViewer;
	private TabItem descriptionTab;
    private StyledTextEditor descriptionTextEditor;
	IAction refreshTreeAction;
	IAction deleteObjectAction;
	Button applyDescriptionButton;
	
	private IKObjectChangeListener listener;
	
	private boolean showTree = false;
	private boolean synching = false;
	
	public ObjectEditorPanel(Composite parent, int style, int nodeType, IKObjectChangeListener docChangedListener) {
		this(parent, style, nodeType, docChangedListener, false);
	}
	
	public ObjectEditorPanel(Composite parent, int style, int nodeType, IKObjectChangeListener docChangedListener, boolean showTree) {
		super(parent, style);
		this.type = nodeType;
		this.listener = docChangedListener;
		this.showTree = showTree;
		
        GridLayout twoColumnLayout = new GridLayout(2, false);
        setLayout(twoColumnLayout);
        setLayoutData(new GridData(GridData.FILL_BOTH));
        
        addCommonStuff();

		tabFolder =new TabFolder(this, SWT.TOP | SWT.BORDER);
        GridDataFactory.fillDefaults().span(2, 1).grab(true,  true).applyTo(tabFolder);
		
        if( showTree ) {
        	createTreeViewTab();
        }
		
        addCustomPanel();
        
        createDescriptionTab(getTabFolder());
	}
	
	protected TabFolder getTabFolder() {
		return this.tabFolder;
	}

	public int getNodeType() {
		return this.type;
	}
	
	public String getNodeTypeName() {
		return RelationalIdentifier.getTypeDisplayName(this.type);
	}
	
	public KomodoObject getTargetObject() {
		return this.targetObject;
	}
	
	public void setTargetObject(KomodoObject targetObject) {
		this.targetObject = targetObject;
		if( type == RelationalIdentifier.MODEL ) {
			
		}
	}
	
	private void createDescriptionTab(TabFolder tabFolder) {
        // build the SELECT tab
		Composite thePanel = createDescriptionPanel(tabFolder);

        this.descriptionTab = new TabItem(tabFolder, SWT.NONE);
        this.descriptionTab.setControl(thePanel);
        this.descriptionTab.setText(Messages.descriptionLabel);
	}
	
    private Composite createDescriptionPanel(Composite parent) {
        Composite thePanel = WidgetFactory.createPanel(parent, SWT.NONE, 1, 3);
        Composite buttonPanel = new Composite(thePanel, SWT.NONE);
        GridLayoutFactory.swtDefaults().margins(1,  1).numColumns(4).equalWidth(false).applyTo(buttonPanel);

        
        applyDescriptionButton = new Button(buttonPanel, SWT.PUSH);
        applyDescriptionButton.setText("Apply");
        applyDescriptionButton.addSelectionListener(new SelectionListener() {
			
			@Override
			public void widgetSelected(SelectionEvent e) {
				boolean changed = false;
				try {
					changed = RelationalObjectEditorUtil.setDescription(null, getTargetObject(), descriptionTextEditor.getText());
				} catch (Exception e1) {
					e1.printStackTrace();
				}
				if( changed ) {
					applyDescriptionButton.setEnabled(false);
            		applyDescriptionButton.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_BLACK));
				}
				
			}
			
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});
        applyDescriptionButton.setEnabled(false);
        
        GridLayoutFactory.fillDefaults().numColumns(2).equalWidth(true).margins(5, 5).applyTo(thePanel);
        GridDataFactory.fillDefaults().grab(true, true).applyTo(thePanel);

        final Group descGroup = WidgetFactory.createGroup(thePanel, UILabelUtil.getLabel(UiLabelConstants.LABEL_IDS.DESCRIPTION), GridData.FILL_BOTH, 3);
        descriptionTextEditor = new StyledTextEditor(descGroup, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL | SWT.WRAP | SWT.BORDER);
        GridDataFactory.fillDefaults().grab(true, true).hint(SWT.DEFAULT, 120).minSize(SWT.DEFAULT, 30).applyTo(descriptionTextEditor.getTextWidget());
        descriptionTextEditor.setText(""); //$NON-NLS-1$
        descriptionTextEditor.getTextWidget().addModifyListener(new ModifyListener() {

            @Override
            public void modifyText(ModifyEvent e) {
            	if( ! descriptionTextEditor.getText().isEmpty() ) {
            		applyDescriptionButton.setEnabled(true);
            		applyDescriptionButton.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_RED));
            	}
            }
        });

        return thePanel;
    }
	
	@SuppressWarnings("unused")
	private void addCommonStuff() {
		// Add label for Node Type
		TYPE : {
			WidgetFactory.createLabel(this, SWT.NONE, "Type :");
			nodeTypeLabel = new Label(this, SWT.NONE);
			nodeTypeLabel.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_BLUE));
			nodeTypeLabel.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
			if( type > -1 ) {
				nodeTypeLabel.setText(RelationalIdentifier.getTypeDisplayName(type));
			}
		}
		
		
		// Add label and text field for "Name"
		NAME : {
			WidgetFactory.createLabel(this, "Name :");
			nameText = new Text(this, SWT.BORDER);
			nameText.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_BLUE));
			nameText.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
			nameText.setEditable(true);
			nameText.addModifyListener(new ModifyListener() {
	    		public void modifyText( final ModifyEvent event ) {
	    			if( targetObject != null && !synching) {
	    				//targetObject.setName(null, nameText.getText());
		        		handleInfoChanged();
	    			}
	    		}
	    	});
		}
		
		// Add label and text field for "Path"
		NAME : {
			WidgetFactory.createLabel(this, "Path :");
			pathText = new Text(this, SWT.BORDER);
			pathText.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_BLUE));
			pathText.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
			pathText.setEditable(false);
		}
	}
	
	private void createTreeViewTab() {
        // build the SELECT tab
		Composite thePanel = createTreeViewPanel(getTabFolder());

        this.treeViewTab = new TabItem(tabFolder, SWT.NONE);
        this.treeViewTab.setControl(thePanel);
        this.treeViewTab.setText("Contents");
	}
	
	private Composite createTreeViewPanel(Composite parent) {
        Composite thePanel = WidgetFactory.createPanel(parent, SWT.NONE, 1, 1);
        GridLayoutFactory.fillDefaults().numColumns(1).margins(5, 5).applyTo(thePanel);
        GridDataFactory.fillDefaults().grab(true, true).applyTo(thePanel);
    	this.wsTreeViewer = new WorkspaceTreeViewer(thePanel, SWT.BORDER | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
//        Object item1 = wsTreeViewer.getTree().getItem(0);
//        if( item1 != null ) {
//      	  
//        }
    	this.wsTreeViewer.getControl().setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        final MenuManager actionMenu = new MenuManager();
        this.wsTreeViewer.getControl().setMenu(actionMenu.createContextMenu(parent));
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
					} else if( RelationalIdentifier.isType(node, CoreConstants.RelationalType.MODEL )) {
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
					}
					actionMenu.add(new Separator());
					actionMenu.add(deleteObjectAction);
					actionMenu.add(new Separator());
					actionMenu.add(refreshTreeAction);
          		
//                	selectEditorPanel((KomodoObject)sel.getFirstElement());
                }

            }
        });
        
        refreshTreeAction = new Action("Refresh", CorePlugin.getDefault().getImageDescriptor(CoreConstants.Images.REFRESH)) {
			 @Override
	        public void run() {
				 wsTreeViewer.refresh();
	        }
		};
		
        deleteObjectAction = new Action("Delete", CorePlugin.getDefault().getImageDescriptor(CoreConstants.Images.DELETE)) {
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
		
		this.wsTreeViewer.setInput(getTargetObject());
		
		return thePanel;
	}
	
	public void addCustomPanel() {
		// NOOP  Override to add other stuff
	}
	
	public void handleInfoChanged() {
		if( !synching) {

			listener.documentChanged(new ObjectChangedEvent(getTargetObject()));
		}
	}

	@Override
	public void setVisible(boolean visible) {
		super.setVisible(visible);
		
		if( visible ) {
			syncWithTarget();
		}
	}
	
	protected boolean synching() {
		return this.synching;
	}
	
	protected void setSynching(boolean synching) {
		this.synching = synching;
	}
	
	protected void syncWithTarget() {
		this.synching = true;
		
		// TODO: FIX below
		if( isVisible() && getTargetObject() != null ) {
			if( showTree ) {
				this.wsTreeViewer.setInput(getTargetObject());
			}
			
			String oldText = nameText.getText();
			String newName = oldText;
			try {
				newName = getTargetObject().getName(null);
			} catch (KException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			setTextValue(nameText, newName);
			
			setTextValue(pathText, getTargetObject().getAbsolutePath());
			
			String desc = ObjectEditorUtils.getDescription(getTargetObject());
			if( desc == null ) desc = StringUtils.EMPTY_STRING;
			descriptionTextEditor.setText(desc);

		} else {
			nameText.setText("");
		}
		this.synching = false;
	}
	
	protected void setTextValue(Text textWidget, String value) {
		if( value == null ) {
			textWidget.setText(EMPTY_STRING);
		} else {
			textWidget.setText(value);
		}
	}
	
	public KomodoObject getCurrentSelection() {
		if( showTree ) {
	        IStructuredSelection sel = (IStructuredSelection)wsTreeViewer.getSelection();
	
	        if (sel.size() == 1 && sel.getFirstElement() instanceof KomodoObject ) {
	        	return (KomodoObject)sel.getFirstElement();
	        }
		}
        
        return null;
	}
	
	
	public void clear() {
		if( showTree ) {
			this.wsTreeViewer.getTree().clearAll(true);
		}
		this.nameText.setText(StringUtils.EMPTY_STRING);
		this.pathText.setText(StringUtils.EMPTY_STRING);
		this.descriptionTextEditor.setText(StringUtils.EMPTY_STRING);
		this.targetObject = null;
	}
	
	protected void validate() {
		// TODO: Check values in UI
	}

	public void addDoubleClickListener(IDoubleClickListener listener) {
		if( showTree ) {
			wsTreeViewer.addDoubleClickListener(listener);
		}
	}
	
}
