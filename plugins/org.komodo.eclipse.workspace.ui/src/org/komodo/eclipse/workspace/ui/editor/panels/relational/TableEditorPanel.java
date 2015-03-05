package org.komodo.eclipse.workspace.ui.editor.panels.relational;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;
import org.komodo.eclipse.workspace.CoreConstants;
import org.komodo.eclipse.workspace.CorePlugin;
import org.komodo.eclipse.workspace.ui.Messages;
import org.komodo.eclipse.workspace.ui.common.table.TableViewerBuilder;
import org.komodo.eclipse.workspace.ui.common.util.UILabelUtil;
import org.komodo.eclipse.workspace.ui.common.util.UiLabelConstants;
import org.komodo.eclipse.workspace.ui.common.util.WidgetFactory;
import org.komodo.eclipse.workspace.ui.editor.panels.KObjectChangeListener;
import org.komodo.eclipse.workspace.ui.editor.panels.ObjectEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.AddNameObjectDialog;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ColumnDatatypeEditingSupport;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ColumnNameEditingSupport;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ColumnWidthEditingSupport;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ElementChangedEvent;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ViewerElementChangedListener;
import org.komodo.eclipse.workspace.util.RelationalIdentifier;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Table;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

public class TableEditorPanel extends ObjectEditorPanel implements ViewerElementChangedListener {

	private TabItem generalPropertiesTab;
	private TabItem columnsTab;
	
	// table property widgets
	private Button materializedCB, supportsUpdateCB;
	private Button findTableReferenceButton;
	private Label materializedTableLabel;
	private Text cardinalityText, materializedTableText, nameInSourceText;
	
	private TableViewerBuilder columnsViewer;
	private Button addColumnButton, deleteColumnButton, upColumnButton, downColumnButton;
	
	private ColumnWidthEditingSupport columnWidthEditSupport;
	private ColumnNameEditingSupport columnNameEditSupport;
	private ColumnDatatypeEditingSupport columnTypeEditSupport;
	
	public TableEditorPanel(Composite parent, int style, int nodeType,
			KObjectChangeListener objectChangeListener) {
		super(parent, style, nodeType, objectChangeListener, true);
	}
	
	public void addCustomPanel() {
//		createColumnsTab(getTabFolder());
		createGeneralPropertiesTab(getTabFolder());
	}
	
	private void createGeneralPropertiesTab(TabFolder folderParent) {
        // build the SELECT tab
		Composite thePanel = createPropertiesPanel(folderParent);

        this.generalPropertiesTab = new TabItem(folderParent, SWT.NONE);
        this.generalPropertiesTab.setControl(thePanel);
        this.generalPropertiesTab.setText(Messages.propertiesLabel);
//        this.generalPropertiesTab.setImage(RelationalUiUtil.getRelationalImage(TYPES.TABLE, ModelType.PHYSICAL, Status.OK_STATUS));
	}
	
	
	@SuppressWarnings("unused")
	private Composite createPropertiesPanel(Composite parent) {
		Composite thePanel = WidgetFactory.createPanel(parent, SWT.NONE, 1, 3);
		GridLayoutFactory.fillDefaults().margins(5, 5).applyTo(thePanel);
		GridDataFactory.fillDefaults().grab(true, true).applyTo(thePanel);

		Composite cardinalityPanel = new Composite(thePanel, SWT.NONE);
        GridDataFactory.fillDefaults().grab(true, false).applyTo(cardinalityPanel);
        GridLayoutFactory.fillDefaults().numColumns(2).equalWidth(false).applyTo(cardinalityPanel);

        NAME_IN_SOURCE: {
            Label label = new Label(cardinalityPanel, SWT.NONE);
            label.setText("Name In Source");
            
            this.nameInSourceText =  new Text(cardinalityPanel, SWT.BORDER | SWT.SINGLE);
            this.nameInSourceText.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_BLUE));
            GridDataFactory.fillDefaults().grab(true, false).applyTo(this.nameInSourceText);
            this.nameInSourceText.addModifyListener(new ModifyListener() {
        		@Override
    			public void modifyText( final ModifyEvent event ) {
//        			if( isSynchronizing() )
//        			    return;

        			if( !nameInSourceText.getText().isEmpty()) {
        			    try {	
        			        String nis = nameInSourceText.getText();
        			        if( nis != null && nis.isEmpty() ) {
        			        	nis = null;
        			        }
        			        getTable().setNameInSource(null, nis);
        			    } catch (KException ex) {
        			        MessageDialog.openError(
        			                                getShell(), 
        			                                "Problem setting name in source for table",  
        			                                ex.getMessage()); 
    	        				    return;
        			    }
        			}
        		}
            });
        }

        Label label = new Label(cardinalityPanel, SWT.NONE);
        label.setText("Cardinality");
        
        this.cardinalityText =  new Text(cardinalityPanel, SWT.BORDER | SWT.SINGLE);
        this.cardinalityText.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_BLUE));
        GridDataFactory.fillDefaults().grab(true, false).applyTo(this.cardinalityText);
        this.cardinalityText.addModifyListener(new ModifyListener() {
    		@Override
			public void modifyText( final ModifyEvent event ) {

    			if( !cardinalityText.getText().isEmpty()) {
    			    try {	
    			        int value = Integer.parseInt(cardinalityText.getText());
    			        getTable().setCardinality(null, value);
    			        handleInfoChanged();
    			    } catch (NumberFormatException | KException ex) {
    			        MessageDialog.openError(
    			                                getShell(), 
    			                                "Problem setting cardinality for table", //Messages.cardinalityErrorTitle, 
    			                                "Must be an interger"); //Messages.cardinalityMustBeAnInteger);
	        				    return;
    			    }
    			}
    		}
        });

        Composite checkButtonPanel = new Composite(thePanel, SWT.NONE);
        GridDataFactory.fillDefaults().grab(true, false).applyTo(checkButtonPanel);
        GridLayoutFactory.fillDefaults().numColumns(2).applyTo(checkButtonPanel);

        this.supportsUpdateCB = new Button(checkButtonPanel, SWT.CHECK | SWT.RIGHT);
        GridDataFactory.fillDefaults().align(SWT.LEFT, SWT.CENTER).applyTo(this.supportsUpdateCB);
        this.supportsUpdateCB.setText(Messages.supportsUpdateLabel);
        this.supportsUpdateCB.addSelectionListener(new SelectionAdapter() {
            /**            		
             * {@inheritDoc}
             * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected( SelectionEvent e ) {
            	try {
					getTable().setUpdatable(null, supportsUpdateCB.getSelection());
				} catch (KException e1) {
					MessageDialog.openError(
                            getShell(), 
                            "Problem setting supports update for table",  e1.getMessage());
				}
            }
        });

        // TODO: if (isPhysicalModel()) {
            this.materializedCB = new Button(thePanel, SWT.CHECK | SWT.RIGHT);
            GridDataFactory.fillDefaults().align(SWT.LEFT, SWT.CENTER).applyTo(this.materializedCB);
            this.materializedCB.setText(Messages.materializedLabel);
            this.materializedCB.addSelectionListener(new SelectionAdapter() {
                /**            		
                 * {@inheritDoc}
                 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
                 */
                @Override
                public void widgetSelected(SelectionEvent e) {
                	// TODO:
//                    getRelationalReference().setMaterialized(materializedCB.getSelection());
//                    if (!materializedCB.getSelection()) {
//                        getRelationalReference().setMaterializedTable(null);
//                    }
                    handleInfoChanged();
                }
            });

            Composite materializedPanel = new Composite(thePanel, SWT.NONE);
            GridDataFactory.fillDefaults().grab(true, false).applyTo(materializedPanel);
            GridLayoutFactory.fillDefaults().numColumns(3).applyTo(materializedPanel);

            materializedTableLabel = new Label(materializedPanel, SWT.NONE | SWT.RIGHT);
            materializedTableLabel.setText(Messages.tableReferenceLabel);
            GridDataFactory.fillDefaults().align(SWT.RIGHT, SWT.CENTER).applyTo(materializedTableLabel);

            this.materializedTableText = new Text(materializedPanel, SWT.BORDER | SWT.SINGLE);
            this.materializedTableText.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_BLUE));
            GridDataFactory.fillDefaults().grab(true, false).applyTo(this.materializedTableText);

            this.findTableReferenceButton = new Button(materializedPanel, SWT.PUSH);
            this.findTableReferenceButton.setText(UILabelUtil.getLabel(UiLabelConstants.LABEL_IDS.ELIPSIS));
            GridDataFactory.fillDefaults().hint(30, SWT.DEFAULT).applyTo(this.findTableReferenceButton);
            this.findTableReferenceButton.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {

                }
            });
//        }

        return thePanel;
	}
	
	
	
	private void createColumnsTab(TabFolder folderParent) {
        Composite thePanel = createColumnTableGroup(folderParent);

        this.columnsTab = new TabItem(folderParent, SWT.NONE);
        this.columnsTab.setControl(thePanel);
        this.columnsTab.setText(Messages.columnsLabel);
        this.columnsTab.setImage(RelationalIdentifier.getImage(CoreConstants.RelationalType.COLUMN));
	}
	
	private Composite createColumnTableGroup(Composite parent) {
	  	
	    Composite thePanel = WidgetFactory.createPanel(parent, SWT.NONE, 1, 1);
        GridLayoutFactory.fillDefaults().margins(5, 5).applyTo(thePanel);
        GridDataFactory.fillDefaults().grab(true, true).applyTo(thePanel);

        Composite buttonPanel = WidgetFactory.createPanel(thePanel, SWT.NONE, 1, 4);
        GridLayoutFactory.fillDefaults().numColumns(4).applyTo(buttonPanel);
        GridDataFactory.fillDefaults().grab(true, false).applyTo(buttonPanel);

    	addColumnButton = new Button(buttonPanel, SWT.PUSH);
    	addColumnButton.setText(UILabelUtil.getLabel(UiLabelConstants.LABEL_IDS.ADD));
    	GridDataFactory.fillDefaults().applyTo(addColumnButton);
    	addColumnButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				// Launch Name dialog
				AddNameObjectDialog dialog = new AddNameObjectDialog(getShell(), "Add Column", "newColumn");
				dialog.open();
				
				if( dialog.getReturnCode() == IDialogConstants.OK_ID ) {
					String name = dialog.getName();
					try {
						UnitOfWork transaction = CorePlugin.getKEngine().getDefaultRepository().createTransaction("Add Column", false, null);
						KomodoObject newChild = getTargetObject().addChild(transaction, name, "nt:unstructured"); //CreateTable.TABLE_ELEMENT);
						newChild.addDescriptor(transaction, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
						transaction.commit();
					} catch (KException ex) {
						MessageDialog.openError(getShell(), "Error Adding Column", ex.getMessage());
					} finally {
						refreshColumnsViewer();
					}
					
				}
			}
    		
		});
    	
    	deleteColumnButton = new Button(buttonPanel, SWT.PUSH);
    	deleteColumnButton.setText(UILabelUtil.getLabel(UiLabelConstants.LABEL_IDS.DELETE));
    	GridDataFactory.fillDefaults().applyTo(deleteColumnButton);
    	deleteColumnButton.setEnabled(false);
    	deleteColumnButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				KomodoObject column = null;
				
				IStructuredSelection selection = (IStructuredSelection)columnsViewer.getSelection();
				for( Object obj : selection.toArray()) {
					if( RelationalIdentifier.isType((KomodoObject)obj, CoreConstants.RelationalType.COLUMN) ) {
						column =  (KomodoObject) obj;
						break;
					}
				}
				if( column != null ) {
					try {
						UnitOfWork transaction = CorePlugin.getKEngine().getDefaultRepository().createTransaction("Add Column", false, null);
						getTargetObject().removeChild(transaction, column.getName(transaction));
						transaction.commit();
					} catch (KException ex) {
						MessageDialog.openError(getShell(), "Error Adding Column", ex.getMessage());
					} finally {
						refreshColumnsViewer();
					}
					deleteColumnButton.setEnabled(false);
					refreshColumnsViewer();
				}
			}
    		
		});
    	
    	upColumnButton = new Button(buttonPanel, SWT.PUSH);
    	upColumnButton.setText(UILabelUtil.getLabel(UiLabelConstants.LABEL_IDS.MOVE_UP));
    	GridDataFactory.fillDefaults().applyTo(upColumnButton);
    	upColumnButton.setEnabled(false);
    	upColumnButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				Column info = null;
				
				IStructuredSelection selection = (IStructuredSelection)columnsViewer.getSelection();
				for( Object obj : selection.toArray()) {
					if( obj instanceof Column ) {
						info =  (Column) obj;
						break;
					}
				}
				if( info != null ) {
					int selectedIndex = columnsViewer.getTable().getSelectionIndex();
					 // TODO: getRelationalReference().moveColumnUp(info);
					refreshColumnsViewer();
					columnsViewer.getTable().select(selectedIndex-1);
//					downColumnButton.setEnabled(getRelationalReference().canMoveColumnDown(info));
//					upColumnButton.setEnabled(getRelationalReference().canMoveColumnUp(info));
					
				}
			}
    		
		});
    	
    	downColumnButton = new Button(buttonPanel, SWT.PUSH);
    	downColumnButton.setText(UILabelUtil.getLabel(UiLabelConstants.LABEL_IDS.MOVE_DOWN));
    	GridDataFactory.fillDefaults().applyTo(downColumnButton);
    	downColumnButton.setEnabled(false);
    	downColumnButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				Column info = null;
				
				IStructuredSelection selection = (IStructuredSelection)columnsViewer.getSelection();
				for( Object obj : selection.toArray()) {
					if( obj instanceof Column ) {
						info =  (Column) obj;
						break;
					}
				}
				if( info != null ) {
					int selectedIndex = columnsViewer.getTable().getSelectionIndex();
					 // TODO: getRelationalReference().moveColumnDown(info);
					refreshColumnsViewer();
					columnsViewer.getTable().select(selectedIndex+1);
					
//					downColumnButton.setEnabled(getRelationalReference().canMoveColumnDown(info));
//					upColumnButton.setEnabled(getRelationalReference().canMoveColumnUp(info));
					
				}
			}
    		
		});
    	
    	this.columnsViewer = new TableViewerBuilder(thePanel, (SWT.SINGLE | SWT.FULL_SELECTION | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER));  	

        // create columns
        TableViewerColumn column = this.columnsViewer.createColumn(SWT.LEFT, 30, 40, true);
        column.getColumn().setText(Messages.columnNameLabel);
        
        columnNameEditSupport = new ColumnNameEditingSupport(this.columnsViewer.getTableViewer());
        columnNameEditSupport.setViewerElementChangedListener(this);
        column.setEditingSupport(columnNameEditSupport);
        column.setLabelProvider(new ColumnDataLabelProvider(0));

        column = this.columnsViewer.createColumn(SWT.LEFT, 30, 40, true);
        column.getColumn().setText(Messages.dataTypeLabel);
        column.setLabelProvider(new ColumnDataLabelProvider(1));
        columnTypeEditSupport = new ColumnDatatypeEditingSupport(this.columnsViewer.getTableViewer());
        columnTypeEditSupport.setViewerElementChangedListener(this);
        column.setEditingSupport(columnTypeEditSupport);
        
        column = this.columnsViewer.createColumn(SWT.LEFT, 30, 40, true);
        column.getColumn().setText(Messages.lengthLabel);
        column.setLabelProvider(new ColumnDataLabelProvider(2));
        columnWidthEditSupport = new ColumnWidthEditingSupport(this.columnsViewer.getTableViewer());
        columnWidthEditSupport.setViewerElementChangedListener(this);
        column.setEditingSupport(columnWidthEditSupport);

        refreshColumnsViewer();
        
        this.columnsViewer.addSelectionChangedListener(new ISelectionChangedListener() {
			
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				IStructuredSelection sel = (IStructuredSelection)event.getSelection();
				
				if( sel.isEmpty()) {
					deleteColumnButton.setEnabled(false);
					upColumnButton.setEnabled(false);
					downColumnButton.setEnabled(false);
				} else {
					boolean enable = true;
					Object[] objs = sel.toArray();
					Column columnInfo = null;
					for( Object obj : objs) {
						if(  !(obj instanceof Column)) {
							enable = false;
							break;
						} else {
							columnInfo = (Column)obj;
						}
					} 
					if( objs.length == 0 ) {
						enable = false;
					}
					deleteColumnButton.setEnabled(enable);
					if( enable ) {
						 // TODO: 
//						upColumnButton.setEnabled(getRelationalReference().canMoveColumnUp(columnInfo));
//						downColumnButton.setEnabled(getRelationalReference().canMoveColumnDown(columnInfo));
					}
					
				}
				
			}
		});
        
        return thePanel;
    }
	
	/**
	 * 
	 * @return element that changed
	 */
	public void notifyChanged(ElementChangedEvent event) {
		columnsViewer.getTableViewer().refresh(event.getElement());
		handleInfoChanged();
	}
	
	private void refreshColumnsViewer() {
		if( isVisible() && columnsViewer != null ) {
//			columnsViewer.getTable().clearAll();
	        KomodoObject targetKO = getTargetObject();
	        
	        if( targetKO != null ) {
	        	try {
					Object[] children = targetKO.getChildren(null);
					for( Object child : children) { // row : getRelationalReference().getColumns() ) {
						if( RelationalIdentifier.isType((KomodoObject)child, CoreConstants.RelationalType.COLUMN)) {
							this.columnsViewer.add(child);
						}
					}
				} catch (KException e1) {
					e1.printStackTrace();
				}
	        }
	        this.columnsViewer.refresh();
		}
	}
	

	@Override
	public void setVisible(boolean visible) {
		// TODO Auto-generated method stub
		super.setVisible(visible);
		
		if( visible ) {
			syncWithTarget();
		}
	}
	
	
	
	@Override
	protected void syncWithTarget() {
		super.syncWithTarget();
		setSynching(true);
		
		refreshColumnsViewer();
		try {
			this.supportsUpdateCB.setSelection(getTable().isUpdatable(null));
		} catch (KException e1) {
			e1.printStackTrace();
		}
		try {
			this.materializedCB.setSelection(getTable().isMaterialized(null));
		} catch (KException e1) {
			e1.printStackTrace();
		}
		try {
			String nis = getTable().getNameInSource(null);
			if( nis == null ) nis = StringUtils.EMPTY_STRING;
			this.nameInSourceText.setText(nis);
		} catch (KException e) {
			e.printStackTrace();
		}
		
		try {
			int card = getTable().getCardinality(null);
			this.cardinalityText.setText(Integer.toString(card));
		} catch (KException e) {
			e.printStackTrace();
		}
		
		setSynching(false);
	}
	
	
	@Override
	public void clear() {
		materializedCB.setSelection(false);
		supportsUpdateCB.setSelection(false);
		cardinalityText.setText(StringUtils.EMPTY_STRING);
		materializedTableText.setText(StringUtils.EMPTY_STRING);
		nameInSourceText.setText(StringUtils.EMPTY_STRING);
		super.clear();
	}

	private Table getTable() {
		return ((Table)getTargetObject());
	}

	class ColumnDataLabelProvider extends ColumnLabelProvider {

		private final int columnNumber;

		public ColumnDataLabelProvider(int columnNumber) {
			this.columnNumber = columnNumber;
		}

		/**
		 * {@inheritDoc}
		 * 
		 * @see org.eclipse.jface.viewers.ColumnLabelProvider#getText(java.lang.Object)
		 */
		@Override
		public String getText(Object element) {
			if( element instanceof KomodoObject ) {
				switch (this.columnNumber) {
					case 0: {
						try {
							return ((KomodoObject)element).getName(null);
						} catch (KException e) {
							e.printStackTrace();
						} //"<name>"; //((Column)element).getName(null);
						
						return "<name>";
					}
					case 1: {
						return "<type>"; //((Column)element).getDatatype();
					}
					case 2: {
						return "<length>"; //Integer.toString(((Column)element).getLength());
					}
				}
			}
			return StringConstants.EMPTY_STRING;
		}

		/**
		 * {@inheritDoc}
		 * 
		 * @see org.eclipse.jface.viewers.CellLabelProvider#getToolTipText(java.lang.Object)
		 */
		@Override
		public String getToolTipText(Object element) {
			switch (this.columnNumber) {
			case 0: {
				return "Tooltip 1"; //getString("columnNameColumnTooltip"); //$NON-NLS-1$
			}
			case 1: {
				return "Tooltip 2"; //getString("datatypeColumnTooltip"); //$NON-NLS-1$
			}
		}
		return "unknown tooltip"; //$NON-NLS-1$
		}

		@Override
		public Image getImage(Object element) {
			if( this.columnNumber == 0 ) {
				return null; // UiPlugin.getDefault().getImage(UiConstants.Images.COLUMN_ICON);
			}
			return null;
		}
	}

}
