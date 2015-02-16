package org.komodo.eclipse.workspace.ui.editor.panels.relational;

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
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;
import org.komodo.eclipse.sql.ui.editor.panel.SqlEditorPanel;
import org.komodo.eclipse.workspace.CoreConstants;
import org.komodo.eclipse.workspace.CorePlugin;
import org.komodo.eclipse.workspace.ui.Messages;
import org.komodo.eclipse.workspace.ui.UiConstants;
import org.komodo.eclipse.workspace.ui.common.table.TableViewerBuilder;
import org.komodo.eclipse.workspace.ui.common.util.CoreStringUtil;
import org.komodo.eclipse.workspace.ui.common.util.ObjectEditorUtils;
import org.komodo.eclipse.workspace.ui.common.util.StyledTextEditor;
import org.komodo.eclipse.workspace.ui.common.util.UILabelUtil;
import org.komodo.eclipse.workspace.ui.common.util.UiLabelConstants;
import org.komodo.eclipse.workspace.ui.common.util.WidgetFactory;
import org.komodo.eclipse.workspace.ui.editor.panels.IKObjectChangeListener;
import org.komodo.eclipse.workspace.ui.editor.panels.ObjectEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ColumnDatatypeEditingSupport;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ColumnNameEditingSupport;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ColumnWidthEditingSupport;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ElementChangedEvent;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ViewerElementChangedListener;
import org.komodo.eclipse.workspace.ui.explorer.WorkspaceTreeViewer;
import org.komodo.eclipse.workspace.util.RelationalIdentifier;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.View;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.utils.StringUtils;

public class ViewEditorPanel extends ObjectEditorPanel implements ViewerElementChangedListener {

	private TabItem generalPropertiesTab;
	private TabItem columnsTab;
	private TabItem sqlTab;
    private SqlEditorPanel sqlEditorPanel;
    
	
	// table property widgets
	private Button supportsUpdateCB;
	private Text cardinalityText;
	
	private TableViewerBuilder columnsViewer;
	private Button addColumnButton, deleteColumnButton, upColumnButton, downColumnButton;
	
	private ColumnWidthEditingSupport columnWidthEditSupport;
	private ColumnNameEditingSupport columnNameEditSupport;
	private ColumnDatatypeEditingSupport columnTypeEditSupport;
	
	
	private WorkspaceTreeViewer sourceTreeViewer;
	
	public ViewEditorPanel(Composite parent, int style, int nodeType,
			IKObjectChangeListener objectChangeListener) {
		super(parent, style, nodeType, objectChangeListener, true);
	}
	
	public void addCustomPanel() {
//		createColumnsTab(getTabFolder());
		createSQLTab(getTabFolder());
		createGeneralPropertiesTab(getTabFolder());
	}

	private View getView() {
		return ((View)getTargetObject());
	}
	
	@Override
	protected void syncWithTarget() {
		super.syncWithTarget();
		setSynching(true);
		
		refreshColumnsViewer();
		// TODO: this.supportsUpdateCB.setSelection(getTable().getUpdatable());
		// TODO: this.materializedCB.setSelection(getTable().getMaterialized());
		
		try {
			int card = getView().getCardinality(null);
			this.cardinalityText.setText(Integer.toString(card));
			String sql = getView().getQueryExpression(null);
			if( sql == null ) sql = StringUtils.EMPTY_STRING;
			sqlEditorPanel.setText(sql);
		} catch (KException e) {
			e.printStackTrace();
		}
		
		setSynching(false);
	}
	
	
	@Override
	public void clear() {
		supportsUpdateCB.setSelection(false);
		cardinalityText.setText(StringUtils.EMPTY_STRING);
		sqlEditorPanel.clear();
		super.clear();
	}
	
    /*
     * Create the SQL Tab
     */
    private void createSQLTab( TabFolder folderParent ) {
    	Composite wrapperPanel = WidgetFactory.createPanel(folderParent, SWT.NONE);
        GridLayoutFactory.swtDefaults().numColumns(1).equalWidth(false).applyTo(wrapperPanel);
        GridDataFactory.swtDefaults().grab(true, true).applyTo(wrapperPanel);
        
        Composite buttonPanel = new Composite(wrapperPanel, SWT.NONE);
        GridLayoutFactory.swtDefaults().margins(1,  1).numColumns(4).equalWidth(false).applyTo(buttonPanel);
        GridDataFactory.swtDefaults().span(1, 1).applyTo(buttonPanel);
        
        Button applyButton = new Button(buttonPanel, SWT.PUSH);
        applyButton.setText("Apply");
        applyButton.addSelectionListener(new SelectionListener() {
			
			@Override
			public void widgetSelected(SelectionEvent e) {
				String sql = sqlEditorPanel.getText();
				if( sql != null && sql.isEmpty() ) {
					sql = StringUtils.EMPTY_STRING;
				}
				try {
					getView().setQueryExpression(null, sql);
				} catch (KException e1) {
					e1.printStackTrace();
				}
			}
			
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});
        
        sqlEditorPanel = new SqlEditorPanel(wrapperPanel);
        
//        createSqlSourcesTableGroup(wrapperPanel);

        this.sqlTab = new TabItem(folderParent, SWT.NONE);
        this.sqlTab.setControl(wrapperPanel);
        this.sqlTab.setText(Messages.sqlLabel);
//        this.sqlTab.setImage(RelationalUiUtil.getNativeSQLImage(Status.OK_STATUS));
        
        
    }

	private void createGeneralPropertiesTab(TabFolder folderParent) {
        // build the SELECT tab
		Composite thePanel = createPropertiesPanel(folderParent);

        this.generalPropertiesTab = new TabItem(folderParent, SWT.NONE);
        this.generalPropertiesTab.setControl(thePanel);
        this.generalPropertiesTab.setText(Messages.propertiesLabel);
//        this.generalPropertiesTab.setImage(RelationalUiUtil.getRelationalImage(TYPES.TABLE, ModelType.PHYSICAL, Status.OK_STATUS));
	}
	
	private Composite createPropertiesPanel(Composite parent) {
		Composite thePanel = WidgetFactory.createPanel(parent, SWT.NONE, 1, 3);
		GridLayoutFactory.fillDefaults().margins(5, 5).applyTo(thePanel);
		GridDataFactory.fillDefaults().grab(true, true).applyTo(thePanel);

		Composite cardinalityPanel = new Composite(thePanel, SWT.NONE);
        GridDataFactory.fillDefaults().grab(true, false).applyTo(cardinalityPanel);
        GridLayoutFactory.fillDefaults().numColumns(2).equalWidth(false).applyTo(cardinalityPanel);

        Label label = new Label(cardinalityPanel, SWT.NONE);
        label.setText("Cardinality");
        
        this.cardinalityText =  new Text(cardinalityPanel, SWT.BORDER | SWT.SINGLE);
        this.cardinalityText.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_BLUE));
        GridDataFactory.fillDefaults().grab(true, false).applyTo(this.cardinalityText);
        this.cardinalityText.addModifyListener(new ModifyListener() {
    		@Override
			public void modifyText( final ModifyEvent event ) {
//    			if( isSynchronizing() )
//    			    return;

    			if( !cardinalityText.getText().isEmpty()) {
    			    try {	
    			        int value = Integer.parseInt(cardinalityText.getText());
    			        getView().setCardinality(null, value);
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
            	// TODO:  getRelationalReference().setSupportsUpdate(supportsUpdateCB.getSelection());
                handleInfoChanged();
            }
        });

        return thePanel;
	}
	
	
	private void createColumnsTab(TabFolder folderParent) {
        Composite thePanel = createColumnTableGroup(folderParent);

        this.columnsTab = new TabItem(folderParent, SWT.NONE);
        this.columnsTab.setControl(thePanel);
        this.columnsTab.setText(Messages.columnsLabel);
        this.columnsTab.setImage(RelationalIdentifier.getImage(CoreConstants.RelationalType.COLUMN));
	}
	
	private void createSqlSourcesTableGroup( Composite parent ) {
	    Composite thePanel = WidgetFactory.createPanel(parent, SWT.NONE, 1, 1);
        GridLayoutFactory.fillDefaults().margins(5, 5).applyTo(thePanel);
        GridDataFactory.fillDefaults().grab(true, true).applyTo(thePanel);

        Composite buttonPanel = WidgetFactory.createPanel(thePanel, SWT.NONE, 1, 4);
        GridLayoutFactory.fillDefaults().numColumns(4).applyTo(buttonPanel);
        GridDataFactory.fillDefaults().grab(true, false).applyTo(buttonPanel);

    	Button addSourceButton = new Button(buttonPanel, SWT.PUSH);
    	addSourceButton.setText(UILabelUtil.getLabel(UiLabelConstants.LABEL_IDS.ADD));
    	GridDataFactory.fillDefaults().applyTo(addSourceButton);
    	addSourceButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				// TODL: handle add source
			}
		});
    	
    	Button removeSourceButton = new Button(buttonPanel, SWT.PUSH);
    	removeSourceButton.setText("Remove");
    	GridDataFactory.fillDefaults().applyTo(removeSourceButton);
    	removeSourceButton.setEnabled(false);
    	removeSourceButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				// TODO: handle remove source
			}
    		
		});
    	
    	this.sourceTreeViewer = new WorkspaceTreeViewer(thePanel, SWT.BORDER | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
    	GridDataFactory.fillDefaults().grab(false, true).applyTo(this.sourceTreeViewer.getControl());
    	
    	this.sourceTreeViewer.setInput(null);
    	
    	// TODO: get source and view models in this VDB 
    	// Will display a tree of available source tables for the SQL
    	
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
				 // TODO: getRelationalReference().createColumn();
				handleInfoChanged();
			}
    		
		});
    	
    	deleteColumnButton = new Button(buttonPanel, SWT.PUSH);
    	deleteColumnButton.setText(UILabelUtil.getLabel(UiLabelConstants.LABEL_IDS.DELETE));
    	GridDataFactory.fillDefaults().applyTo(deleteColumnButton);
    	deleteColumnButton.setEnabled(false);
    	deleteColumnButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				Column column = null;
				
				IStructuredSelection selection = (IStructuredSelection)columnsViewer.getSelection();
				for( Object obj : selection.toArray()) {
					if( obj instanceof Column ) {
						column =  (Column) obj;
						break;
					}
				}
				if( column != null ) {
					 // TODO: getRelationalReference().removeColumn(column);
					deleteColumnButton.setEnabled(false);
					handleInfoChanged();
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
					handleInfoChanged();
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
					handleInfoChanged();
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

//        if( getRelationalReference() != null ) {
//	        for( Column row : getRelationalReference().getColumns() ) {
//	        	this.columnsViewer.add(row);
//	        }
//        }
        
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
			if( element instanceof Column ) {
				switch (this.columnNumber) {
					case 0: {
						return "<name>"; //((Column)element).getName(null);
					}
					case 1: {
						return "<type>"; //((Column)element).getDatatype();
					}
					case 2: {
						return "<length>"; //Integer.toString(((Column)element).getLength());
					}
				}
			}
			return CoreStringUtil.Constants.EMPTY_STRING;
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
