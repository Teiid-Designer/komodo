package org.komodo.eclipse.workspace.ui.editor.panels.relational;

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
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;
import org.komodo.eclipse.sql.ui.editor.panel.SqlEditorPanel;
import org.komodo.eclipse.workspace.CoreConstants;
import org.komodo.eclipse.workspace.CorePlugin;
import org.komodo.eclipse.workspace.ui.Messages;
import org.komodo.eclipse.workspace.ui.common.table.TableViewerBuilder;
import org.komodo.eclipse.workspace.ui.common.util.UILabelUtil;
import org.komodo.eclipse.workspace.ui.common.util.UiLabelConstants;
import org.komodo.eclipse.workspace.ui.common.util.WidgetFactory;
import org.komodo.eclipse.workspace.ui.editor.panels.KObjectChangeListener;
import org.komodo.eclipse.workspace.ui.editor.panels.ObjectEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ColumnDatatypeEditingSupport;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ColumnNameEditingSupport;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ColumnWidthEditingSupport;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ElementChangedEvent;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ViewerElementChangedListener;
import org.komodo.eclipse.workspace.util.RelationalIdentifier;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.VirtualProcedure;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.utils.StringUtils;

public class VirtualProcedureEditorPanel   extends ObjectEditorPanel implements ViewerElementChangedListener {

	private TabItem generalPropertiesTab;
	private TabItem parametersTab;
	private TabItem sqlTab;
	private TabItem resultSetTab;
    private SqlEditorPanel sqlEditorPanel;

	
	private Button includeResultSetCB;
	private Text resultSetNameText;
	private Button addParameterButton, deleteParameterButton, upParameterButton, downParameterButton;

	private TableViewerBuilder parametersViewer;
	private TableViewerBuilder columnsViewer;
	private Button addColumnButton, deleteColumnButton, upColumnButton, downColumnButton;
	
	private ColumnWidthEditingSupport columnWidthEditSupport;
	private ColumnNameEditingSupport columnNameEditSupport;
	private ColumnDatatypeEditingSupport columnTypeEditSupport;
	
	private ColumnWidthEditingSupport parameterWidthEditSupport;
	private ColumnNameEditingSupport parameterNameEditSupport;
	private ColumnDatatypeEditingSupport parameterTypeEditSupport;
	
	public VirtualProcedureEditorPanel(Composite parent, int style, int nodeType, KObjectChangeListener objectChangeListener) {
		super(parent, style, nodeType, objectChangeListener, true);
	}
	
	public void addCustomPanel() {
		createParametersTab(getTabFolder());
		createResultSetTab(getTabFolder());
		createSQLTab(getTabFolder());
		createGeneralPropertiesTab(getTabFolder());
	}

	private VirtualProcedure getVirtualProcedure() {
		return ((VirtualProcedure)getTargetObject());
	}
	
	@Override
	protected void syncWithTarget() {
		super.syncWithTarget();
		setSynching(true);
		
//		refreshColumnsViewer();
//		 TODO: this.supportsUpdateCB.setSelection(getTable().getUpdatable());
		// TODO: this.materializedCB.setSelection(getTable().getMaterialized());
		
//		try {
//			String sql = getVirtualProcedure().getQueryExpression(null);
//			if( sql == null ) sql = StringUtils.EMPTY_STRING;
//			sqlEditorPanel.setText(sql);
//		} catch (KException e) {
//			e.printStackTrace();
//		}
		
		setSynching(false);
	}
	
	
	@Override
	public void clear() {
		includeResultSetCB.setSelection(false);
		resultSetNameText.setText(StringUtils.EMPTY_STRING);
		sqlEditorPanel.clear();
		super.clear();
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


        return thePanel;
	}

	private void createParametersTab(TabFolder folderParent) {
        Composite thePanel = createParametersTableGroup(folderParent);

        this.parametersTab = new TabItem(folderParent, SWT.NONE);
        this.parametersTab.setControl(thePanel);
        this.parametersTab.setText(Messages.parametersLabel);
        this.parametersTab.setImage(RelationalIdentifier.getImage(CoreConstants.RelationalType.PARAMETER));
	}
	
	
	private void createResultSetTab(TabFolder folderParent) {
        Composite thePanel = createResultSetPanel(folderParent);

        this.resultSetTab = new TabItem(folderParent, SWT.NONE);
        this.resultSetTab.setControl(thePanel);
        this.resultSetTab.setText(Messages.resultSetLabel);
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
// TODO:				try {
//					getProcedure().setQueryExpression(null, sql);
//				} catch (KException e1) {
//					e1.printStackTrace();
//				}
			}
			
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
			}
		});
        sqlEditorPanel = new SqlEditorPanel(wrapperPanel);

        this.sqlTab = new TabItem(folderParent, SWT.NONE);
        this.sqlTab.setControl(wrapperPanel);
        this.sqlTab.setText(Messages.sqlLabel);
//        this.sqlTab.setImage(RelationalUiUtil.getNativeSQLImage(Status.OK_STATUS));
    }
    
    
	
	private Composite createParametersTableGroup(Composite parent) {
	  	
	    Composite thePanel = WidgetFactory.createPanel(parent, SWT.NONE, 1, 1);
        GridLayoutFactory.fillDefaults().margins(5, 5).applyTo(thePanel);
        GridDataFactory.fillDefaults().grab(true, true).applyTo(thePanel);

        Composite buttonPanel = WidgetFactory.createPanel(thePanel, SWT.NONE, 1, 4);
        GridLayoutFactory.fillDefaults().numColumns(4).applyTo(buttonPanel);
        GridDataFactory.fillDefaults().grab(true, false).applyTo(buttonPanel);

    	addParameterButton = new Button(buttonPanel, SWT.PUSH);
    	addParameterButton.setText(UILabelUtil.getLabel(UiLabelConstants.LABEL_IDS.ADD));
    	GridDataFactory.fillDefaults().applyTo(addParameterButton);
    	addParameterButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				 // TODO: getRelationalReference().createColumn();
				handleInfoChanged();
			}
    		
		});
    	
    	deleteParameterButton = new Button(buttonPanel, SWT.PUSH);
    	deleteParameterButton.setText(UILabelUtil.getLabel(UiLabelConstants.LABEL_IDS.DELETE));
    	GridDataFactory.fillDefaults().applyTo(deleteParameterButton);
    	deleteParameterButton.setEnabled(false);
    	deleteParameterButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				Column column = null;
				
				IStructuredSelection selection = (IStructuredSelection)parametersViewer.getSelection();
				for( Object obj : selection.toArray()) {
					if( obj instanceof Column ) {
						column =  (Column) obj;
						break;
					}
				}
				if( column != null ) {
					 // TODO: getRelationalReference().removeColumn(column);
					deleteParameterButton.setEnabled(false);
					handleInfoChanged();
				}
			}
    		
		});
    	
    	upParameterButton = new Button(buttonPanel, SWT.PUSH);
    	upParameterButton.setText(UILabelUtil.getLabel(UiLabelConstants.LABEL_IDS.MOVE_UP));
    	GridDataFactory.fillDefaults().applyTo(upParameterButton);
    	upParameterButton.setEnabled(false);
    	upParameterButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				Column info = null;
				
				IStructuredSelection selection = (IStructuredSelection)parametersViewer.getSelection();
				for( Object obj : selection.toArray()) {
					if( obj instanceof Column ) {
						info =  (Column) obj;
						break;
					}
				}
				if( info != null ) {
					int selectedIndex = parametersViewer.getTable().getSelectionIndex();
					 // TODO: getRelationalReference().moveColumnUp(info);
					handleInfoChanged();
					parametersViewer.getTable().select(selectedIndex-1);
//					downParameterButton.setEnabled(getRelationalReference().canMoveColumnDown(info));
//					upParameterButton.setEnabled(getRelationalReference().canMoveColumnUp(info));
					
				}
			}
    		
		});
    	
    	downParameterButton = new Button(buttonPanel, SWT.PUSH);
    	downParameterButton.setText(UILabelUtil.getLabel(UiLabelConstants.LABEL_IDS.MOVE_DOWN));
    	GridDataFactory.fillDefaults().applyTo(downParameterButton);
    	downParameterButton.setEnabled(false);
    	downParameterButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				Column info = null;
				
				IStructuredSelection selection = (IStructuredSelection)parametersViewer.getSelection();
				for( Object obj : selection.toArray()) {
					if( obj instanceof Column ) {
						info =  (Column) obj;
						break;
					}
				}
				if( info != null ) {
					int selectedIndex = parametersViewer.getTable().getSelectionIndex();
					 // TODO: getRelationalReference().moveColumnDown(info);
					handleInfoChanged();
					parametersViewer.getTable().select(selectedIndex+1);
//					downParameterButton.setEnabled(getRelationalReference().canMoveColumnDown(info));
//					upParameterButton.setEnabled(getRelationalReference().canMoveColumnUp(info));
					
				}
			}
    		
		});
    	
    	this.parametersViewer = new TableViewerBuilder(thePanel, (SWT.SINGLE | SWT.FULL_SELECTION | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER));  	

        // create columns
        TableViewerColumn column = this.parametersViewer.createColumn(SWT.LEFT, 30, 40, true);
        column.getColumn().setText(Messages.columnNameLabel);
        
        parameterNameEditSupport = new ColumnNameEditingSupport(this.parametersViewer.getTableViewer());
        parameterNameEditSupport.setViewerElementChangedListener(this);
        column.setEditingSupport(parameterNameEditSupport);
        column.setLabelProvider(new ColumnDataLabelProvider(0));

        column = this.parametersViewer.createColumn(SWT.LEFT, 30, 40, true);
        column.getColumn().setText(Messages.dataTypeLabel);
        column.setLabelProvider(new ColumnDataLabelProvider(1));
        parameterTypeEditSupport = new ColumnDatatypeEditingSupport(this.parametersViewer.getTableViewer());
        parameterTypeEditSupport.setViewerElementChangedListener(this);
        column.setEditingSupport(parameterTypeEditSupport);
        
        column = this.parametersViewer.createColumn(SWT.LEFT, 30, 40, true);
        column.getColumn().setText(Messages.lengthLabel);
        column.setLabelProvider(new ColumnDataLabelProvider(2));
        parameterWidthEditSupport = new ColumnWidthEditingSupport(this.parametersViewer.getTableViewer());
        parameterWidthEditSupport.setViewerElementChangedListener(this);
        column.setEditingSupport(parameterWidthEditSupport);

//        if( getRelationalReference() != null ) {
//	        for( Column row : getRelationalReference().getColumns() ) {
//	        	this.columnsViewer.add(row);
//	        }
//        }
        
        this.parametersViewer.addSelectionChangedListener(new ISelectionChangedListener() {
			
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				IStructuredSelection sel = (IStructuredSelection)event.getSelection();
				
				if( sel.isEmpty()) {
					deleteParameterButton.setEnabled(false);
					upParameterButton.setEnabled(false);
					downParameterButton.setEnabled(false);
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
					deleteParameterButton.setEnabled(enable);
					if( enable ) {
						 // TODO: 
//						upParameterButton.setEnabled(getRelationalReference().canMoveColumnUp(columnInfo));
//						downParameterButton.setEnabled(getRelationalReference().canMoveColumnDown(columnInfo));
					}
					
				}
				
			}
		});
        
        return thePanel;
    }
	
	private Composite createResultSetPanel(Composite parent) {
	    Composite thePanel = WidgetFactory.createPanel(parent, SWT.NONE, 1, 2);
        GridLayoutFactory.fillDefaults().margins(10, 10).applyTo(thePanel);
        GridDataFactory.fillDefaults().grab(true, true).applyTo(thePanel);
    	
        this.includeResultSetCB = new Button(thePanel, SWT.CHECK | SWT.RIGHT);
        GridDataFactory.fillDefaults().align(SWT.LEFT, SWT.CENTER).applyTo(this.includeResultSetCB);
        this.includeResultSetCB.setText(UILabelUtil.getLabel(UiLabelConstants.LABEL_IDS.INCLUDE));
        // TODO: this.includeResultSetCB.setToolTipText(Messages.includeResultSetTooltip);
        // TODO: this.includeResultSetCB.setSelection(getRelationalReference().getResultSet() != null);
        this.includeResultSetCB.addSelectionListener(new SelectionAdapter() {
            /**            		
             * {@inheritDoc}
             * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected( SelectionEvent e ) {
            	// TODO:
//            	if( includeResultSetCB.getSelection() ) {
//            		if( getRelationalReference().getResultSet() == null ) {
//            			RelationalProcedureResultSet resultSet = new RelationalProcedureResultSet();
//            			
//            			if( resultSetNameText.getText() != null ) {
//            				resultSet.setName(resultSetNameText.getText());
//            			}
//            			getRelationalReference().setResultSet(resultSet);
//            		}
//            	} else {
//            		getRelationalReference().setResultSet(null);
//            	}
                handleInfoChanged();
            }
        });
        
        Composite namePanel = WidgetFactory.createPanel(thePanel, SWT.NONE, 2, 2);
        GridLayoutFactory.fillDefaults().numColumns(2).applyTo(namePanel);
        GridDataFactory.fillDefaults().grab(true, false).applyTo(namePanel);

        Label label = new Label(namePanel, SWT.NONE | SWT.RIGHT);
        label.setText(UILabelUtil.getLabel(UiLabelConstants.LABEL_IDS.NAME));
        GridDataFactory.fillDefaults().applyTo(label);
        
        this.resultSetNameText =  new Text(namePanel, SWT.BORDER | SWT.SINGLE);
        this.resultSetNameText.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_BLUE));
        GridDataFactory.fillDefaults().grab(true, false).minSize(50, SWT.DEFAULT).applyTo(this.resultSetNameText);
        this.resultSetNameText.addModifyListener(new ModifyListener() {
    		@Override
			public void modifyText( final ModifyEvent event ) {
    			String value = resultSetNameText.getText();
    			if( value == null ) {
    				value = StringConstants.EMPTY_STRING;
    			}
    			// TODO:
//        		if( getRelationalReference().getResultSet() != null ) {
//        			RelationalProcedureResultSet resultSet = getRelationalReference().getResultSet();
//        			resultSet.setName(value);
//        		}
        		handleInfoChanged();
    		}
        });

        Composite buttonPanel = WidgetFactory.createPanel(thePanel, SWT.NONE, 1, 4);
        GridLayoutFactory.fillDefaults().numColumns(4).applyTo(buttonPanel);
        GridDataFactory.fillDefaults().span(2, 1).applyTo(buttonPanel);
	  	
    	addColumnButton = new Button(buttonPanel, SWT.PUSH);
    	addColumnButton.setText(UILabelUtil.getLabel(UiLabelConstants.LABEL_IDS.ADD));
    	GridDataFactory.fillDefaults().applyTo(addColumnButton);
    	addColumnButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
	    		// TODO: getRelationalReference().getResultSet().createColumn();
				handleInfoChanged();
			}
    		
		});
    	this.addColumnButton.setEnabled(false);
    	
    	deleteColumnButton = new Button(buttonPanel, SWT.PUSH);
    	deleteColumnButton.setText(UILabelUtil.getLabel(UiLabelConstants.LABEL_IDS.DELETE));
    	GridDataFactory.fillDefaults().applyTo(deleteColumnButton);
    	deleteColumnButton.setEnabled(false);
    	deleteColumnButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				// TODO:
//				RelationalColumn column = null;
//				
//				IStructuredSelection selection = (IStructuredSelection)columnsViewer.getSelection();
//				for( Object obj : selection.toArray()) {
//					if( obj instanceof RelationalColumn ) {
//						column =  (RelationalColumn) obj;
//						break;
//					}
//				}
//				if( column != null ) {
//					getRelationalReference().getResultSet().removeColumn(column);
//					deleteColumnButton.setEnabled(false);
//					handleInfoChanged();
//				}
			}
    		
		});
    	
    	upColumnButton = new Button(buttonPanel, SWT.PUSH);
    	upColumnButton.setText(UILabelUtil.getLabel(UiLabelConstants.LABEL_IDS.MOVE_UP));
    	GridDataFactory.fillDefaults().applyTo(upColumnButton);
    	upColumnButton.setEnabled(false);
    	upColumnButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				// TODO:
//				RelationalColumn info = null;
//				
//				IStructuredSelection selection = (IStructuredSelection)columnsViewer.getSelection();
//				for( Object obj : selection.toArray()) {
//					if( obj instanceof RelationalColumn ) {
//						info =  (RelationalColumn) obj;
//						break;
//					}
//				}
//				if( info != null ) {
//					int selectedIndex = columnsViewer.getTable().getSelectionIndex();
//					getRelationalReference().getResultSet().moveColumnUp(info);
//					handleInfoChanged();
//					columnsViewer.getTable().select(selectedIndex-1);
//					downColumnButton.setEnabled(getRelationalReference().getResultSet().canMoveColumnDown(info));
//					upColumnButton.setEnabled(getRelationalReference().getResultSet().canMoveColumnUp(info));
//					
//				}
			}
    		
		});
    	
    	downColumnButton = new Button(buttonPanel, SWT.PUSH);
    	downColumnButton.setText(UILabelUtil.getLabel(UiLabelConstants.LABEL_IDS.MOVE_DOWN));
    	GridDataFactory.fillDefaults().applyTo(downColumnButton);
    	downColumnButton.setEnabled(false);
    	downColumnButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				// TODO:
//				RelationalColumn info = null;
//				
//				IStructuredSelection selection = (IStructuredSelection)columnsViewer.getSelection();
//				for( Object obj : selection.toArray()) {
//					if( obj instanceof RelationalColumn ) {
//						info =  (RelationalColumn) obj;
//						break;
//					}
//				}
//				if( info != null ) {
//					int selectedIndex = columnsViewer.getTable().getSelectionIndex();
//					getRelationalReference().getResultSet().moveColumnDown(info);
//					handleInfoChanged();
//					columnsViewer.getTable().select(selectedIndex+1);
//					downColumnButton.setEnabled(getRelationalReference().getResultSet().canMoveColumnDown(info));
//					upColumnButton.setEnabled(getRelationalReference().getResultSet().canMoveColumnUp(info));
//					
//				}
			}
    		
		});
    	
    	this.columnsViewer = new TableViewerBuilder(thePanel, (SWT.SINGLE | SWT.FULL_SELECTION | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER));  

        TableViewerColumn column = this.columnsViewer.createColumn(SWT.LEFT, 30, 40, true);
        column.getColumn().setText("Result Set Columns"); //Messages.columnNameLabel);
        
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

        
    	// TODO:
//        if( this.getRelationalReference().getResultSet() != null ) {
//	        for( RelationalColumn row : this.getRelationalReference().getResultSet().getColumns() ) {
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
					// TODO:
//					RelationalColumn columnInfo = null;
//					for( Object obj : objs) {
//						if(  !(obj instanceof RelationalColumn)) {
//							enable = false;
//							break;
//						} else {
//							columnInfo = (RelationalColumn)obj;
//						}
//					} 
//					if( objs.length == 0 ) {
//						enable = false;
//					}
//					deleteColumnButton.setEnabled(enable && !getRelationalReference().isNativeQueryProcedure());
//					if( enable ) {
//						upColumnButton.setEnabled(getRelationalReference().getResultSet().canMoveColumnUp(columnInfo));
//						downColumnButton.setEnabled(getRelationalReference().getResultSet().canMoveColumnDown(columnInfo));
//					}
					
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
		Object element = event.getElement();
		// TODO:
		// IF ELEMENT a Parameter, refresh parametersViewer
		// parametersViewer.getTableViewer().refresh(event.getElement());
		// IF ELEMENT a Column, refresh result set columnsViewer
		// columnsViewer.getTableViewer().refresh(event.getElement());
		
		handleInfoChanged();
	}
	
	@Override
	public void setVisible(boolean visible) {
		// TODO Auto-generated method stub
		super.setVisible(visible);
		
		// TODO for Result SET columns??
//		if( columnsViewer != null ) {
//			columnsViewer.setInput(getTargetObject());
//		}
		if( this.parametersViewer != null ) {
			refreshParametersViewer();
		}
	}
	
	
	private void refreshParametersViewer() {
		if( isVisible() && columnsViewer != null ) {
//			columnsViewer.getTable().clearAll();
	        KomodoObject targetKO = getTargetObject();
	        
	        if( targetKO != null ) {
	        	try {
					Object[] children = targetKO.getChildren(null);
					for( Object child : children) {
						if( RelationalIdentifier.isType((KomodoObject)child, CoreConstants.RelationalType.PARAMETER)) {
							this.parametersViewer.add(child);
						}
					}
				} catch (KException e1) {
					e1.printStackTrace();
				}
	        }
	        this.parametersViewer.refresh();
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
				return CorePlugin.getDefault().getImage(CoreConstants.Images.COLUMN_ICON);
			}
			return null;
		}
	}

}

