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
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.komodo.eclipse.workspace.CoreConstants;
import org.komodo.eclipse.workspace.CorePlugin;
import org.komodo.eclipse.workspace.ui.Messages;
import org.komodo.eclipse.workspace.ui.actions.NewColumnAction;
import org.komodo.eclipse.workspace.ui.common.table.TableViewerBuilder;
import org.komodo.eclipse.workspace.ui.common.util.UILabelUtil;
import org.komodo.eclipse.workspace.ui.common.util.UiLabelConstants;
import org.komodo.eclipse.workspace.ui.common.util.WidgetFactory;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ColumnDatatypeEditingSupport;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ColumnNameEditingSupport;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ColumnWidthEditingSupport;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ElementChangedEvent;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ViewerElementChangedListener;
import org.komodo.relational.model.ProcedureResultSet;
import org.komodo.relational.model.ResultSetColumn;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;

public class ResultSetEditorPanel extends Composite implements ViewerElementChangedListener{
	private static final String EMPTY_STRING = StringConstants.EMPTY_STRING;
	
	private ProcedureResultSet resultSet;
	private boolean nameEditable;
	private Button includeResultSetCB;
	private Text resultSetNameText;
	
	private TableViewerBuilder columnsViewer;
	private Button addColumnButton, deleteColumnButton, upColumnButton, downColumnButton;
	
	private ColumnWidthEditingSupport columnWidthEditSupport;
	private ColumnNameEditingSupport columnNameEditSupport;
	private ColumnDatatypeEditingSupport columnTypeEditSupport;
	
	public ResultSetEditorPanel(Composite parent, int style, boolean setNameEditable) {
		super(parent, style);
		this.nameEditable = setNameEditable;
		
        GridLayoutFactory.fillDefaults().margins(10, 10).applyTo(this);
        GridDataFactory.fillDefaults().grab(true, true).applyTo(this);
        
        buildPanel();
	}
	
	public ProcedureResultSet getResultSet() {
		return this.resultSet;
	}
	
	public final void syncWithTarget() {

		if( getResultSet() != null ) {
	        try {
				for( ResultSetColumn row : ((TabularResultSet)this.getResultSet()).getColumns(null) ) {
					this.columnsViewer.add(row);
				}
			} catch (KException e) {
				e.printStackTrace();
			}
	        
	        this.addColumnButton.setEnabled(true);
        } else {
        	this.addColumnButton.setEnabled(false);
        }
	}
	
	public void setResultSet(ProcedureResultSet resultSet) {
		this.resultSet = resultSet;
	}
	
	private void buildPanel() {
		
		this.includeResultSetCB = new Button(this, SWT.CHECK | SWT.RIGHT);
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
//                handleInfoChanged();
            }
        });
        
        Composite namePanel = WidgetFactory.createPanel(this, SWT.NONE, 2, 2);
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
//        		handleInfoChanged();
    		}
        });
        this.resultSetNameText.setEditable(this.nameEditable);

        Composite buttonPanel = WidgetFactory.createPanel(this, SWT.NONE, 1, 4);
        GridLayoutFactory.fillDefaults().numColumns(4).applyTo(buttonPanel);
        GridDataFactory.fillDefaults().span(2, 1).applyTo(buttonPanel);
	  	
    	addColumnButton = new Button(buttonPanel, SWT.PUSH);
    	addColumnButton.setText(UILabelUtil.getLabel(UiLabelConstants.LABEL_IDS.ADD));
    	GridDataFactory.fillDefaults().applyTo(addColumnButton);
    	addColumnButton.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
	    		NewColumnAction action = new NewColumnAction();
	    		action.setTarget(getResultSet());
	    		action.run();
	    		columnsViewer.refresh();
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
    	
    	this.columnsViewer = new TableViewerBuilder(this, (SWT.SINGLE | SWT.FULL_SELECTION | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER));  

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
					ResultSetColumn columnInfo = null;
					for( Object obj : objs) {
						if(  !(obj instanceof ResultSetColumn)) {
							enable = false;
							break;
						} else {
							columnInfo = (ResultSetColumn)obj;
						}
					} 
					if( objs.length == 0 ) {
						enable = false;
					}
					deleteColumnButton.setEnabled(enable);
					if( enable ) {
						// TODO:
//						upColumnButton.setEnabled(getRelationalReference().getResultSet().canMoveColumnUp(columnInfo));
//						downColumnButton.setEnabled(getRelationalReference().getResultSet().canMoveColumnDown(columnInfo));
					}
					
				}
				
			}
		});
	}

	@Override
	public void notifyChanged(ElementChangedEvent event) {
		// TODO Auto-generated method stub
		
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
			if( element instanceof ResultSetColumn ) {
				try {
					switch (this.columnNumber) {
						case 0: {
							return ((ResultSetColumn)element).getName(null);
						}
						case 1: {
							return ((ResultSetColumn)element).getDatatypeName(null);
						}
						case 2: {
							return Long.toString(((ResultSetColumn)element).getLength(null));
						}
					}
				} catch (KException e) {
					e.printStackTrace();
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
