package org.komodo.eclipse.workspace.ui.editor.panels.relational;

import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.komodo.eclipse.workspace.CoreConstants;
import org.komodo.eclipse.workspace.CorePlugin;
import org.komodo.eclipse.workspace.ui.Messages;
import org.komodo.eclipse.workspace.ui.common.table.TableViewerBuilder;
import org.komodo.eclipse.workspace.ui.common.util.CoreStringUtil;
import org.komodo.eclipse.workspace.ui.common.util.UILabelUtil;
import org.komodo.eclipse.workspace.ui.common.util.UiLabelConstants;
import org.komodo.eclipse.workspace.ui.common.util.WidgetFactory;
import org.komodo.eclipse.workspace.ui.editor.panels.IKObjectChangeListener;
import org.komodo.eclipse.workspace.ui.editor.panels.ObjectEditorPanel;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.UserDefinedFunctionEditorPanel.ParameterDataLabelProvider;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ColumnDatatypeEditingSupport;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ColumnNameEditingSupport;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ColumnWidthEditingSupport;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ElementChangedEvent;
import org.komodo.eclipse.workspace.ui.editor.panels.relational.util.ViewerElementChangedListener;
import org.komodo.eclipse.workspace.util.RelationalIdentifier;
import org.komodo.relational.model.Column;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;

public class PushdownFunctionEditorPanel extends ObjectEditorPanel implements ViewerElementChangedListener {
	private TabItem generalPropertiesTab;
	private TabItem parametersTab;
	
	private Button addParameterButton, deleteParameterButton, upParameterButton, downParameterButton;
	private TableViewerBuilder parametersViewer;
	
	private ColumnWidthEditingSupport parameterWidthEditSupport;
	private ColumnNameEditingSupport parameterNameEditSupport;
	private ColumnDatatypeEditingSupport parameterTypeEditSupport;
	
	public PushdownFunctionEditorPanel(Composite parent, int style,
			int nodeType, IKObjectChangeListener docChangedListener) {
		super(parent, style, nodeType, docChangedListener);
		// TODO Auto-generated constructor stub
	}

	public PushdownFunctionEditorPanel(Composite parent, int style,
			int nodeType, IKObjectChangeListener docChangedListener,
			boolean showTree) {
		super(parent, style, nodeType, docChangedListener, showTree);
		// TODO Auto-generated constructor stub
	}

	public void addCustomPanel() {
		createParametersTab(getTabFolder());
		createGeneralPropertiesTab(getTabFolder());
	}
	
	@Override
	protected void syncWithTarget() {
		super.syncWithTarget();
		setSynching(true);

		setSynching(false);
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
        column.setLabelProvider(new ParameterDataLabelProvider(0));

        column = this.parametersViewer.createColumn(SWT.LEFT, 30, 40, true);
        column.getColumn().setText(Messages.dataTypeLabel);
        column.setLabelProvider(new ParameterDataLabelProvider(1));
        parameterTypeEditSupport = new ColumnDatatypeEditingSupport(this.parametersViewer.getTableViewer());
        parameterTypeEditSupport.setViewerElementChangedListener(this);
        column.setEditingSupport(parameterTypeEditSupport);
        
        column = this.parametersViewer.createColumn(SWT.LEFT, 30, 40, true);
        column.getColumn().setText(Messages.lengthLabel);
        column.setLabelProvider(new ParameterDataLabelProvider(2));
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
	
	
	private void refreshParametersViewer() {
		if( isVisible() && parametersViewer != null ) {
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
	
	class ParameterDataLabelProvider extends ColumnLabelProvider {

		private final int columnNumber;

		public ParameterDataLabelProvider(int columnNumber) {
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
				return CorePlugin.getDefault().getImage(CoreConstants.Images.PARAMETER_ICON);
			}
			return null;
		}
	}
}
